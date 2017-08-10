module Main where
import           Control.Monad
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString.Lazy as Bs
import qualified Data.ByteString as B
import           Data.Int
import           Data.List
import           Data.List.Split
import qualified Data.Map.Lazy        as M
import           Data.Maybe
import           Data.Word            (Word16, Word8)
import           Numeric              (showHex)
import           Table                (decodeTable, paramCodes, newLineCodes)
import           Text.Printf
import           Data.Ord
import           Data.ByteString.Lazy.Search (strictify, breakOn)
import           System.Environment
import           Data.Char (ord)
import qualified     System.IO.UTF8    as U

data LzEntry = Liter Word8 | Refer {len :: Int, dist :: Int}

instance Show LzEntry where
  show (Liter b) = " Liter 0x"++ showHex b ""
  show (Refer l d)  =  " Refer {len = 0x" ++ showHex l "" ++ " dist = 0x" ++ showHex d "" ++ "}"



type Script = [Inst]
type Inst = [LzBlock]
data LzBlock = LzBlock {lastMessageIndex :: Word8, messages :: [Message]}
instance Show LzBlock where
  show (LzBlock lastMessageIdx  ms) = " lastMessageIndex=0x" ++ showHex lastMessageIdx "" ++"; "++ show (map listToHex ms)

type Message = [Word8]
data Chunk = Chunk { instPtr :: Word16, instBank :: Word8, lzBlocksCount :: Word8 } deriving Show

type DecodeTable = M.Map Word8 String
type EncodeTable = M.Map String Word8

----------------------------CONSTANTS-------------------------------------------
calcRomOffset :: (Word16, Word8) -> Int64 --calculate ROM offset by ptr-bank touple
calcRomOffset (ptr, bank) = (fromIntegral bank - 1) * 0x4000 + fromIntegral ptr

mteBank :: Word8
mteBank = 0x52

instanceTblOffs :: Int64
instanceTblOffs = calcRomOffset(0x41C3, mteBank)

dictPtrTblPtr :: Word16
dictPtrTblPtr = 0x4787

dictPtrTblOff :: Int64
dictPtrTblOff = calcRomOffset(dictPtrTblPtr, mteBank)


----------------------------INTERFACE-------------------------------------------
main :: IO()
main = getArgs >>= parse
  where
    parse ["-v"] = putStrLn "mgsgbLz v0.1\nMTE and LZ compression tool for MGS:GB game."
    parse ["-m"] = rebuildDictionary
    parse ["-d", "-b", inpName, offset] = decompressBlock inpName (read offset)
    parse ["-d", "-s", inpName] = decodeScript inpName
    parse ["-c", "-i", instNum, firstMsgNum, instP] = compressInst (read instNum) (read firstMsgNum) (read instP)
    parse ["-c", "-b", inpName] = compressBlock inpName
    parse ["-t", inpName] = checkScript inpName
    parse _ = putStrLn "Usage:\n\
      \  mgsgbLz -m                       Rebuild dictionary from plain script.\n\
      \  mgsgbLz -d -b <input> <offs>     Decompress one LZ block from input ROM at offset.\n\
      \  mgsgbLz -d -s <input>            Decode script from input ROM.\n\
      \  mgsgbLz -c -i <inst> <fst> <ptr> Encode one inst with given fst message and ptr.\n\
      \  mgsgbLz -c -b <input>            Compress one input plain binary block with LZ.\n\
      \  mgsgbLz -t <input>               Test check plain script for various issues.\n\
      \Options:\n\
      \  -h     Show this screen.\n\
      \  -v     Show version."


rebuildDictionary :: IO() --build and save new MTE dictionary
rebuildDictionary = do
  input <- readFile "mgsGbScript.txt"
  names <- readFile "names.txt"
  let
    encodeTable = buildEncodeTable decodeTable
    dictMte = buildDictionary (lines names) input
    dictMteSerialized = serializeDictionary encodeTable dictMte
  Bs.writeFile "mteDictionary.bin" $ Bs.pack dictMteSerialized


decompressBlock :: String -> Int64 -> IO() --decode one block from ROM at offset
decompressBlock inputName offset = do
  input <- Bs.readFile inputName
  let getBlock =  runGetOrFail getDecodedLzBlock $ Bs.drop offset input
  case getBlock of
    Left (_, _, errStr) -> error errStr
    Right (_, offs, block) -> do
      putStrLn $ printf "Packed LZ block size was 0x%X" offs
      Bs.writeFile "decompressedBlock.bin" $ Bs.pack block

decodeScript :: String -> IO() --decoding script
decodeScript inputName = do
      input <- Bs.readFile inputName
      let
        script = removeElement 14 $ map readInst chunks --last but one element is burried and blanked
        chunks = runGet (replicateM 16 getChunk) $ Bs.drop instanceTblOffs input
        getChunk = Chunk <$> getWord16le <*> getWord8 <*> getWord8
        readInst :: Chunk -> Inst
        readInst (Chunk ptr bank blocksCnt) = map readBlock blocksPtrTbl
          where
            readBlock p =  runGet getLzBlock $ Bs.drop (calcRomOffset (p, bank)) input
            getLzBlock = LzBlock <$> getWord8 <*> getDecodedLzBlockMessages --read last msg and then decode whole block
            blocksPtrTbl = runGet (replicateM (fromIntegral blocksCnt) getWord16le) $ Bs.drop (calcRomOffset (ptr, bank)) input

        dict = getDictionary dictPtrTblOff input
        mteDecodedScript = map (decodeMte dict) script
        plainScript = printScript decodeTable mteDecodedScript
      writeFile "mgsGbScript.txt" plainScript

compressBlock :: String -> IO()
compressBlock inputName = do
  input <- Bs.readFile inputName
  let
    blockPlain = Bs.unpack input
    blockLzEntries = encodeLz blockPlain
    lzStartByte = findNotUsedByte blockPlain
    blockLzSerialized = serializeLzEntries lzStartByte blockLzEntries
    compressedBlock = word16ToWord8 inputSize ++ lzStartByte : blockLzSerialized
    inputSize = fromIntegral $ length blockPlain
  Bs.writeFile "compressedBlock.bin" $ Bs.pack compressedBlock


compressInst :: Int -> Word8 -> Word16 -> IO()
compressInst instNum firstMsgNum instP = do
  input <- readFile "mgsGbScript.txt"
  dictMteFile <- Bs.readFile "mteDictionary.bin"
  let
    encodeTable = buildEncodeTable decodeTable
    dictMte = parseDictionary $ Bs.unpack dictMteFile
    instances = splitByDelimStr "{INSTANCE #"   input
    inst = buildInstance encodeTable dictMte firstMsgNum $ instances !! instNum
    instSerialized = serializeInst instP inst --0x5ebd for 8th inst
  Bs.writeFile ("inst"++ show instNum ++ ".bin") $ Bs.pack instSerialized

checkScript :: String -> IO()
checkScript inputName = do
  input <- U.readFile inputName
  let
    lenThreshold = 24 --maximum line length without warning
    encodeTable = buildEncodeTable decodeTable
    inputLines = lines input --tags are not counted
    textOnlyList = map (filterOutNewLineChars . removeTags)  inputLines
    filterOutNewLineChars = filter (not . flip elem "§¶¤⌂\n\r")
    allLookupedLines :: [(Int, [Int])] --line, columns of not-found items
    allLookupedLines = zip [1..] (map (lookupLine encodeTable) textOnlyList)
    notFoundLines = filter (not . null . snd)  allLookupedLines
    excessLines = map (+1) $ findIndices (\a -> length a > lenThreshold) textOnlyList
    newCharsOnlyList = map (filter (`elem` "§¶¤⌂")) inputLines
    excessRepeats = findRepeats newCharsOnlyList
  putStrLn $ "Chars not found (Ln, Col): " ++ show notFoundLines
  putStrLn $ "Excessive length lines: " ++ show excessLines
  putStrLn $ "3 newlines in a row numbers: " ++ show excessRepeats


lookupLine :: M.Map String Word8 -> String -> [Int] --get indexes of line, which are not found in table
lookupLine tbl input = map (+1) $ findIndices check input -- +1 as char pos zero based
  where
    check chr = isNothing $ M.lookup [chr] tbl

findRepeats :: [String] -> [Int] --get indexes of lines with three ¶ in a row
findRepeats  = go 1
  where
    go _ [] = []
    go count ("¶":"¶":"¶":ns) = count : go (count + 1) ("¶":"¶":ns)
    go count (_:ns) = go (count + 1) ns

---------------------------HELPERS----------------------------------------------

word16ToWord8 :: Word16 -> [Word8]
word16ToWord8 x = [fromIntegral (x .&. 0xFF), fromIntegral (x `shiftR` 8)]

word8ToWord16 :: [Word8] -> Word16
word8ToWord16 xs = if length xs < 2 then error "word8ToWord16 not enough bytes"
                                    else fromIntegral (head xs) + fromIntegral (xs !! 1) * 0x100

listToHex :: (PrintfArg a) => [a] -> String
listToHex = concatMap (printf "0x%04X, ")

removeElement :: Int -> [a] -> [a] --remove from list at given index
removeElement idx xs = start ++ tail end
  where (start, end) = splitAt idx xs

merge :: [a] -> [a] -> [a] --merge two lists intercalated
merge [] _ = [] --if first list is finished, return merged list
merge _ [] = [] --if second list finished, return merged list
merge (x:xs) ys = x:merge ys xs

----------------------------DICTIONARY------------------------------------------

buildDictionary :: [String] -> String -> [String]
buildDictionary fullNames input = fullNames ++ sortOn (Down . length) foundSubstrings --sort by length descending
  where
    foundSubstrings = take (0x100 - length fullNames) $ substrHistogram 3 16 stripped --dict has 0x100 entries, 16 symbols max in dict
    stripped = removeTags . removeNames $ filter (/='\n') input --delete names and tags from dict analysis
    names = nub $ filter (/="") fullNames --names can repeat

    removeNames :: String -> String --delete all found names from input script
    removeNames [] = []
    removeNames xss@(x:xs) = if  prefix /= [] then removeNames $ drop (length prefix) xss
                                              else x: removeNames xs
                             where
                               prefix = findPrefix names xss
                               findPrefix [] _ = []--return name, which is found, else []
                               findPrefix (n:ns) inp =  if n `isPrefixOf` inp then n
                                                             else findPrefix ns inp

removeTags :: String -> String
removeTags [] = [] --delete {} tags
removeTags(i:is) = if i == '{' then removeTags $ tail (dropWhile (/= '}') is)
                               else i:removeTags is

buildPtrTbl :: Word16 -> [[Word8]] -> [Word8] --build 16bit ptr tbl by table start addr and serialized entries
buildPtrTbl start entries = Bs.unpack $ runPut (mapM_ putWord16le tbl)
  where
    tbl = init $ scanl addLen (start + tblLen) entries --last entry doesn't generate
    tblLen = fromIntegral $ 2*length entries
    addLen accum xs = accum + fromIntegral(length xs)

serializeDictionary :: M.Map String Word8 -> [String] -> [Word8] --serialize dict by table and entries
serializeDictionary table entries = ptrTbl ++ dictBody
  where
    ptrTbl = buildPtrTbl dictPtrTblPtr terminatedEntries --0x200 is 2x100 dict entries
    dictBody = concat terminatedEntries
    terminatedEntries = map (serializeString table . (++ "§")) entries

splitByDelimStr :: String -> String -> [String] --split string by tags {..} bodies
splitByDelimStr delim input = map (safeTail.dropWhile (/='\n')) substWithDelims
  where substWithDelims = split (dropBlanks $ keepDelimsL $ onSublist delim) input
        safeTail xs = if null xs then error "it's in splitbydelimstr" else tail xs


parseDictionary :: [Word8] -> [Message] --get list of dictionary entries from serialized dict
parseDictionary input = splitOn [0xFF] $ drop 0x200 input --2x100 pointer bytes

getDictionary :: Int64 -> Bs.ByteString -> [Message] --read MTE dictionary from ROM
getDictionary tblOffs input = map (takeWhile (/=0xFF)) chunks --each entry FF terminateds
  where
    chunks = map (\offs -> Bs.unpack (Bs.drop offs input)) ptrTbl
    ptrTbl = map (\ptr -> calcRomOffset (ptr, mteBank)) ptrTblW16
    ptrTblW16 = runGet (replicateM 0x100 getWord16le) $ Bs.drop tblOffs input

------------------------------DECODE--------------------------------------------
enumNames :: String -> [String] --get infinite list of {%name% #XX} strings
enumNames name = ["{"++name++ printf" #%02d}\n" (n :: Int) | n<-[0..]]

printScript :: DecodeTable -> Script -> String
printScript tbl insts = concat $ merge (enumNames "INSTANCE") $ map printInst insts
  where
    printInst blocks = concat $ merge (enumNames "BLOCK") $ map printBlock blocks
    printBlock (LzBlock _ msgs) = concat $ merge (enumNames "MSG") $ map (decodeString tbl) msgs

decodeString :: DecodeTable -> Message -> String --decode one message
decodeString _ [] = []
decodeString table (byte:bytes)
  | byte `elem` paramCodes = printf "{%s 0x%02X}" foundEntry (head bytes) ++ decodeString table (tail bytes)
  | byte `elem` newLineCodes = foundEntry ++ "\n" ++ decodeString table bytes
  | otherwise  =  foundEntry ++ decodeString table bytes
  where
    foundEntry = fromMaybe (printf "\\%02X" byte) (M.lookup byte table)

getBinaryScript:: [Inst] -> [Word8]
getBinaryScript  = concatMap getInstScript
  where
    getInstScript :: Inst -> [Word8]
    getInstScript  = concatMap (concat.messages)


------------------------------ENCODE--------------------------------------------

buildEncodeTable :: M.Map k String -> M.Map String k
buildEncodeTable = M.fromList . map swap . M.toList
  where swap (k,v) = (v,k)

serializeString :: EncodeTable -> String -> [Word8]
serializeString tbl = go
  where
    go [] = []
    go ('\n':is) = go is--discard newlines - only for pretty printing script
    go input@('{':_) = [tag, arg] ++ go (drop (length tagBody) input)
      where
        tagBody = takeWhileInclusive (/= '}') input
        (tagStr, tagArgStr) = break (== ' ') $ init $ tail tagBody --drop tag brackets
        arg = read tagArgStr :: Word8
        tag :: Word8
        tag = fromMaybe (error (printf "Tag not found: %s" tagBody))
                    (M.lookup tagStr tbl)
        takeWhileInclusive _ [] = [] --takeWhile, but include last element
        takeWhileInclusive p (x:xs) = x : if p x
                                            then takeWhileInclusive p xs
                                            else []
    go (i:is) = tag : go strippedTail
      where
        tag = fromMaybe (error (printf "Char \"%s\" with code %d not found" [i] (ord i)))
                                 (M.lookup [i] tbl)
        strippedTail--newline should go right after symbol, discard spaces etc. which will go in serialized otherwise
          | tag `elem` newLineCodes = dropWhile (/= '\n') is
          | otherwise = is



findNotUsedByte :: [Word8] -> Word8 --find byte in input, which is not used for LZ encoding flag
findNotUsedByte xs = go [0..0xFF]
  where
    go [] = error "Lz start byte not found"
    go (n:ns) = if n `notElem` xs then n else go ns

--build instance from char encoding table, serialized MTE dictionary and
--plain instance in string
buildInstance :: M.Map String Word8 -> [Message] -> Word8 -> String -> Inst
buildInstance encodeTable mteDict firstMsgNum plainBlock = blocks
  where
    plainBlocks =  splitByDelimStr "{BLOCK #" plainBlock
    blocksMessages = map (splitByDelimStr "{MSG #") plainBlocks --divide on blocks and messages:: [[String]]
    msgCounts = tail $ scanl addLen firstMsgNum blocksMessages--start with first length
    addLen accum strs = accum + fromIntegral (length strs)
    blocks = zipWith buildBlock msgCounts blocksMessages
    buildBlock lastMsgIdx msgs = LzBlock lastMsgIdx mteEncodedMsgs --assemble into LzBlock structure
      where
        serializedMsgs = map (serializeString encodeTable) msgs
        mteEncodedMsgs = map (encodeMte mteDict) serializedMsgs

serializeInst :: Word16 -> Inst -> [Word8] --serialize instance with ptr in ROM and instance given
serializeInst instPtrTblPtr blocks = ptrTbl ++ concat serializedBlocks
  where
    ptrTbl = buildPtrTbl instPtrTblPtr serializedBlocks --build pointer table out of blocks
    serializedBlocks = zipWith serializeBlock blocks [0..] --actual LZ blocks of instance

serializeBlock :: LzBlock -> Int -> [Word8] --serialize one LZ block
serializeBlock (LzBlock lastIdx msgs) blockNumber = if blockSize > 0x800
    then error (printf "Block #%02d length 0x%X > 0x800, WRAM overflow. ABORTED!" blockNumber blockSize)
    else lastIdx : size ++ lzStartByte : blockLzSerialized --game's format
  where
    size = word16ToWord8 blockSize
    lzStartByte = findNotUsedByte blockPlain
    blockLzSerialized = serializeLzEntries lzStartByte blockLz
    blockLz = encodeLz blockPlain
    blockPlain = blockPtrTbl ++ concat msgs
    blockPtrTbl = buildPtrTbl 0 msgs --pointer table 0-based for actual messages in block
    blockSize = fromIntegral $ length blockPlain

---------------------------MTE--------------------------------------------------
decodeMte :: [Message] -> Inst -> Inst --decode instance with given dictionary
decodeMte dict  = map decodeBlock
  where
    decodeBlock (LzBlock lstMsg msgs) = LzBlock lstMsg (map decodeMsg msgs)
    decodeMsg [] = []
    decodeMsg [x] =  [x] --replace MTE entries format FB{CODE}
    decodeMsg (x:idx:xs)
      | x == 0xFB = dict !! fromIntegral idx ++ decodeMsg xs
      | otherwise  = x : decodeMsg (idx:xs)

substrHistogram :: Int -> Int -> String -> [String] --get repeated substrings list sorted by freq*length
substrHistogram minLen maxLen is = takeWhile (/=[]) $ go [is] --loop while repeating substrings are found
 where
   go xs = bestMte : go strippedInput
     where
       strippedInput = concatMap (splitOn bestMte) xs --strip found mte entry from input string
       --find most frequent substring in input stings list
       bestMte  = if M.null freqMap then [] --no more repeated substrings found
                  else  fst $ maximumBy lenFreqCompare $ M.toList freqMapNoSpecial
         where
           lenFreqCompare (w1, f1) (w2, f2) = (f1*length w1) `compare` (f2 * length w2) --compare words by length and frequency
           freqMapNoSpecial = M.filterWithKey noSpecialChars freqMap
           noSpecialChars s _ = all (`notElem` s) ['§','¶','¤','⌂']
           freqMap = M.filter (> 1) $ M.fromListWith (+) [(c, 1) | c <- getAllWords] --don't take words, which don't repeat
           getAllWords = concatMap wordsStartingHere $ concatMap (init. tails) xs --get all possible words starting here. last is empty, discard
           wordsStartingHere ys = drop minLen $ take (maxLen+1) $ inits ys

encodeMte :: [Message] -> Message -> Message --encode serialized script with MTE table given
encodeMte _ [] = []
encodeMte dictMte input@(i:is) = if  isJust dictIndexLen --if found, then emit MTE byte touple
                                  then  [0xFB, fst (fromJust dictIndexLen)] ++
                                    encodeMte dictMte (drop (snd (fromJust dictIndexLen)) input)
                                  else i: encodeMte dictMte is --not found in dict, emit literal
                             where
                               dictIndexLen :: Maybe (Word8, Int) --return (index, len) of found mte entry, else Nothing
                               dictIndexLen = findPrefix dictMte input 0
                               findPrefix [] _ _ = Nothing --whole dictionary checked, no prefixes found
                               findPrefix ([]:ns) inp count = findPrefix ns inp  (count + 1) --empty entry in mte table, pass
                               findPrefix (n:ns) inp count =  if n `isPrefixOf` inp then Just (count, length n)
                                                                else findPrefix ns inp (count + 1)

---------------------------LZ---------------------------------------------------
getDecodedLzBlockMessages :: Get [Message]
getDecodedLzBlockMessages = do
  decodedBlock <- getDecodedLzBlock
  let
    msgPtrTblLen = fromIntegral $ word8ToWord16 decodedBlock --fist pointer points to end of ptr table
    msgPtrTbl = map (fromIntegral.word8ToWord16) $ chunksOf 2 $ take msgPtrTblLen decodedBlock
    readMsg :: Int -> Message
    readMsg offs = go (drop offs decodedBlock)
      where
        go [] = []
        go (0xFB:code:xs) = 0xFB:code:go xs --we're in MTE, don't detect FF
        go (0xFF:_) = [0xFF] --end of msg found
        go (x:xs) = x : go xs --usual char, takt to msg
  return $ map readMsg msgPtrTbl

getDecodedLzBlock :: Get [Word8]
getDecodedLzBlock =  do
  entries <- getLzEntries
  --error $ show entries --debug entries
  return $ decodeLz entries

getLzEntries :: Get [LzEntry]
getLzEntries = do
  unpackedSize <- getWord16le
  lzStartByte <- getWord8
  let
    go :: Int -> Get [LzEntry]
    go count = if count >= fromIntegral unpackedSize
        then return [] --unpacked whole block, exit
        else do
          byte <- getWord8
          if byte /= lzStartByte
            then do --raw copy byte
              rest <- go (count + 1)
              return (Liter byte : rest)
            else do --lz copy
              distLo <- getWord8
              distLenByte <- getWord8
              let
                lenNybble = distLenByte .&. 0xF
                distHi = distLenByte `shiftR` 4
                distance :: Int
                distance = (fromIntegral distHi `shiftL` 8) + fromIntegral distLo
              if lenNybble == 0xF
                then do --extended length mode
                  fullLen <- getWord8
                  let lenLong = fromIntegral fullLen + 0x13
                  rest <- go (count + lenLong)
                  return (Refer lenLong distance : rest)
                else do --normal lz copy
                  let lenShort = fromIntegral $ lenNybble + 4
                  rest <- go (count + lenShort)
                  return (Refer lenShort distance : rest)
  go 0 --finally just getting entries starting with zero counter

decodeLz :: [LzEntry] -> [Word8] --decode given LzEntries
decodeLz  =  go []
  where
    go buffer [] = buffer
    go buffer (Liter b:es) = go (buffer ++ [b]) es
    go buffer (Refer l d:es)
      | d >= length buffer = go (buffer ++ zeroChunk) (Refer restLen 0 : es)--if we read beyond cur pos, we read zeroes from inited memory and then ring back to start of buffer
      | otherwise = go (buffer ++ refChunk) es
      where
        zeroChunk = take l (replicate cutLen 0)
        cutLen = windowSize + 1 - d --the rest of inited memory till the end of buffer
        restLen = l - cutLen
        refChunk = take l . cycle' $ drop d buffer --usual lz copy
                                                  --infinite cycle list. If length>distance, we will read data, already decoded in this reference
        cycle' xs = if null xs then xs else cycle xs --cycle, that works with empty lists


windowSize :: Int
windowSize = 0xFFF --4096 as distance can be encoded only by 12 bits
matchLenMax :: Int
matchLenMax = 0xFF --length of match can be encoded only by byte

--get latest index of  length common chunk. Returns length - distance tuple. (0, 0) means not found.
--ByteStrings are used for speed of 'indices' string search algorithm
findInBufferMatch :: Bs.ByteString -> Bs.ByteString -> (Int, Int)
findInBufferMatch needle' haystack' = go n haystack'
  where
    n = strictify $ Bs.take (fromIntegral matchLenMax) needle'
    -- memory is not inited in some cases, do not rely on tailzeros and ringbuffer - delete after testing
    --h = Bs.concat [haystack', tailZeros, startOfHay] --form a ring of buffer after window
    --tailZeros = Bs.replicate (fromIntegral windowSize - Bs.length haystack' + 1) 0 --uninited memory within window
    --startOfHay = Bs.take (fromIntegral matchLenMax) haystack' --continue from beginning as ring buffer
    go :: B.ByteString -> Bs.ByteString -> (Int, Int)
    go needle haystack
      | B.null needle = (0, 0) --match is not found at all
      | Bs.null (snd index)  = go (B.init needle) haystack --full needle not found, cut and search again
      | d < fromIntegral (Bs.length haystack') && B.length needle + d > fromIntegral(Bs.length haystack') = go (B.init needle) haystack --that's outbuffer case
      | otherwise   = (fromIntegral (B.length needle), d)
      where
        index = breakOn needle haystack--indices needle haystack
        d = fromIntegral $ Bs.length $ fst index --first found match in buffer will be returned

--define if needle is a circular list with period of len, then we can encode with length > distance
--find length of haystack suffix, and then check max match between cycle list
--and needle
findOutBufferMatch :: (Eq a) => [a] -> [a] -> (Int, Int)
findOutBufferMatch needle' haystack'
  | length needle > fromIntegral windowSize || null needleMatch = (0, 0) --needle was not found at the end of haystack
  | otherwise = (outBufferLen, d)
  where
    needle = take  matchLenMax needle'
    haystack = take windowSize haystack'
    needleMatch = getSuffix needle
    getSuffix n = if n `isSuffixOf` haystack then n else getSuffix $ init n
    --take while needle is equal to infinite circular array and output length
    outBufferLen = length . takeWhile id  $ zipWith (==) needle $ cycle needleMatch
    d = length haystack - length needleMatch

--Match is found in 2 passes: first search for match in passed buffer window (InBufferMatch),
--then we check again if full needle is circular list with a period of found match length (OutBufferMatch)
--so we could encode with len > dist
--Then we skip 1 byte and see if we could get better result, compare that and
--emit optimal LzEntry.
encodeLz :: [Word8] -> [LzEntry]
encodeLz inp = encodeLz' inp [] --start with zero buffer
  where
  encodeLz' [] _ = [] -- end of stream
  encodeLz' input@(i:is) buffer
    | l < 4 || l < l2 = Liter i : encodeLz' is (buffer ++ [i])--skip one literal if refer too short or ther's longer refer ahead
    | otherwise =  Refer l d : encodeLz' (drop l input) (buffer ++ take l input)
    where (l, d)   = findLzMatch input buffer
          (l2, _) = findLzMatch is (buffer ++ [i]) --search for a longer match at the next input byte (LZ non greedy string parse)
          findLzMatch needle haystack = max (findInBufferMatch (Bs.pack needle) (Bs.pack haystack)) (findOutBufferMatch needle haystack)

--serialize entries by startbyte and list of entries
serializeLzEntries :: Word8 -> [LzEntry] -> [Word8]
serializeLzEntries startByte = concatMap serializeLzEntry
  where
  serializeLzEntry (Liter i) = [i] --serialization of one LzEntry
  serializeLzEntry (Refer l d)
    | l < 0x13 = [startByte, distLo, distHiLen] --control bytes are dd Dl
    | otherwise = [startByte, distLo, distHiOrF, extLen] --extended length case
    where
      distLo = fromIntegral $ d .&. 0xFF
      distHiLen = fromIntegral $ ((l - 4).&.0xF) + ((d .&. 0xF00) `shiftR` 4)
      distHiOrF = fromIntegral $ 0xF + ((d .&. 0xF00) `shiftR` 4)
      extLen = fromIntegral $ (l - 0x13) .&. 0xFF
