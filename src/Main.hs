module Main where
import           Control.Monad
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as Bs
import           Data.ByteString.Lazy.Search (breakOn, strictify)
import qualified Data.HashMap.Strict         as HM
import           Data.Int
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Word                   (Word16, Word8)
import           Numeric                     (showHex)
import           System.Console.ANSI
import           System.Environment
import           System.IO
import           Text.Parsec                 hiding (count)
import           Text.Parsec.String
import           Text.Printf


import           RomhackLib                  (DecodeTable, EncodeTable,
                                              addComments, decodeByTable,
                                              encodeByTable, readDecodeTable,
                                              readEncodeTable,
                                              scriptEntriesParser)


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
main = getArgs >>= parseArgs
  where
    parseArgs ["-v"] = putStrLn "mgsgbLz v0.3\nMTE and LZ compression tool for MGS:GB game."
    parseArgs ("-ra": inpNames) = rebuildDictionaryAdaptive inpNames
    parseArgs ["-r", inpName] = rebuildDictionary inpName
    parseArgs ["-d", "-b", inpName, offset] = decompressBlock inpName (read offset)
    parseArgs ["-d", "-s", inpName] = decompressScript inpName
    parseArgs ["-c", "-i", inpName, firstMsgNum, instP] = compressInst inpName (read firstMsgNum) (read instP)
    parseArgs ["-c", "-b", inpName] = compressBlock inpName
    parseArgs ["-t", inpName] = checkInstance inpName
    parseArgs _ = putStrLn "Usage:\n\
      \  mgsgbLz -r <input>               Rebuild dictionary from plain script file.\n\
	  \  mgsgbLz -ra <inputs> <weights>   Rebuild adaptive from multipe files.\n\
      \  mgsgbLz -d -b <input> <offs>     Decompress one LZ block from input ROM at offset.\n\
      \  mgsgbLz -d -s <input>            Decode script from input ROM.\n\
      \  mgsgbLz -c -i <inst> <fst> <ptr> Encode one inst with given fst message and ptr.\n\
      \  mgsgbLz -c -b <input>            Compress one input plain binary block with LZ.\n\
      \  mgsgbLz -t <input>               Test check plain script instance for various issues.\n\
      \Options:\n\
      \  -h     Show this screen.\n\
      \  -v     Show version."

rebuildDictionary :: String -> IO() --build and save new MTE dictionary from single script file
rebuildDictionary inputName = do
  input <- readFile inputName
  tableStr <- readFile "table.tbl"
  fullNamesStr <- readFile "names.txt"
  let
    fullNames = lines fullNamesStr
    names = nub $ filter (not.null) fullNames --names can repeat
    encodeTable = readEncodeTable tableStr
    decodeTable = readDecodeTable tableStr

    commentsFreeLines = filter (not . isPrefixOf "//") $ lines input --skip comments
    tagsFreeLines = concatMap splitOnTags commentsFreeLines--tags cannot be in MTE
    strippedInput = concatMap (splitOnStrings names) tagsFreeLines --names are forced in dictionary
    encodedChunks = map (B.pack . encodeByTable encodeTable) $ filter (not.null) strippedInput
    mteEntries = take (0x100 - length fullNames) $ getMteFromBs encodedChunks

    plainEntries = map (decodeByTable decodeTable . B.unpack)  mteEntries
    plainDictionary = fullNames ++ plainEntries --0x100 MTE entries
    updateProgress :: (Int, String) -> IO()
    updateProgress (num, _) = hPutStr stderr (printf "\rSearching... [%03d/256]" num)

    encodedDictionary = map ((++[0xFF]) . encodeByTable encodeTable) plainDictionary
    ptrTbl = buildPtrTbl dictPtrTblPtr encodedDictionary
  mapM_ updateProgress (zip [1..] plainDictionary) --show progress
  clearLine
  hPutStrLn stderr "\rDone."
  writeFile "mteDictionaryPlain.txt" $ unlines plainDictionary --for information
  Bs.writeFile "mteDictionary.bin" $ Bs.pack $ ptrTbl ++ concat encodedDictionary

rebuildDictionaryAdaptive :: [String] -> IO() --build and save new MTE dictionary from multiple script files
rebuildDictionaryAdaptive params = do
  let
    filesList :: [(String, Int)]
    filesList = makeTuples params

    makeTuples (name: portion:xs) = (name, read portion): makeTuples xs
    makeTuples _                  = []

  inputs <- mapM (readFile . fst) filesList
  tableStr <- readFile "table.tbl"
  fullNamesStr <- readFile "names.txt"
  let
    fullNames = lines fullNamesStr
    names = nub $ filter (not.null) fullNames --names can repeat
    encodeTable = readEncodeTable tableStr
    decodeTable = readDecodeTable tableStr

    getMteEntries :: String -> [String]
    getMteEntries input = map (decodeByTable decodeTable . B.unpack)  mteEntries
      where
        commentsFreeLines = filter (not . isPrefixOf "//") $ lines input --skip comments
        tagsFreeLines = concatMap splitOnTags commentsFreeLines--tags cannot be in MTE
        strippedInput = concatMap (splitOnStrings names) tagsFreeLines --names are forced in dictionary
        encodedChunks = map (B.pack . encodeByTable encodeTable) $ filter (not.null) strippedInput
        mteEntries = take (0x100 - length fullNames) $ getMteFromBs encodedChunks

    mteList = map getMteEntries inputs
    mtePairs = mteList `zip` map snd filesList
    plainEntries = mergeByRatios mtePairs $ 0x100 - length fullNames
    plainDictionary = fullNames ++ plainEntries --0x100 MTE entries

    updateProgress :: (Int, String) -> IO()
    updateProgress (num, _) = hPutStr stderr (printf "\rSearching... [%03d/256]" num)

    encodedDictionary = map ((++[0xFF]) . encodeByTable encodeTable) plainDictionary
    ptrTbl = buildPtrTbl dictPtrTblPtr encodedDictionary
  mapM_ updateProgress (zip [1..] plainDictionary) --show progress
  clearLine
  hPutStrLn stderr "\rDone."
  writeFile "mteDictionaryPlain.txt" $ unlines plainDictionary --for information
  Bs.writeFile "mteDictionary.bin" $ Bs.pack $ ptrTbl ++ concat encodedDictionary


--take from each of given lists based on it's given proportion in tuple
--(list, weight), the bigger the weight, the longer prefix will be taken
--from exactly this list and other lists portion will be reduced
mergeByRatios :: (Eq a) => [([a], Int)] -> Int -> [a]
mergeByRatios pairs total = foldl takeUnique [] lenPairs
  where
    lists = map fst pairs
    proportions = map snd pairs
    totalProportions = sum proportions
    unitLen = total `div` totalProportions
    initLengths = map (*unitLen) $ init proportions
    lengths = initLengths ++ [total - sum initLengths]--last takes leftover space
    lenPairs = lengths `zip` lists

takeUnique :: (Eq a) => [a] -> (Int, [a]) -> [a] --take n elems, only if not found in haystack
takeUnique haystack pairs = haystack ++ go pairs
  where
    go (0, _) = [] --end of count
    go (_, []) = [] --needle exhausted
    go (n, x : xs) = if x `elem` haystack
      then go (n, xs)
      else x : go (n - 1, xs)



divideInterval :: Int -> Int -> [Int] --list of lengths for dividing given range by n chunks
divideInterval range n = replicate (n - 1) meanLen ++ [lastChunk]
  where
    meanLen = range `div` n
    lastChunk = range - (meanLen * (n - 1))

decompressBlock :: String -> Int64 -> IO() --decode one block from ROM at offset
decompressBlock inputName offset = do
  input <- Bs.readFile inputName
  let getBlock =  runGetOrFail getDecodedLzBlock $ Bs.drop offset input
  case getBlock of
    Left (_, _, errStr) -> error errStr
    Right (_, offs, block) -> do
      putStrLn $ printf "Packed LZ block size was 0x%X" offs
      Bs.writeFile "decompressedBlock.bin" $ Bs.pack block

decompressScript :: String -> IO() --decoding script
decompressScript inputName = do
      input <- Bs.readFile inputName
      decodeTblStr <- readFile "table.tbl"
      let
        decodeTable = readDecodeTable decodeTblStr
        instances = removeElement 14 $ map readInst chunks --last but one element is burried and blanked
        chunks = runGet (replicateM 16 getChunk) $ Bs.drop instanceTblOffs input
        getChunk = Chunk <$> getWord16le <*> getWord8 <*> getWord8
        readInst :: Chunk -> Inst
        readInst (Chunk ptr bank blocksCnt) = map readBlock blocksPtrTbl
          where
            readBlock p =  runGet getLzBlock $ Bs.drop (calcRomOffset (p, bank)) input
            getLzBlock = LzBlock <$> getWord8 <*> getDecodedLzBlockMessages --read last msg and then decode whole block
            blocksPtrTbl = runGet (replicateM (fromIntegral blocksCnt) getWord16le) $ Bs.drop (calcRomOffset (ptr, bank)) input

        dict = getDictionary dictPtrTblOff input
        mteDecodedInstances = map (decodeMte dict) instances
        outFileNames = ["instance "++ printf"#%02d.txt" (n :: Int) | n<-[0..]]
        plainInstances = map (printInstance True decodeTable) mteDecodedInstances
      mapM_ (uncurry writeFile) $ zip outFileNames plainInstances

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


compressInst :: String -> Word8 -> Word16 -> IO()
compressInst inputName firstMsgNum instP =   do
  input <- readFile inputName
  dictMteFile <- Bs.readFile "mteDictionary.bin"
  encodeTableStr <- readFile "table.tbl"
  let
    encodeTable = readEncodeTable encodeTableStr
    dictMte = splitOn [0xFF] $ drop 0x200 $ Bs.unpack dictMteFile
    inst = buildInstance encodeTable dictMte firstMsgNum input
    instSerialized = serializeInst instP inst --0x5ebd for 8th inst
    dropExt  = reverse . tail . dropWhile ('.' /=) . reverse
  Bs.writeFile (dropExt inputName ++ ".bin") $ Bs.pack instSerialized


checkInstance :: String -> IO() --check for possible script errors
checkInstance inputName  = do
  input <- readFile inputName
  tableStr <- readFile "table.tbl"
  let
    encodeTbl = readEncodeTable tableStr
    inputLines = lines input
    noDelimsInputLines = map nullDelims inputLines --nullify delimeters
    nullDelims str = if isDelimStr str then [] else str
    isDelimStr str = any (`isPrefixOf` str) ["[block #", "[message #", "//"]

    stringPixelLen str = calcPixLen (encodeByTable encodeTbl stripped)
              where
                stripped = concat $ splitOnTags str  --strip out tags
                calcPixLen charCodes = sum $ map ((lengthList !!) . fromIntegral) charCodes
                  where
                    lengthList :: [Int] --pixel width of each char code
                    lengthList = [6,3,6,6,7,6,6,6,6,6,
                                  6,6,6,6,5,5,7,6,4,7,
                                  6,5,7,6,6,6,6,6,6,6,
                                  6,6,8,6,6,6,9,3,8,6,
                                  6,3,5,3,8,5,5,2,6,5,
                                  5,6,5,6,5,5,2,5,5,2,
                                  8,5,5,5,5,5,5,4,6,6,
                                  8,6,5,5,8,8,8,8,8,8,
                                  8,0,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
                                  6,6,6,5,7,5,6,6,6,6,6,6,6,6,6,6,
                                  6,6,6,6,6,8,6,7,6,6,7,7,7,6,6,7,
                                  6,6,5,5,5,5,5,5,8,6,6,6,5,6,6,5,
                                  5,5,5,5,6,5,6,6,6,5,6,7,6,6,5,5,
                                  7,5,7,7,8,8,8,8,8,8,8,8,8,8,8,8,
                                  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
                                  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
                                  5,6,4,3,3,8,8,5,3,5,5,6,8,8,8,3]

    excessPixLines = map (+1) $ findIndices (\s -> stringPixelLen s > 16*8) noDelimsInputLines
  --  shortPixLines = map (+1) $ findIndices (\s -> stringPixelLen s < 8*8) noDelimsInputLines
    whiteSpaceLines =  map (+1) $ findIndices isWhiteSpace noDelimsInputLines
      where
        isWhiteSpace str
          | length reversed < 2 = False --line less than 2 chars
          | head reversed == 0x2F = True --space after eol tag
          | reversed !! 1 == 0x2F = True
          | otherwise = False
          where reversed = reverse $ encodeByTable encodeTbl str

    nlCounts = scanl countNls 0 inputLines
    countNls :: Int -> String -> Int --count newlines, considering previous count accum
    countNls accum str = if isDelimStr str
                          then  0 --delims terminated count
                          else  foldl countStrNl accum $ encodeByTable encodeTbl str
      where
        countStrNl accum2 code
          | code == 0xFE = accum2 + 1 --newline increment count
          | code `elem` [0xF8, 0xFD, 0xFF] = 0 --eos, scroll, clear resets count
          | otherwise = accum2
    fourthLineNumbers = findIndices (>2) nlCounts
    fifthLineNumbers =  findIndices (>3) nlCounts

  putStrLn $ "Lines longer 16 tiles: " ++ show excessPixLines
  putStrLn $ "Out of 3-lines dialog screen bounds lines: " ++ show fourthLineNumbers
  putStrLn $ "Out of 4-lines info screen bounds lines: " ++ show fifthLineNumbers
  -- putStrLn $ "Lines shorter 8 tiles: " ++ show shortPixLines
  putStrLn $ "Lines with whitespaces near newline tag: " ++ show whiteSpaceLines


splitOnTags :: String -> [String] --split out tags between []
splitOnTags str = filter (not.null) stripped
  where
    stripped = head splitted : map (tail . dropWhile (/=']')) (tail splitted)
    splitted = splitOn "[" str

splitOnStrings :: [String] -> String -> [String] --split out list of substrings from string
splitOnStrings needles haystack = filter (not.null) splitted
  where
    splitted = foldl splitStr [haystack] needles
    splitStr accum needle = concatMap (splitOn needle) accum


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
merge [] _      = [] --if first list is finished, return merged list
merge _ []      = [] --if second list finished, return merged list
merge (x:xs) ys = x:merge ys xs

----------------------------DICTIONARY------------------------------------------

buildPtrTbl :: Word16 -> [[Word8]] -> [Word8] --build 16bit ptr tbl by table start addr and serialized entries
buildPtrTbl start entries = Bs.unpack $ runPut (mapM_ putWord16le tbl)
  where
    tbl = init $ scanl addLen (start + tblLen) entries --last entry doesn't generate
    tblLen = fromIntegral $ 2*length entries
    addLen accum xs = accum + fromIntegral(length xs)

getDictionary :: Int64 -> Bs.ByteString -> [Message] --read MTE dictionary from ROM
getDictionary tblOffs input = map (takeWhile (/=0xFF)) chunks --each entry FF terminateds
  where
    chunks = map (\offs -> Bs.unpack (Bs.drop offs input)) ptrTbl
    ptrTbl = map (\ptr -> calcRomOffset (ptr, mteBank)) ptrTblW16
    ptrTblW16 = runGet (replicateM 0x100 getWord16le) $ Bs.drop tblOffs input

------------------------------DECODE--------------------------------------------

printInstance :: Bool -> DecodeTable -> Inst -> String
printInstance isAddComments tbl blocks = concat $ merge (enumNames "block") $ map printBlock blocks
  where
    --get infinite list of [%name% #XX] strings
    enumNames name = ["["++name++ printf" #%02d]\n" (n :: Int) | n<-[0..]]
    printBlock (LzBlock _ msgs) = concat $ merge (enumNames "message")
                                    $ map appendNewLine plainMsgs
      where
        plainMsgs = if isAddComments then addComments decodedMsgs else decodedMsgs
        --omit closing 0xFF for pretty print and replace it with newline
        decodedMsgs = map (decodeByTable tbl . init) msgs
        appendNewLine s = s ++ ['\n']

------------------------------ENCODE--------------------------------------------

findNotUsedByte :: [Word8] -> Word8 --find byte in input, which is not used for LZ encoding flag
findNotUsedByte xs = go [0..0xFF]
  where
    go []     = error "Lz start byte not found"
    go (n:ns) = if n `notElem` xs then n else go ns

--build instance from char encoding table, serialized MTE dictionary and
--plain instance in string
buildInstance :: EncodeTable -> [Message] -> Word8 -> String -> Inst
buildInstance encodeTable mteDict firstMsgNum inst =
                                    zipWith buildBlock msgCounts serializedMsgs
  where --append with FF as end of message was omitted in printed script
    serializedMsgs = map (map (\s -> s ++ [0xFF])) parsedMsgs
    parsedMsgs = parseMsgsByTable encodeTable inst
    msgCounts = tail $ scanl addLen firstMsgNum serializedMsgs--start with first length
    addLen accum strs = accum + fromIntegral (length strs)
    buildBlock lastMsgIdx msgs = LzBlock lastMsgIdx mteEncodedMsgs --assemble LzBlock
      where mteEncodedMsgs = map (encodeMte mteDict) msgs

-- split on [blocks[messages]] and encode by table
parseMsgsByTable :: EncodeTable -> String -> [[Message]]
parseMsgsByTable encodeTbl plainInst = case parse (instanceParser encodeTbl)
                                  "Script parse failed" plainInst of
                                    Left err     -> error $ show err
                                    Right blocks -> blocks

--parse multiple entries into inclosed lists of bytes [block[messages]]
instanceParser :: EncodeTable ->  Parser [[Message]]
instanceParser encodeTable = blockDelim >> block `manyTill` eof
  where
    block =  messageDelim >> message `manyTill` (eof <|> try blockDelim)
    message = scriptEntriesParser encodeTable messageEndings
    messageEndings = eof <|> Text.Parsec.lookAhead (try blockDelim) <|> try messageDelim
    messageDelim = lexeme $ voidLine"[message #"
    blockDelim = lexeme $ voidLine "[block #"
    lexeme p = many (newLines <|> comments) >> p
    comments = void $ try $ string "//" >> anyChar `manyTill` newline
    newLines = void $ try newline
    voidLine :: String -> Parser ()
    voidLine s = void $ string s >> anyChar `manyTill` newline

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

getMteFromBs :: [B.ByteString] -> [B.ByteString]
getMteFromBs [] = []
getMteFromBs strs = if null subs then [] -- no more substrings possible, exhausted string
                        else bestMte : getMteFromBs strippedInput --strip and find next MTE
  where
    mteCodeSize = 1 -- 1 byte per MTE code additionally
    minLen = 3
    maxLen = 16
    subs = concatMap substrings strs
    substrings s = [ i | t <- B.tails s,
                          i <- drop minLen (take maxLen (B.inits t))]
    (bestMte, _, _) = HM.foldlWithKey' getMax (B.empty, 0, 0) freqMap
    freqMap :: HM.HashMap B.ByteString Int
    freqMap = HM.fromListWith (+) [(s, 1) | s <- subs]

    getMax (accStr, accCount, accWeight) str count
      | curWeight > accWeight = (str, count, curWeight)
      | curWeight == accWeight && count > accCount = (str, count, curWeight)
      | otherwise = (accStr, accCount, accWeight)
      where curWeight = count * (B.length str - mteCodeSize)
        --weight = replace len x number of occurences - MTE bytes taken
    strippedInput = concatMap strip strs
    strip haystack = clean : if B.null rest then []
                              else strip $ B.drop mteLen rest
      where
        (clean, rest) = B.breakSubstring bestMte haystack
        mteLen = B.length bestMte

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
        go []             = []
        go (0xFB:code:xs) = 0xFB:code:go xs --we're in MTE, don't detect FF
        go (0xFF:_)       = [0xFF] --end of msg found
        go (x:xs)         = x : go xs --usual char, takt to msg
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
