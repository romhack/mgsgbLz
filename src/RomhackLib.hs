--Romhack library by Griever.
--V 0.2 201709
module RomhackLib ( showListHex
                  , CharCode
                  , DecodeTable, EncodeTable, TableEntry(..)
                  , readDecodeTable, readEncodeTable
                  , decodeByTable, addComments, encodeByTable, scriptEntriesParser
                  , dropDuplicateMsgs, splitByEndCode
                  , BinaryBlockVars(..), PtrTableVars(..)
                  , readW8Tail, readBsTail, writeBlock, readW16LePtrTable, readW8Block)
                  where
import           Control.Monad
import           Data.Binary.Get        hiding (lookAhead)
import qualified Data.ByteString        as Bss
import qualified Data.ByteString.Lazy   as Bs
import           Data.Int
import           Data.List
import qualified Data.List.Split        as S
import           Data.Maybe
import           Data.Ord
import           Data.Word
import           Numeric
import           Text.Parsec            hiding (count)
import           Text.Parsec.Combinator
import           Text.Parsec.String
import           Text.Printf



type CharCode = Word8

data PtrTableVars = PtrTableVars { ptrTableOffset :: Int64
                                  ,msgCount       :: Int64
                                  ,ptrBase        :: Int64
                                } deriving (Show)
data BinaryBlockVars = BinaryBlockVars {  blockName   :: String
                                        , blockOffset :: Int64
                                        , blockSize   :: Int64
                                        , fillByte    :: Word8}

type DecodeTable = [(CharCode, TableEntry)]
type EncodeTable = [(String, TableEntry)]


data TableEntry = NormalEntry {code ::CharCode, str ::  String}
                | EndToken {code:: CharCode, str:: String, nls:: Int}
                | ControlCode {code:: CharCode, str:: String, nls:: Int,
                              params:: [String]}
                  deriving (Show)

--read offsets in ROM for each of pointer entry
readW16LePtrTable :: Bs.ByteString -> PtrTableVars -> [Int64]
readW16LePtrTable input (PtrTableVars offset cnt base) =
    map ((+ base) . fromIntegral) pointers
  where
    pointers :: [Word16]
    pointers = runGet (replicateM (fromIntegral cnt) getWord16le) (Bs.drop offset input)

readW8Block :: Bs.ByteString -> BinaryBlockVars -> [Word8]
readW8Block input (BinaryBlockVars _ offset size _) = Bs.unpack binaryBlock
  where
    binaryBlock =  Bs.take size $ Bs.drop offset input

readW8Tail :: Bs.ByteString -> Int64 -> [Word8]
readW8Tail input offset = Bs.unpack $ Bs.drop offset input

readBsTail :: Bs.ByteString -> Int64 -> Bs.ByteString
readBsTail input offset = Bs.drop offset input

--print hex values for each element of list
showListHex :: (PrintfArg a) => [a] -> String
showListHex xs = "["++ concatMap (printf "0x%02X,") xs ++ "]"

writeBlock :: BinaryBlockVars -> String -> Bs.ByteString -> IO()
writeBlock  (BinaryBlockVars patchName offset size fillB) dstName patchBlock = do
  dstFile <- Bss.readFile dstName
  let
    dummyLen = size - Bs.length patchBlock
    dummy = Bss.replicate (fromIntegral dummyLen) fillB
    insertBlock :: Bss.ByteString
    insertBlock = Bs.toStrict patchBlock `Bss.append` dummy
    patchLen = Bs.length patchBlock
    start = Bss.take (fromIntegral offset) dstFile
    end = Bss.drop  (fromIntegral (offset + size)) dstFile
  if patchLen > size
    then error $ printf "%s size 0x%X > specified 0x%X. ABORTED!" patchName patchLen size
    else do
      Bss.writeFile dstName $ Bss.concat [start, insertBlock, end]
      putStrLn $ printf "%s successfully written at 0x%X" patchName offset
      putStrLn $ printf "0x%X bytes are free" dummyLen

readDecodeTable :: String -> DecodeTable
readDecodeTable tblStr = buildDecodeTable parsedEntries
  where
    parsedEntries = parseTable tblStr
    buildDecodeTable :: [TableEntry] -> DecodeTable --sorting for longest collision case
    buildDecodeTable es = sortBy (flip $ comparing (length . str . snd)) tuples
      where
        tuples = map (\e -> (code e, e)) es --pull out code to fst in tuple for search

readEncodeTable :: String -> EncodeTable
readEncodeTable tblStr = buildEncodeTable parsedEntries
  where
    parsedEntries = parseTable tblStr
    buildEncodeTable :: [TableEntry] -> EncodeTable --sorting for longest collision case
    buildEncodeTable es = sortBy (flip $ comparing (length . str . snd)) tuples
      where
        tuples = map (\e -> (str e, e)) es--pull out string to fst in tuple for search

-- filter only unique messages and then replace pointers on the same string
-- with pointer on first found unique string
dropDuplicateMsgs :: [Bs.ByteString] -> ([Int64], [Bs.ByteString])
dropDuplicateMsgs msgs = (offsets, uniqueMsgs)
  where
    offsets = map (\m -> fromJust (lookup m uniqueLut)) msgs
    uniqueMsgs = nub msgs
    uniqueOffsets= init $ scanl (\acc bs -> acc + Bs.length bs) 0 uniqueMsgs
    uniqueLut = zip uniqueMsgs uniqueOffsets

-- split block with any endcode from given table
splitByEndCode :: EncodeTable -> [Word8] -> [[Word8]]
splitByEndCode tbl = (S.split. S.dropFinalBlank. S.keepDelimsR. S.oneOf) eoss
  where
    eoss = foldl getEos [] tbl
    getEos acc t = case t of (_, EndToken c _ _) -> c : acc; _ -> acc;

--helper parser for exactly 2-digit hex in script or table parser
hexnum2Dgt :: Parser Word8
hexnum2Dgt = do
  digits <- count 2 hexDigit
  return $  (fst . head) (readHex digits)

----------------------------PARSE BINARY----------------------------------------
decodeByTable :: DecodeTable -> [CharCode] -> String
decodeByTable tbl = go  --decode message from binary by table
  where
    go :: [Word8] -> String
    go [] = []
    go (x:xs) =
      let foundEntry = fromMaybe (error (printf "Char with code 0x%02X not \
                                        \found in table" x)) (lookup x tbl)
      in case foundEntry of
        NormalEntry _ s -> s ++ go xs
        EndToken _ s ns -> s ++"]" ++ replicate ns '\n' ++ go xs
        ControlCode _ s ns ps -> s ++ parStr ++"]" ++ replicate ns '\n'
                                    ++ go (drop (length ps) xs)
          where
            parStr = concatMap showParam $ zip ps xs
            showParam (par, val) = printf " %s=%02X" par val

addComments :: [String] -> [String] --add one-line comment to every line of each message
addComments = map prependComment
  where
    prependComment :: String -> String
    prependComment s = prependSlash s ++ s
    prependSlash st = unlines (map (\xs -> "//" ++ xs) $ lines st)

----------------------------PARSE SCRIPT----------------------------------------
{- Script is a plain text file, which can have raw hex bytes values: [$1F]
c-style single line comments are also allowed: //this is comment\n
parameter control codes will look like [item hi=AB lo=1B]
simple control and end codes are just [end] or [eol]
-}
encodeByTable :: EncodeTable -> String -> [Word8]
encodeByTable encodeTbl script = case parse (scriptEntriesParser encodeTbl eof)
                                  "Script parse failed" script of
                                    Left err -> error $ show err
                                    Right codes -> codes

scriptEntriesParser :: EncodeTable -> Parser() -> Parser [Word8] --parse multiple entries into binary
scriptEntriesParser encodeTbl endParser = concat <$> (many (newLines <|> comments) >> --first occurence of skipped data
                          choice [try (lexeme endToken), try (lexeme controlCode)
                                  , try (lexeme rawByte), lexeme normalEntry]
                                    `manyTill` endParser)
  where
    --skip newlines or single line comments
    lexeme p = p <* skipMany (newLines <|> comments)
    comments = void $ try $ string "//" >> anyChar `manyTill` newline
    newLines = void $ try newline

    tableMatch :: Parser TableEntry
    tableMatch = choice $ map keyValParser encodeTbl

    keyValParser :: (String, TableEntry) -> Parser TableEntry
    keyValParser (s,e) = try (string s) >> return e--get provided matching string code

    rawByte :: Parser [Word8]
    rawByte = do
      b <- between (string "[$") (char ']') hexnum2Dgt
      return [b]

    normalEntry :: Parser [Word8]
    normalEntry = do
      e <- tableMatch
      case e of
        NormalEntry c _ -> return [c]
        _ -> fail "Normal entry match not found"

    endToken :: Parser [Word8]
    endToken = do
      e <- tableMatch --control and endtoken are prepended with [ already
      _ <- char ']'
      case e of
        EndToken c _ _ -> return [c]
        _ -> fail "End token match not found"

    controlCode :: Parser [Word8]
    controlCode =  paramControl <* char ']'
      where
        paramControl =   do
          e <- tableMatch --control and endtoken are prepended with [ already
          case e of
            ControlCode c _ _ pars -> do
              ps <- parseParamsList pars
              return $ c : ps
            _ -> fail "Control code match not found"


    parseParamsList :: [String] -> Parser [Word8]
    parseParamsList [] = return []
    parseParamsList (p:ps) = do
      _ <- space >> string p >> char '='
      v <- hexnum2Dgt
      vs <- parseParamsList ps
      return $ v:vs

----------------------------PARSE TABLE-----------------------------------------
{- Table format is
3A=ABC
3B=Z
$ED=[eol]\n -control code
$12=[item],lo,hi -control code with parameters
$13=[param]\n\n,par1,par2
/E0=[end]\n\n -end code

on same codes collision, the longest sequence is taken
-}
parseTable :: String -> [TableEntry]
parseTable tbl =  case parse tableParser "Table parse failed" tbl of
      Left err -> error $ show err
      Right entries -> entries

tableParser :: Parser [TableEntry] --main parser for table file
tableParser = choice [endToken, controlCode, normalEntry] `manyTill` eof
  where
    lineEnd :: Parser ()
    lineEnd = void (try (string "\r\n")
              <|> try (string "\n")
              <|> try (string "\r"))
              <|> eof

    normalEntry :: Parser TableEntry --parse  01 = ABCD
    normalEntry = NormalEntry <$> (hexnum2Dgt <* char '=') <*> anyChar `manyTill` lineEnd

    countNls :: Parser Int --handles \n after special codes
    countNls = length <$> many (string "\\n")

    endToken :: Parser TableEntry --parse /E0=[end]\n\n
    endToken = EndToken <$> (char '/' *> hexnum2Dgt <* string "=[")
                        <*>  (("["++) <$> anyChar `manyTill` char ']')
                        <*> countNls <* lineEnd


    controlCode :: Parser TableEntry
    controlCode = ControlCode <$> (char '$' *> hexnum2Dgt <* string "=[")
                      <*> (("["++) <$> anyChar `manyTill` char ']')
                      <*> countNls
                      <*> many param <* lineEnd
      where
        param = char ','*> many1 (noneOf ",\n")
