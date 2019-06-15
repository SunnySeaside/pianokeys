import Text.ParserCombinators.Parsec
import Data.Char
import qualified Data.Map as Map
import Control.Monad
import Data.Maybe
import System.Environment
import System.Console.GetOpt

keyDefFile = (spaces'' *> keySyms <* comment) `sepEndBy` eol <* eof
keySyms = keySym `sepEndBy` spaces'

keySym :: GenParser Char st String
keySym = liftM (\x->[x]) digit <|>
  liftM2 (:) (noneOf "%~:{}0123456789\r\n\t ") (many $ noneOf " \r\n\t~:")

keyCodeFile = let list=(keyCodeGroup <* comment) `sepEndBy` (skipMany1 eol) <* eof in
  liftM (Map.fromList . concat) list

keyCodeGroup = do
  keyCode <- naturalNumber
  spaces'
  syms <- keySyms
  return $ zip syms [keyCode..]
--keyCodeGroup' = liftM2 (flip zip) (keySyms <* spaces') (liftM (\x->[x..]) naturalNumber)


data Note = C | C' | D | D' | E | F | F' | G | G' | A | A' | B deriving (Enum,Ord,Eq)
isWholeNote x = x `elem` [C,D,E,F,G,A,B]
--newtype Note = Nl Note' deriving (Enum)
instance Show Note where
  show x = case x of
             C -> "C"
             C' -> "C#"
             D -> "D"
             D' -> "D#"
             E -> "E"
             F -> "F"
             F' -> "F#"
             G -> "G"
             G' -> "G#"
             A -> "A"
             A' -> "A#"
             B -> "B"

data Pitch = P Note Int deriving (Eq)
instance Enum Pitch where
  fromEnum (P n oct) = fromEnum n + oct * 12
  toEnum x = P (toEnum idx) oct
    where (oct,idx) = divMod x 12
instance Show Pitch where
  show (P l oct) = show l ++ show oct

pitchName :: GenParser Char st Pitch
pitchName = do
  letter' <- oneOf "CDEFGAB"
  let letter = case letter' of
                 'C' -> C
                 'D' -> D
                 'E' -> E
                 'F' -> F
                 'G' -> G
                 'A' -> A
                 'B' -> B
  sharp <- optionMaybe $ char '#'
  octave <- naturalNumber --option
  case sharp of
    Nothing -> return $ P letter octave
    Just x -> return $ P (succ letter) octave

keyMapFile keyDefs = liftM concat blocks
  where blocks=(spaces'' *> keyMapBlock <* comment) `sepEndBy` (many1 eol) <* eof
        keyMapBlock = option [] (try keyMapSeq <|> keyMapEntry) --todo try is expensive
        keyMapEntry = do
          (keys,Just pitch) <- keyWithPitch
          iter <- seqType
          return $ keyMapSanitize (keyMapFrom keys (iter pitch))
        keyWithPitch = liftM2 (,) keySymRange (optionMaybe $ spaces' *> pitchName)
        keyMapSeq = do
          string "seq"
          st <- seqType
          char '{'
          spaces
          entries <- (spaces'' *> keyWithPitch <* comment) `sepEndBy` (skipMany1 eol)
          char '}'
          let (pre,pivot:rest) = break (isJust . snd) entries --TODO:detect multiple pitches
          let pitch = fromJust $ snd pivot
          let preKeys = concat $ map fst pre
          let preMap' = scanr (\k (_,p) -> (k,predPitch p)) (head $ fst pivot,st pitch) preKeys
          let preMap = [(x,toPitch y) | (x,y) <- preMap']
          let _:restMap = keyMapFrom (concat $ map fst (pivot:rest)) (st pitch)
          return $ keyMapSanitize (preMap++restMap)
        keySymRange = do
          start <- keySym
          sep <- optionMaybe $ char '~'
          case sep of
            Nothing -> return [start]
            Just x -> liftM (keySymRange_lookup start) keySym
-- TODO: optimize performance; maybe use map?
        keySymRange_lookup start end =
          let found=filter (not . null) (map (getSubSeq start end) keyDefs) in
            if length found == 1 then
              head found
            else
              error $ "multiple or none key sequences match "++start++"~"++end

getSubSeq start end fullseq =
  let t=dropWhile (/= start) fullseq in
    foldr (\i l -> if i==end then [end]
                   else if null l then []
                   else i:l)
          [] t
keyMapFrom keys iter = zip keys (map toPitch (pitchFrom iter))
keyMapSanitize m = [(x,y) | (x,Just y) <- m] --catMaybes

-- TODO support different keys
data PitchIter = WholeI Pitch | SemiI Pitch --todo add more
defaultIter (P note oct) = (if isWholeNote note then WholeI else SemiI) (P note oct)
pitchFrom i = iterate succPitch i
--pitchFrom i = i : pitchFrom (succPitch i)

toPitch (WholeI p) = Just p
toPitch (SemiI (P note oct)) = if isWholeNote note then Nothing else Just (P note oct)

succPitch (WholeI (P B oct)) = WholeI (P C (oct+1))
succPitch (WholeI (P E oct)) = WholeI (P F oct)
succPitch (SemiI (P C oct)) = SemiI (P C' oct)
succPitch (SemiI (P F oct)) = SemiI (P F' oct)
succPitch (SemiI (P A' oct)) = SemiI (P C (oct+1))
succPitch (WholeI (P n oct)) = WholeI (P (succ $ succ n) oct)
succPitch (SemiI (P n oct)) = SemiI (P (succ $ succ n) oct)

predPitch (WholeI (P C oct)) = WholeI (P B (oct-1))
predPitch (WholeI (P F oct)) = WholeI (P E oct)
predPitch (SemiI (P C' oct)) = SemiI (P C oct)
predPitch (SemiI (P F' oct)) = SemiI (P F oct)
predPitch (SemiI (P C oct)) = SemiI (P A' (oct-1))
predPitch (WholeI (P n oct)) = WholeI (P (pred $ pred n) oct)
predPitch (SemiI (P n oct)) = SemiI (P (pred $ pred n) oct)

seqType :: GenParser Char st (Pitch -> PitchIter)
seqType = do
  iterName <- optionMaybe (char ':' *> many1 letter)
  case iterName of
    Nothing -> return defaultIter
    Just "default" -> return defaultIter
    x -> fail "unknown iterator name"

--https://stackoverflow.com/questions/10726085/how-do-i-get-parsec-to-let-me-call-read-int/10726784#10726784
naturalNumber :: GenParser Char st Int
naturalNumber =  foldl (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

spaces' = skipMany1 $ oneOf " \t"
spaces'' = skipMany $ oneOf " \t"
eol = try(string "\r\n") <|> string "\n" <|> string "\r"
comment = optional $ spaces'' >> char '%' >> many (noneOf "\r\n")

--TODO: use proper XML library
keyMapToXml keyCodes m = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
  "<!DOCTYPE rawkeyboardmap>\n"++
  "<rawkeymap version=\"1.0\">\n"++
  concat["<mapping keycode=\""++(show i)++"\" note=\""++(show j)++"\"/>\n" | (i,j) <- nums]++
  "</rawkeymap>\n"
  where nums=[(fromJust (Map.lookup key keyCodes),fromEnum pitch) | (key,pitch) <- m]



data Flag = Help | Keycodes String | Keydef String | Outfile String deriving (Eq)
main = do
  argv0 <- getProgName
  argv <- getArgs
  let optDescr=[Option "h?" ["help"] (NoArg Help) "display help",
                Option "c" ["keycodes"] (ReqArg Keycodes "FILE") "path of keycode file",
                Option "d" ["keydef"] (ReqArg Keydef "FILE") "path of keyboard definition file",
                Option "o" ["output"] (ReqArg Outfile "FILE") "path of output keymap"]
  let (opts,args,errs) = getOpt Permute optDescr argv
  if not (null errs) then do
    fail $ concat errs
  else if Help `elem` opts then do
    putStrLn $ usageInfo ("Usage: "++argv0++" [OPTIONS..] [inputFile]") optDescr
  else do
    codeFile <- case [x | Keycodes x <- opts] of --todo: is the optimial?
                 [x] -> return x
                 [] -> return "linux.keycodes"
                 x -> fail "only one keycode file may be specified"
    keyCodes <- parseFileOrErr keyCodeFile codeFile
    defFile <- case [x | Keydef x <- opts] of
                 [x] -> return x
                 [] -> return "laptop.kbd"
                 x -> fail "only one keyboard definition file may be specified"
    keyDefs <- parseFileOrErr keyDefFile defFile
    keyMap <- case args of
                [x] -> parseFileOrErr (keyMapFile keyDefs) x
                [] -> fail "no input keymap specified" --TODO: stdin
                x -> fail "only one input keymap may be specified"
    outFunc <- case [x | Outfile x <- opts] of
                 [x] -> return (writeFile x)
                 [] -> return (writeFile "out.xml") --TODO: stdout or based on fileName
                 x -> fail "only one output file may be specified"
-- let (if null args then getContent <stdin>
-- outFunc <-
-- stdout writeFile readFile
    outFunc $ keyMapToXml keyCodes keyMap

parseFileOrErr parser file = do
  res <- parseFromFile parser file
  case res of
    Left x -> fail $ show x
    Right x -> return x
