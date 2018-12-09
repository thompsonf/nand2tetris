import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import System.Environment
import Text.Printf
import Text.Read

main = do
  [input, output] <- getArgs
  source <- readFile input
  let ast = parse source
  let symMap = buildSymMap ast
  writeFile output $ unlines $ genCode symMap ast

-- SYMBOL

parseSym :: String -> Maybe String
parseSym str@(x:xs) = if (isValidFirst x && isValidRest xs) then Just str else Nothing
  where isValidFirst c = c == '_' || c == '.' || c == '$' || c == ':' || isLetter c
        isValidRest str = case str of
          (x:xs) -> (isDigit x || isValidFirst x) && isValidRest xs
          "" -> True

-- LABELS

removeParensHelper :: String -> Maybe String
removeParensHelper ")" = Just ""
removeParensHelper (x:xs) = case removeParensHelper xs of
  Just rest -> Just (x:rest)
  Nothing -> Nothing
removeParensHelper _ = Nothing

removeParens :: String -> Maybe String
removeParens ('(':xs) = removeParensHelper xs
removeParens _ = Nothing

parseLabel :: String -> Maybe String
parseLabel str = case (removeParens str) of
  Just s -> parseSym s
  Nothing -> Nothing

-- A INSTRUCTIONS

data AInst = AInstSym String | AInstNum Int deriving Show

parseAInst :: String -> Maybe AInst
parseAInst ('@':xs) = case (parseSym xs, readMaybe xs :: Maybe Int) of
  (Just s, _) -> Just $ AInstSym s
  (_, Just n) -> Just $ AInstNum n
  _ -> Nothing
parseAInst _ = Nothing

-- C INSTRUCTIONS

data Dest = NullDest
  | DestA
  | DestD
  | DestM
  | AD
  | AM
  | DM
  | ADM
  deriving Show

parseDest :: String -> Maybe Dest
parseDest "" = Just NullDest
parseDest "A" = Just DestA
parseDest "D" = Just DestD
parseDest "M" = Just DestM
parseDest "AD" = Just AD
parseDest "AM" = Just AM
parseDest "DA" = Just AD
parseDest "DM" = Just DM
parseDest "MA" = Just AM
parseDest "MD" = Just DM
parseDest "ADM" = Just ADM
parseDest "AMD" = Just ADM
parseDest "DAM" = Just ADM
parseDest "DMA" = Just ADM
parseDest "MAD" = Just ADM
parseDest "MDA" = Just ADM
parseDest _ = Nothing

data Comp = Zero
  | One
  | NegOne
  | A
  | D
  | M
  | NotA
  | NotD
  | NotM
  | NegA
  | NegD
  | NegM
  | APlusOne
  | DPlusOne
  | MPlusOne
  | AMinusOne
  | DMinusOne
  | MMinusOne
  | APlusD
  | DPlusM
  | AMinusD
  | DMinusA
  | DMinusM
  | MMinusD
  | AAndD
  | DAndM
  | DOrA
  | DOrM
  deriving Show

parseComp :: String -> Maybe Comp
parseComp "0" = Just Zero
parseComp "1" = Just One
parseComp "-1" = Just NegOne
parseComp "A" = Just A
parseComp "D" = Just D
parseComp "M" = Just M
parseComp "!A" = Just NotA
parseComp "!D" = Just NotD
parseComp "!M" = Just NotM
parseComp "-A" = Just NegA
parseComp "-D" = Just NegD
parseComp "-M" = Just NegM
parseComp "1+A" = Just APlusOne
parseComp "A+1" = Just APlusOne
parseComp "1+D" = Just DPlusOne
parseComp "D+1" = Just DPlusOne
parseComp "1+M" = Just MPlusOne
parseComp "M+1" = Just MPlusOne
parseComp "A-1" = Just AMinusOne
parseComp "D-1" = Just DMinusOne
parseComp "M-1" = Just MMinusOne
parseComp "A+D" = Just APlusD
parseComp "D+A" = Just APlusD
parseComp "D+M" = Just DPlusM
parseComp "M+D" = Just DPlusM
parseComp "A-D" = Just AMinusD
parseComp "D-A" = Just DMinusA
parseComp "D-M" = Just DMinusM
parseComp "M-D" = Just MMinusD
parseComp "A&D" = Just AAndD
parseComp "D&A" = Just AAndD
parseComp "D&M" = Just DAndM
parseComp "M&D" = Just DAndM
parseComp "A|D" = Just DOrA
parseComp "D|A" = Just DOrA
parseComp "D|M" = Just DOrM
parseComp "M|D" = Just DOrM
parseComp _ = Nothing

data Jump = NullJump
  | JGT
  | JEQ
  | JGE
  | JLT
  | JNE
  | JLE
  | JMP
  deriving Show

parseJump :: String -> Maybe Jump
parseJump "" = Just NullJump
parseJump "JGT" = Just JGT
parseJump "JEQ" = Just JEQ
parseJump "JGE" = Just JGE
parseJump "JLT" = Just JLT
parseJump "JNE" = Just JNE
parseJump "JLE" = Just JLE
parseJump "JMP" = Just JMP
parseJump _ = Nothing

splitCInst :: String -> (String, String, String)
splitCInst inst = (dest, comp, jump)
  where
    eqIndex = elemIndex '=' inst
    (dest, rest) = case eqIndex of Just index -> (take index inst, drop (index + 1) inst)
                                   Nothing -> ("", inst)
    semIndex = elemIndex ';' rest
    (comp, jump) = case semIndex of Just index -> (take index rest, drop (index + 1) rest)
                                    Nothing -> (rest, "")

data CInst = CInst Dest Comp Jump deriving Show

parseCInst :: String -> Maybe CInst
parseCInst inst = case (parseDest d, parseComp c, parseJump j) of
  (Just d, Just c, Just j) -> Just $ CInst d c j
  _ -> Nothing
  where (d, c, j) = splitCInst inst

-- CLEAN INPUT

stripComment :: String -> String
stripComment "" = ""
stripComment ('/':'/':xs) = ""
stripComment (x:xs) = x : stripComment xs

stripWhitespace :: String -> String
stripWhitespace s = filter (not . isSpace) s

getLines :: String -> [String]
getLines source = filter (/= "") cleanLines
  where
    ls = lines source
    cleanLines = map (stripComment . stripWhitespace) ls

-- CONSTRUCT AST

data Line = LineL String | LineA AInst | LineC CInst deriving Show

parseLine :: String -> Maybe Line
parseLine l = case (parseLabel l, parseAInst l, parseCInst l) of
  (Just label, _, _) -> Just $ LineL label
  (_, Just ainst, _) -> Just $ LineA ainst
  (_, _, Just cinst) -> Just $ LineC cinst
  _ -> Nothing

parseLineOrError :: (Int, String) -> Line
parseLineOrError (lineNum, s) = case parseLine s of
  Just line -> line
  Nothing -> error $ "Failed to parse line " ++ show lineNum ++ ": " ++ s

parse :: String -> [Line]
parse source = map parseLineOrError $ zip [0..] (getLines source)

-- CONSTRUCTING SYMBOL MAP

type SymMap = M.Map String Int

initialMap ::SymMap
initialMap = M.fromList [("SP", 0),
                       ("LCL", 1),
                       ("ARG", 2),
                       ("THIS", 3),
                       ("THAT", 4),
                       ("R0", 0),
                       ("R1", 1),
                       ("R2", 2),
                       ("R3", 3),
                       ("R4", 4),
                       ("R5", 5),
                       ("R6", 6),
                       ("R7", 7),
                       ("R8", 8),
                       ("R9", 9),
                       ("R10", 10),
                       ("R11", 11),
                       ("R12", 12),
                       ("R13", 13),
                       ("R14", 14),
                       ("R15", 15),
                       ("SCREEN", 16384),
                       ("KBD", 24576)]

addLabels :: [Line] -> Int -> SymMap -> SymMap
addLabels (x:xs) lineNum curMap = case x of
  LineL label -> addLabels xs lineNum $ M.insert label lineNum curMap
  _ -> addLabels xs (lineNum + 1) curMap
addLabels _ _ curMap = curMap

addVars :: [Line] -> Int -> SymMap -> SymMap
addVars (x:xs) nextAddr curMap = case x of
  LineA (AInstSym var) -> case (M.member var curMap) of
    True -> addVars xs nextAddr curMap
    False -> addVars xs (nextAddr + 1) (M.insert var nextAddr curMap)
  _ -> addVars xs nextAddr curMap
addVars _ _ curMap = curMap

buildSymMap :: [Line] -> SymMap
buildSymMap lines = withVars
  where withLabels = addLabels lines 0 initialMap
        withVars = addVars lines 16 withLabels

-- CODE GEN

genAInst :: SymMap -> AInst -> String
genAInst _ (AInstNum num) = printf "%016b" num
genAInst symMap (AInstSym sym) = printf "%016b" $ symMap M.! sym

genDest :: Dest -> String
genDest NullDest = "000"
genDest DestA = "100"
genDest DestD = "010"
genDest DestM = "001"
genDest AD = "110"
genDest AM = "101"
genDest DM = "011"
genDest ADM = "111"

genComp :: Comp -> String
genComp Zero = "0101010"
genComp One = "0111111"
genComp NegOne = "0111010"
genComp A = "0110000"
genComp D = "0001100"
genComp M = "1110000"
genComp NotA = "0110001"
genComp NotD = "0001101"
genComp NotM = "1110001"
genComp NegA = "0110011"
genComp NegD = "0001111"
genComp NegM = "1110011"
genComp APlusOne = "0110111"
genComp DPlusOne = "0011111"
genComp MPlusOne = "1110111"
genComp AMinusOne = "0110010"
genComp DMinusOne = "0001110"
genComp MMinusOne = "1110010"
genComp APlusD = "0000010"
genComp DPlusM = "1000010"
genComp AMinusD = "0000111"
genComp DMinusA = "0010011"
genComp DMinusM = "1010011"
genComp MMinusD = "1000111"
genComp AAndD = "0000000"
genComp DAndM = "1000000"
genComp DOrA = "0010101"
genComp DOrM = "1010101"

genJump :: Jump -> String
genJump NullJump = "000"
genJump JGT = "001"
genJump JEQ = "010"
genJump JGE = "011"
genJump JLT = "100"
genJump JNE = "101"
genJump JLE = "110"
genJump JMP = "111"

genCInst :: CInst -> String
genCInst (CInst dest comp jump) = "111" ++ (genComp comp) ++ (genDest dest) ++ (genJump jump)

genCode :: SymMap -> [Line] -> [String]
genCode symMap (x:xs) = case x of
  LineL _ -> rest
  LineA aInst -> (genAInst symMap aInst) : rest
  LineC cInst -> (genCInst cInst) : rest
  where rest = genCode symMap xs
genCode _ [] = []