import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import ML.Parse (parseFile)
import ML.Types
import System.Environment
import Text.Printf

main = do
  [input, output] <- getArgs
  source <- readFile input
  let ast = parseFile source
  let symMap = buildSymMap ast
  writeFile output $ unlines $ genCode symMap ast

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
  LineL label _ -> addLabels xs lineNum $ M.insert label lineNum curMap
  LineE _ -> addLabels xs lineNum curMap
  _ -> addLabels xs (lineNum + 1) curMap
addLabels _ _ curMap = curMap

addVars :: [Line] -> Int -> SymMap -> SymMap
addVars (x:xs) nextAddr curMap = case x of
  LineA (AInstSym var) _ -> case (M.member var curMap) of
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
  LineE _ -> rest
  LineL _ _ -> rest
  LineA aInst _ -> (genAInst symMap aInst) : rest
  LineC cInst _-> (genCInst cInst) : rest
  where rest = genCode symMap xs
genCode _ [] = []