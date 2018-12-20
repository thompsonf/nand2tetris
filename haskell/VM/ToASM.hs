{-# LANGUAGE NamedFieldPuns #-}

module VM.ToASM (bootstrap, genCode, toASM) where

import ASM.Pretty (prettyLine)
import VM.Pretty (prettyCommand)
import qualified ASM.Types as ASM
import VM.Types

-- Pushes value from D onto stack
push :: [String]
push =
  [ "@SP"
  , "A=M"
  , "M=D"
  , "@SP"
  , "M=M+1" ]

-- Pops value from stack and puts it in RAM[D]
pop :: [String]
pop =
  [ "@R13"
  , "M=D"
  , "@SP"
  , "AM=M-1"
  , "D=M"
  , "@R13"
  , "A=M"
  , "M=D" ]

-- sets D = RAM[A + int]
prePushPtr :: Int -> [String]
prePushPtr int = ["D=M", "@" ++ (show int), "A=D+A", "D=M"]

-- gets ASM label for push/pop static i
static :: String -> Int -> String
static fname int = "@" ++ fname ++ "." ++ (show int)

-- Set D to the appropriate value
prePush :: String -> Segment -> Int -> [String]
prePush fname Static int = [static fname int, "D=M"]
prePush _ Local int = ["@R1"] ++ prePushPtr int
prePush _ Argument int = ["@R2"] ++ prePushPtr int
prePush _ This int = ["@R3"] ++ prePushPtr int
prePush _ That int = ["@R4"] ++ prePushPtr int
prePush _ Constant int = ["@" ++ (show int), "D=A"]
prePush _ Temp int = ["@" ++ (show $ 5 + int), "D=M"]
prePush _ Pointer 0 = ["@R3", "D=M"]
prePush _ Pointer 1 = ["@R4", "D=M"]

-- Sets D = M + int
prePopPtr :: Int -> [String]
prePopPtr int =
  [ "D=M"
  , "@" ++ (show int)
  , "D=D+A" ]

-- Sets D to the address that should be popped into
prePop :: String -> Segment -> Int -> [String]
prePop fname Static int = [static fname int, "D=A"]
prePop _ Local int = ["@R1"] ++ prePopPtr int
prePop _ Argument int = ["@R2"] ++ prePopPtr int
prePop _ This int = ["@R3"] ++ prePopPtr int
prePop _ That int = ["@R4"] ++ prePopPtr int
prePop _ Temp int = ["@" ++ (show $ 5 + int), "D=A"]
prePop _ Pointer 0 = ["@R3", "D=A"]
prePop _ Pointer 1 = ["@R4", "D=A"]

data Context = Context
  { file :: String
  , lineNum :: Int
  , func :: String
  , nextCall :: Int }
  deriving Show

toASM :: Context -> Command -> [String]
-- Memory operations
toASM Context{file} (CM (Push seg int)) = prePush file seg int ++ push
toASM Context{file} (CM (Pop seg int)) = prePop file seg int ++ pop
-- Arithmetic operations
toASM _ (CL Add) = preBinary ++ ["D=D+M"] ++ push
toASM _ (CL Sub) = preBinary ++ ["D=D-M"] ++ push
toASM _ (CL Neg) = neg
-- Boolean operations. Assumes top item(s) of stack
-- are -1 (true) or 0 (false)
toASM _ (CL And) = preBinary ++ ["D=D&M"] ++ push
toASM _ (CL Or) = preBinary ++ ["D=D|M"] ++ push
toASM _ (CL Not) = myNot
-- Comparison operations
toASM Context{file, lineNum} (CL Eq) = eq (getUniq file lineNum)
toASM Context{file, lineNum} (CL Gt) = gt (getUniq file lineNum)
toASM Context{file, lineNum} (CL Lt) = lt (getUniq file lineNum)
-- Flow operations
toASM Context{file, lineNum, func=f} (CF (Label lbl)) =
  ["(" ++ getLbl f lbl ++ ")"]
toASM Context{file, func=f} (CF (Goto lbl)) =
  [ "@" ++ getLbl f lbl
  , "0;JMP" ]
toASM Context{func=f} (CF (IfGoto lbl)) =
  [ "@SP"
  , "AM=M-1"
  , "D=M"
  , "@" ++ getLbl f lbl
  , "D;JNE" ]
-- Function operations
toASM Context{file} (CFun (Fun func nLocals)) =
  [ "(" ++ func ++ ")"] ++ pushZeroes nLocals
toASM Context{func=f, nextCall} (CFun (Call func nArgs)) = 
  -- push return address
  [ "@" ++ lbl, "D=A" ] ++ push ++
  -- push other addresses
  [ "@LCL", "D=M" ] ++ push ++
  [ "@ARG", "D=M" ] ++ push ++
  [ "@THIS", "D=M" ] ++ push ++
  [ "@THAT", "D=M" ] ++ push ++
  -- set ARG = SP - nArgs - 5
  [ "@SP"
  , "D=M"
  , "@" ++ show (nArgs + 5)
  , "D=D-A"
  , "@ARG"
  , "M=D"
  -- set LCL = SP
  , "@SP"
  , "D=M"
  , "@LCL"
  , "M=D"
  -- jump to the function's code
  , "@" ++ func
  , "0;JMP"
  -- function jumps back here after it's done
  , "(" ++ lbl ++ ")" ]
  where lbl = f ++ "$ret." ++ show nextCall
toASM _ (CFun Return) =
  -- store base ptr of frame in R13 (FRAME)
  [ "@LCL"
  , "D=M"
  , "@R13"
  , "M=D"
  -- store return address in R14
  , "@5"
  , "A=D-A"
  , "D=M"
  , "@R14"
  , "M=D"
  -- store return value in ARG
  , "@SP"
  , "A=M-1"
  , "D=M"
  , "@ARG"
  , "A=M"
  , "M=D"
  -- set SP to ARG+1
  , "@ARG"
  , "D=M"
  , "@SP"
  , "M=D+1"
  -- set THAT to RAM[FRAME-1]
  , "@R13"
  , "AM=M-1"
  , "D=M"
  , "@THAT"
  , "M=D"
  -- set THIS to RAM[FRAME-2]
  , "@R13"
  , "AM=M-1"
  , "D=M"
  , "@THIS"
  , "M=D"
  -- set ARG to RAM[FRAME-3]
  , "@R13"
  , "AM=M-1"
  , "D=M"
  , "@ARG"
  , "M=D"
  -- set LCL to RAM[FRAME-4]
  , "@R13"
  , "AM=M-1"
  , "D=M"
  , "@LCL"
  , "M=D"
  -- go back to the calling function
  , "@R14"
  , "A=M"
  , "0;JMP" ]

pushZeroes :: Int -> [String]
pushZeroes 0 = []
pushZeroes nLocals =
  [ "@SP", "A=M" ] ++
  concat (replicate nLocals [ "M=0", "AD=A+1" ]) ++
  [ "@SP", "M=D" ]

getLbl :: String -> String -> String
getLbl func lbl = func ++ "$" ++ lbl

getUniq :: String -> Int -> String
getUniq fname lineNum = fname ++ (show lineNum) ++ "__"

neg :: [String]
neg = 
  [ "@SP"
  , "AM=M-1"
  , "M=-M"
  , "@SP"
  , "M=M+1" ]

myNot :: [String]
myNot =
  [ "@SP"
  , "AM=M-1"
  , "M=!M"
  , "@SP"
  , "M=M+1" ]

pushTOrF :: String -> [String]
pushTOrF uniq =
  [ "@SP"
  , "A=M"
  , "M=0;"
  , "@" ++ uniq ++ "END"
  , "0;JMP"
  , "(" ++ uniq ++ "TRUE)"
  , "@SP"
  , "A=M"
  , "M=-1"
  , "(" ++ uniq ++ "END)"
  , "@SP"
  , "M=M+1" ]

atTrue :: String -> String
atTrue uniq = "@" ++ uniq ++ "TRUE"

eq :: String -> [String]
eq uniq = preBinary ++ ["D=D-M", atTrue uniq, "D;JEQ"] ++ pushTOrF uniq

gt :: String -> [String]
gt uniq = preBinary ++ ["D=D-M", atTrue uniq, "D;JGT"] ++ pushTOrF uniq

lt :: String -> [String]
lt uniq = preBinary ++ ["D=D-M", atTrue uniq, "D;JLT"] ++ pushTOrF uniq

-- Sets
-- * A = 13
-- * M = RAM[13] = value of top of stack
-- * D = value of second in stack
-- And updates SP to point to second num in stack
-- E.g. if stack is [9, 15, 1, 2, 3]
--                   4  3   2  1  0
-- * A = 13
-- * M = RAM[13] = 9
-- * D = 15
-- * SP = RAM[0] = 3
preBinary :: [String]
preBinary =
  [ "@SP"
  , "AM=M-1" -- A points to top of stack, decrement SP
  , "D=M" -- D is first num to operate on
  , "@R13"
  , "M=D" -- RAM[13] holds first num to operate on
  , "@SP"
  , "AM=M-1"
  , "D=M" 
  , "@R13" ] -- D holds second num to operate on

bootstrap :: [String]
bootstrap =
  -- initialize SP
  [ "@256"
  , "D=A"
  , "@SP"
  , "M=D" ] ++
  toASM Context{file="", func="BOOTSTRAP", lineNum=0, nextCall=0} (CFun (Call "Sys.init" 0))

genCode :: String -> [Command] -> [String]
genCode file commands = genCodeHelper context commands
  where context = Context{file=file, lineNum=0, func="BOOTSTRAP", nextCall=0}

genCodeHelper :: Context -> [Command] -> [String]
genCodeHelper _ [] = []
genCodeHelper context@Context{file, lineNum, func, nextCall} (c:cs) = (("//" ++ show c):asm) ++ rest
  where
    asm = toASM context c
    newNextCall = case c of
      (CFun (Call _ _)) -> nextCall + 1
      _ -> nextCall
    newFunc = case c of
      (CFun (Fun fname _)) -> fname
      _ -> func
    newLineNum = lineNum + 1
    newContext = Context{file=file, lineNum=newLineNum, func=newFunc, nextCall=newNextCall}
    rest = genCodeHelper newContext cs
