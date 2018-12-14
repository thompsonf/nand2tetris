module VM.ToASM (toASM) where

import ASM.Pretty (prettyLine)
import VM.Pretty (prettyCommand)
import qualified ASM.Types as ASM
import VM.Types

-- toASMWithComment :: Command -> [ASM.Line]
-- toASMWithComment com = (ASM.LineE $ prettyCommand com):(toASM com)

-- Pushes value from D onto stack
push :: [String]
push =
  [ "@R0"
  , "A=M"
  , "M=D"
  , "@R0"
  , "M=M+1" ]

-- Pops value from stack and puts it in RAM[D]
pop :: [String]
pop =
  [ "@R13"
  , "M=D"
  , "@R0"
  , "AM=M-1"
  , "D=M"
  , "@R13"
  , "A=M"
  , "M=D" ]

-- sets D = RAM[A + int]
prePushPtr int = ["D=A", "@" ++ (show int), "A=D+A", "D=M"]

-- gets ASM label for push/pop static i
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

-- Sets D = A + int
prePopPtr int =
  [ "D=A"
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

toASM :: String -> Command -> [String]
-- Memory operations
toASM fname (CM (Push seg int)) = prePush fname seg int ++ push
toASM fname (CM (Pop seg int)) = prePop fname seg int ++ pop
-- Arithmetic operations
toASM _ (CL Add) = preBinary ++ ["D=D+M"] ++ push
toASM _ (CL Sub) = preBinary ++ ["D=D-M"] ++ push
toASM _ (CL Neg) = neg
-- Boolean operations. Assumes top item(s) of stack
-- are -1 (true) or 0 (false)
toASM _ (CL And) = preBinary ++ ["D=D&M"] ++ push
toASM _ (CL Or) = preBinary ++ ["D=D|M"] ++ push
toASM _ (CL Not) = not
-- Comparison operations
toASM _ (CL Eq) = []
toASM _ (CL Gt) = []
toASM _ (CL Lt) = []

neg = 
  [ "@R0"
  , "AM=M-1"
  , "M=-M"
  , "@R0"
  , "M=M+1" ]

not =
  [ "@R0"
  , "AM=M-1"
  , "M=!M"
  , "@R0"
  , "M=M+1" ]

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
preBinary =
  [ "@R0"
  , "AM=M-1" -- A points to top of stack, decrement SP
  , "D=M" -- D is first num to operate on
  , "@R13"
  , "M=D" -- RAM[13] holds first num to operate on
  , "@R0"
  , "AM=M-1"
  , "D=M" 
  , "@R13" ] -- D holds second num to operate on