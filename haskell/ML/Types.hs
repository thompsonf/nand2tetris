module ML.Types 
( AInst (..)
, Dest (..)
, Comp (..)
, Jump (..)
, CInst (..)
, Line (..)
) where

data AInst = AInstSym String | AInstNum Int deriving Show

data Dest = NullDest
  | DestA
  | DestD
  | DestM
  | AD
  | AM
  | DM
  | ADM
  deriving Show

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

data Jump = NullJump
  | JGT
  | JEQ
  | JGE
  | JLT
  | JNE
  | JLE
  | JMP
  deriving Show

data CInst = CInst Dest Comp Jump deriving Show

-- Last String arg is comment
data Line = LineE String
  | LineL String String
  | LineA AInst String
  | LineC CInst String
  deriving Show