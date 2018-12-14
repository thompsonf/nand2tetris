module VM.Types  where

data Logical = Add
  | Sub
  | Neg
  | Eq
  | Gt
  | Lt
  | And
  | Or
  | Not
  deriving Show

data Segment = Local
  | Argument
  | This
  | That
  | Constant
  | Static
  | Temp
  | Pointer
  deriving Show

data Memory = Pop Segment Int
  | Push Segment Int
  deriving Show

data Command = CL Logical | CM Memory
  deriving Show