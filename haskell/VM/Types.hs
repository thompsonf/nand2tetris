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

data Flow = Label String
  | Goto String
  | IfGoto String
  deriving Show

data Function = Fun String Int
  | Call String Int
  | Return
  deriving Show

data Command = CL Logical
  | CM Memory
  | CF Flow
  | CFun Function
  deriving Show