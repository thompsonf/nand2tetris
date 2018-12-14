module VM.Types 
( Segment (..)
, 
) where

data Logical = Add
  | Sub
  | Neg
  | Eq
  | Gt
  | Lt
  | And
  | Or
  | Not

data Segment = Local
  | Argument
  | This
  | That
  | Constant
  | Static
  | Temp
  | Pointer

data Memory = Pop Segment Int
  | Push Segment Int

data Command = CL Logical | CM Memory

parseSegment :: String -> Segment
parseSegment "local" = Local
parseSegment "argument" = Argument
parseSegment "this" = This
parseSegment "that" = That
parseSegment "constant" = Constant
parseSegment "static" = Static
parseSegment "pointer" = Pointer

parseCommand :: String ->