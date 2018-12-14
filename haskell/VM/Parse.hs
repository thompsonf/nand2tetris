module VM.Parse where

import Data.Maybe
import VM.Types

parseLogical :: String -> Logical
parseLogical "add" = Just Add
parseLogical "sub" = Just Sub
parseLogical "neg" = Just Neg
parseLogical "eq" = Just Eq
parseLogical "gt" = Just Gt
parseLogical "lt" = Just Lt
parseLogical "and" = Just And
parseLogical "or" = Just Or
parseLogical "not" = Just Not

parseSegment :: String -> Maybe Segment
parseSegment "local" = Just Local
parseSegment "argument" = Just Argument
parseSegment "this" = Just This
parseSegment "that" = Just That
parseSegment "constant" = Just Constant
parseSegment "static" = Just Static
parseSegment "pointer" = Just Pointer
parseSegment _ = Nothing

parseMemory :: String -> Maybe Memory
parseMemory _ = Nothing