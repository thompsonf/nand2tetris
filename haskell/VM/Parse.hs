module VM.Parse 
( parseLine
, parseLineOrError
, parseFile
) where

import Data.Char (isSpace)
import Data.Maybe
import Text.Read (readMaybe)
import VM.Types

parseLogical :: String -> Maybe Logical
parseLogical "add" = Just Add
parseLogical "sub" = Just Sub
parseLogical "neg" = Just Neg
parseLogical "eq" = Just Eq
parseLogical "gt" = Just Gt
parseLogical "lt" = Just Lt
parseLogical "and" = Just And
parseLogical "or" = Just Or
parseLogical "not" = Just Not
parseLogical _ = Nothing

parsePushPop :: String -> Maybe (Segment -> Int -> Memory)
parsePushPop "push" = Just Push
parsePushPop "pop" = Just Pop
parsePushPop _ = Nothing

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
parseMemory str = parseMemHelper (words str)

parseMemHelper :: [String] -> Maybe Memory
parseMemHelper (one:two:three:[]) = case (mPP, mSeg, mInt) of
  (Just pp, Just seg, Just int) -> Just (pp seg int)
  _ -> Nothing
  where
    mPP = parsePushPop one
    mSeg = parseSegment two
    mInt = readMaybe three :: Maybe Int
parseMemHelper _ = Nothing

parseLine :: String -> Maybe Command
parseLine l = case (parseLogical $ filter (not . isSpace) l, parseMemory l) of
  (Just log, _) -> Just $ CL log
  (_, Just mem) -> Just $ CM mem
  _ -> Nothing

parseLineOrError :: (Int, String) -> Command
parseLineOrError (lineNum, s) = case parseLine s of
  Just line -> line
  Nothing -> error $ "Failed to parse line " ++ show lineNum ++ ": " ++ s

parseFile :: String -> [Command]
parseFile source = map parseLineOrError $ zip [0..] (lines source)