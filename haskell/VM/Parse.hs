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
parseSegment "temp" = Just Temp
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

parseFlow :: String -> Maybe Flow
parseFlow str = case (words str) of
  ("label":lbl:[]) -> Just Label lbl
  ("if-goto":lbl:[]) -> Just IfGoto lbl
  ("goto":lbl:[]) -> Just Goto lbl
  _ -> Nothing

parseFunction :: String -> Maybe Function
parseFunction str = case (words str) of
  ("function":fname:nLocals:[]) -> case (readMaybe nLocals :: Maybe Int) of
    Just n -> Just Fun fname n
    _ -> Nothing
  ("call":fname:nArgs:[]) -> case (readMaybe nArgs :: Maybe Int) of
    Just n -> Just Call fname n
    _ -> Nothing
  ("return":[]) -> Just Return
  _ -> Nothing

parseLine :: String -> Maybe Command
parseLine l = case (parseLogical $ filter (not . isSpace) l, parseMemory l, parseFlow l, parseFunction l) of
  (Just log, _, _, _) -> Just $ CL log
  (_, Just mem, _, _) -> Just $ CM mem
  (_, _, Just flow, _) -> Just $ CF flow
  (_, _, _, Just func) -> Just $ CFunc func
  _ -> Nothing

parseLineOrError :: (Int, String) -> Command
parseLineOrError (lineNum, s) = case parseLine s of
  Just line -> line
  Nothing -> error $ "Failed to parse line " ++ show lineNum ++ ": " ++ s

removeComment :: String -> String
removeComment "" = ""
removeComment ('/':'/':xs) = ""
removeComment (x:xs) = x:(removeComment xs)

cleanLines :: [String] -> [String]
cleanLines xs = filter (/= "") (map removeComment xs)

parseFile :: String -> [Command]
parseFile source = map parseLineOrError $ zip [0..] (cleanLines (lines source))