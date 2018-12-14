module ASM.Parse
( parseLine
, parseLineOrError
, parseFile
) where

import Data.Char
import Data.List
import Data.Maybe
import ASM.Types
import Text.Read (readMaybe)

-- SYMBOL

parseSym :: String -> Maybe String
parseSym str@(x:xs) = if (isValidFirst x && isValidRest xs) then Just str else Nothing
  where isValidFirst c = c == '_' || c == '.' || c == '$' || c == ':' || isLetter c
        isValidRest str = case str of
          (x:xs) -> (isDigit x || isValidFirst x) && isValidRest xs
          "" -> True

-- LABELS

removeParensHelper :: String -> Maybe String
removeParensHelper ")" = Just ""
removeParensHelper (x:xs) = case removeParensHelper xs of
  Just rest -> Just (x:rest)
  Nothing -> Nothing
removeParensHelper _ = Nothing

removeParens :: String -> Maybe String
removeParens ('(':xs) = removeParensHelper xs
removeParens _ = Nothing

parseLabel :: String -> Maybe String
parseLabel str = case (removeParens str) of
  Just s -> parseSym s
  Nothing -> Nothing

-- A INSTRUCTIONS

parseAInst :: String -> Maybe AInst
parseAInst ('@':xs) = case (parseSym xs, readMaybe xs :: Maybe Int) of
  (Just s, _) -> Just $ AInstSym s
  (_, Just n) -> Just $ AInstNum n
  _ -> Nothing
parseAInst _ = Nothing

-- C INSTRUCTIONS

parseDest :: String -> Maybe Dest
parseDest "" = Just NullDest
parseDest "A" = Just DestA
parseDest "D" = Just DestD
parseDest "M" = Just DestM
parseDest "AD" = Just AD
parseDest "AM" = Just AM
parseDest "DA" = Just AD
parseDest "DM" = Just DM
parseDest "MA" = Just AM
parseDest "MD" = Just DM
parseDest "ADM" = Just ADM
parseDest "AMD" = Just ADM
parseDest "DAM" = Just ADM
parseDest "DMA" = Just ADM
parseDest "MAD" = Just ADM
parseDest "MDA" = Just ADM
parseDest _ = Nothing

parseComp :: String -> Maybe Comp
parseComp "0" = Just Zero
parseComp "1" = Just One
parseComp "-1" = Just NegOne
parseComp "A" = Just A
parseComp "D" = Just D
parseComp "M" = Just M
parseComp "!A" = Just NotA
parseComp "!D" = Just NotD
parseComp "!M" = Just NotM
parseComp "-A" = Just NegA
parseComp "-D" = Just NegD
parseComp "-M" = Just NegM
parseComp "1+A" = Just APlusOne
parseComp "A+1" = Just APlusOne
parseComp "1+D" = Just DPlusOne
parseComp "D+1" = Just DPlusOne
parseComp "1+M" = Just MPlusOne
parseComp "M+1" = Just MPlusOne
parseComp "A-1" = Just AMinusOne
parseComp "D-1" = Just DMinusOne
parseComp "M-1" = Just MMinusOne
parseComp "A+D" = Just APlusD
parseComp "D+A" = Just APlusD
parseComp "D+M" = Just DPlusM
parseComp "M+D" = Just DPlusM
parseComp "A-D" = Just AMinusD
parseComp "D-A" = Just DMinusA
parseComp "D-M" = Just DMinusM
parseComp "M-D" = Just MMinusD
parseComp "A&D" = Just AAndD
parseComp "D&A" = Just AAndD
parseComp "D&M" = Just DAndM
parseComp "M&D" = Just DAndM
parseComp "A|D" = Just DOrA
parseComp "D|A" = Just DOrA
parseComp "D|M" = Just DOrM
parseComp "M|D" = Just DOrM
parseComp _ = Nothing

parseJump :: String -> Maybe Jump
parseJump "" = Just NullJump
parseJump "JGT" = Just JGT
parseJump "JEQ" = Just JEQ
parseJump "JGE" = Just JGE
parseJump "JLT" = Just JLT
parseJump "JNE" = Just JNE
parseJump "JLE" = Just JLE
parseJump "JMP" = Just JMP
parseJump _ = Nothing

splitCInst :: String -> (String, String, String)
splitCInst inst = (dest, comp, jump)
  where
    eqIndex = elemIndex '=' inst
    (dest, rest) = case eqIndex of Just index -> (take index inst, drop (index + 1) inst)
                                   Nothing -> ("", inst)
    semIndex = elemIndex ';' rest
    (comp, jump) = case semIndex of Just index -> (take index rest, drop (index + 1) rest)
                                    Nothing -> (rest, "")

parseCInst :: String -> Maybe CInst
parseCInst inst = case (parseDest d, parseComp c, parseJump j) of
  (Just d, Just c, Just j) -> Just $ CInst d c j
  _ -> Nothing
  where (d, c, j) = splitCInst inst

-- HANDLE COMMENTS

stripWhitespace :: String -> String
stripWhitespace s = filter (not . isSpace) s

splitCommentHelper :: String -> String -> (String, String)
splitCommentHelper "" acc = (acc, "")
splitCommentHelper ('/':'/':rest) acc = (acc, rest)
splitCommentHelper (x:xs) acc = splitCommentHelper xs (x:acc)

splitComment :: String -> (String, String)
splitComment str = (stripWhitespace $ reverse main, comment)
  where (main, comment) = splitCommentHelper str ""

-- CONSTRUCT AST

parseLine :: String -> Maybe Line
parseLine l = case (main == "", parseLabel main, parseAInst main, parseCInst main) of
  (True, _, _, _) -> Just $ LineE comment
  (_, Just label, _, _) -> Just $ LineL label comment
  (_, _, Just ainst, _) -> Just $ LineA ainst comment
  (_, _, _, Just cinst) -> Just $ LineC cinst comment
  _ -> Nothing
  where (main, comment) = splitComment l

parseLineOrError :: (Int, String) -> Line
parseLineOrError (lineNum, s) = case parseLine s of
  Just line -> line
  Nothing -> error $ "Failed to parse line " ++ show lineNum ++ ": " ++ s

parseFile :: String -> [Line]
parseFile source = map parseLineOrError $ zip [0..] (lines source)