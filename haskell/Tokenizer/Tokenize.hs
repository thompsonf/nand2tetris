module Tokenizer.Tokenize 
( tokenize
) where

import Data.Char (isDigit, isSpace)
import Text.Read (read)
import Tokenizer.Types

tokenize :: String -> [Token]
tokenize input = reverse (tokenizeHelper [] "" input)

tokenizeHelper :: [Token] -> String -> String -> [Token]
tokenizeHelper acc temp "" = toToken acc temp
tokenizeHelper acc temp input@(x:xs) =
  if noComment /= input then tokenizeHelper newAcc "" noComment
  else if x == '"' then tokenizeHelper (strTok:acc) "" afterStrConst
  else case toSymbol x of
    Just sym -> tokenizeHelper ((TSymbol sym):newAcc) "" xs
    Nothing -> if (isSpace x) then tokenizeHelper newAcc "" xs
      else tokenizeHelper acc (x:temp) xs
  where newAcc = toToken acc temp
        noComment = stripComment input
        (strTok, afterStrConst) = takeStrConst "" xs

toToken :: [Token] -> String -> [Token]
toToken acc "" = acc
toToken acc rev =
  (if (isDigit (head input)) then TIntConst (read input :: Int)
  else case toKeyword input of
    Just kw -> TKeyword kw
    Nothing -> TIdentifier input):acc
  where input = reverse rev

takeStrConst :: String -> String -> (Token, String)
takeStrConst acc ('"':xs) = (TStrConst $ reverse acc, xs)
takeStrConst acc (x:xs) = takeStrConst (x:acc) xs

toKeyword :: String -> Maybe Keyword
toKeyword "class" = Just KClass
toKeyword "constructor" = Just KConstructor
toKeyword "function" = Just KFunction
toKeyword "method" = Just KMethod
toKeyword "field" = Just KField
toKeyword "static" = Just KStatic
toKeyword "var" = Just KVar
toKeyword "int" = Just KInt
toKeyword "char" = Just KChar
toKeyword "boolean" = Just KBoolean
toKeyword "void" = Just KVoid
toKeyword "true" = Just KTrue
toKeyword "false" = Just KFalse
toKeyword "null" = Just KNull
toKeyword "this" = Just KThis
toKeyword "let" = Just KLet
toKeyword "do" = Just KDo
toKeyword "if" = Just KIf
toKeyword "else" = Just KElse
toKeyword "while" = Just KWhile
toKeyword "return" = Just KReturn
toKeyword _ = Nothing

toSymbol :: Char -> Maybe Symbol
toSymbol '{' = Just SLCurl
toSymbol '}' = Just SRCurl
toSymbol '(' = Just SLParen
toSymbol ')' = Just SRParen
toSymbol '[' = Just SLSquare
toSymbol ']' = Just SRSquare
toSymbol '.' = Just SDot
toSymbol ',' = Just SComma
toSymbol ';' = Just SSemicolon
toSymbol '+' = Just SPlus
toSymbol '-' = Just SMinus
toSymbol '*' = Just SStar
toSymbol '/' = Just SSlash
toSymbol '&' = Just SAmp
toSymbol '|' = Just SBar
toSymbol '<' = Just SLT
toSymbol '>' = Just SGT
toSymbol '=' = Just SEQ
toSymbol _ = Nothing

stripComment :: String -> String
stripComment ('/':'/':xs) = dropWhile (/= '\n') xs
stripComment ('/':'*':xs) = stripBlockCommentHelper xs
stripComment xs = xs

stripBlockCommentHelper :: String -> String
stripBlockCommentHelper ('*':'/':xs) = xs
stripBlockCommentHelper (x:xs) = stripBlockCommentHelper xs