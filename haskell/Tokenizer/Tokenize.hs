module Tokenizer.Tokenize 
( tokenize
) where

import Data.Char (isDigit, isSpace)
import Text.Read (read)
import Tokenizer.Types

tokenize :: String -> [Token]
tokenize input = reverse (tokenizeHelper [] "" input)

tokenizeHelper :: [Token] -> String -> String -> [Token]
tokenizeHelper acc temp input@(x:xs) =
  if noComment /= input then tokenizeHelper (token:acc) "" noComment
  else case toSymbol x of
    Just sym -> tokenizeHelper ((TSymbol sym):token:acc) "" xs
    Nothing -> tokenizeHelper acc (x:temp) xs
  where token = toToken (reverse temp)
        noComment = stripComment xs
tokenizeHelper acc temp "" = (toToken temp):acc

toToken :: String -> Token
toToken input@(x:xs) =
  if (isDigit x) then TIntConst (read input :: Int)
  else if (x == '"') then TStrConst (readStrConst xs)
  else case toKeyword input of
    Just kw -> TKeyword kw
    Nothing -> TIdentifier input

readStrConst :: String -> String
readStrConst ('"':_) = ""
readStrConst (x:xs) = x:(readStrConst xs)
readStrConst _ = ""

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