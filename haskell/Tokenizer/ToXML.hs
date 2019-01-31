module Tokenizer.ToXML
( toXML, keywordToStr, symbolToStr
) where

import Tokenizer.Types

toXML :: [Token] -> String
toXML tokens = "<tokens>" ++ (concat $ map toXMLSingle tokens) ++ "</tokens>"

toXMLSingle :: Token -> String
toXMLSingle (TKeyword kw) = "<keyword> " ++ (keywordToStr kw) ++ " </keyword>"
toXMLSingle (TSymbol sym) = "<symbol> " ++ (symbolToStr sym) ++ " </symbol>"
toXMLSingle (TIntConst int) = "<integerConstant> " ++ (show int) ++ " </integerConstant>"
toXMLSingle (TStrConst str) = "<stringConstant> " ++ str ++ " </stringConstant>"
toXMLSingle (TIdentifier ident) = "<identifier> " ++ ident ++ " </identifier>"

keywordToStr :: Keyword -> String
keywordToStr KClass = "class"
keywordToStr KConstructor = "constructor"
keywordToStr KFunction = "function"
keywordToStr KMethod = "method"
keywordToStr KField = "field"
keywordToStr KStatic = "static"
keywordToStr KVar = "var"
keywordToStr KInt = "int"
keywordToStr KChar = "char"
keywordToStr KBoolean = "boolean"
keywordToStr KVoid = "void"
keywordToStr KTrue = "true"
keywordToStr KFalse = "false"
keywordToStr KNull = "null"
keywordToStr KThis = "this"
keywordToStr KLet = "let"
keywordToStr KDo = "do"
keywordToStr KIf = "if"
keywordToStr KElse = "else"
keywordToStr KWhile = "while"
keywordToStr KReturn = "return"

symbolToStr :: Symbol -> String
symbolToStr SLCurl = "{"
symbolToStr SRCurl = "}"
symbolToStr SLParen = "("
symbolToStr SRParen = ")"
symbolToStr SLSquare = "["
symbolToStr SRSquare = "]"
symbolToStr SDot = "."
symbolToStr SComma = ","
symbolToStr SSemicolon = ";"
symbolToStr SPlus = "+"
symbolToStr SMinus = "-"
symbolToStr SStar = "*"
symbolToStr SSlash = "/"
symbolToStr SAmp = "&amp;"
symbolToStr SBar = "|"
symbolToStr SLT = "&lt;"
symbolToStr SGT = "&gt;"
symbolToStr SEQ = "="