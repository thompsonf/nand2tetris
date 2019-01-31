module Analyzer.ToXML
( toXML
) where

import Analyzer.Types
import Tokenizer.ToXML (keywordToStr, symbolToStr)

toXML :: AClass -> String
toXML (AClass name varDecs subDecs) = "<class>"
  ++ kw KClass
  ++ identToXML name
  ++ (concat $ map classVarDecToXML varDecs)
  ++ (concat $ map subroutineDecToXML subDecs)
  ++ "</class>"

classVarDecToXML :: AClassVarDec -> String
classVarDecToXML (AStatic theType names)= "<classVarDec>"
  ++ kw KStatic
  ++ (typeToXML theType)
  ++ (concat $ map identToXML names)
  ++ "</classVarDec>"
classVarDecToXML (AField theType names)= "<classVarDec>"
  ++ kw KField
  ++ (typeToXML theType)
  ++ (concat $ map identToXML names)
  ++ "</classVarDec>"

subroutineDecToXML :: ASubroutineDec -> String
subroutineDecToXML (ASubroutineDec kind ret name params body) = "<subroutineDec>"
  ++ kindToXML kind
  ++ funReturnToXML ret
  ++ identifierToXML name
  ++ sym SLParen
  ++ "<parameterList>"
  -- TODO
  ++ "</parameterList>"
  ++ sym SRParen

kindToXML :: ASubroutineKind -> String
kindToXML AConstructor = kw KConstructor
kindToXML AFunction = kw KFunction
kindToXML AMethod = kw KMethod

funcReturnToXML :: AFuncReturn -> String
funcReturnToXML (AFuncReturnType t) = typeToXML t
funcReturnToXML AFuncReturnVoid = kw KVoid

typeToXML :: AType -> String
typeToXML AInt = "<keyword>int</keyword>"
typeToXML AChar = "<keyword>char</keyword>"
typeToXML ABoolean = "<keyword>boolean</keyword>"
typeToXML (AClassName name) = ident name

identToXML :: String -> String
identToXML ident = "<identifier>" ++ ident ++ "</identifier>"

kw :: Keyword -> String
kw x = "<keyword>" ++ keywordToStr x ++ "</keyword>"

sym :: Symbol -> String
sym x = "<symbol>" ++ symbolToStr x ++ "</symbol>"
