module Analyzer.ToXML
( toXML
) where

import Analyzer.Types
import Data.List (intercalate)
import Tokenizer.ToXML (keywordToStr, symbolToStr)
import Tokenizer.Types

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
  ++ identToXML name
  ++ sym SLParen
  ++ "<parameterList>"
  ++ intercalate (sym SComma) (map parameterToXML params)
  ++ "</parameterList>"
  ++ sym SRParen
  ++ subroutineBodyToXML body
  ++ "</subroutineDec>"

funReturnToXML :: AFuncReturn -> String
funReturnToXML (AFuncReturnType theType) = typeToXML theType
funReturnToXML AFuncReturnVoid = kw KVoid

parameterToXML :: AParameter -> String
parameterToXML (AParameter theType name) = typeToXML theType ++ identToXML name

varDecToXML :: AVarDec -> String
varDecToXML (AVarDec theType names) = "<varDec>"
  ++ typeToXML theType
  ++ intercalate (sym SComma) (map identToXML names)
  ++ sym SSemicolon
  ++ "</varDec>"

subroutineCallToXML :: ASubroutineCall -> String
subroutineCallToXML (ASubroutineCall className func args) = classStr
  ++ identToXML func
  ++ sym SLParen
  ++ exprListToXML args
  ++ sym SRParen
  where classStr = case className of Nothing -> ""
                                     Just cn -> identToXML cn ++ sym SDot

varWithIdxToXML :: String -> Maybe AExpression -> String
varWithIdxToXML var idx = identToXML var ++ idxStr
  where idxStr = case idx of Nothing -> ""
                             Just idxExpr -> sym SLSquare ++ exprToXML idxExpr ++ sym SRSquare

statementToXML :: AStatement -> String
statementToXML (ALet var idx expr) = "<letStatement>"
  ++ kw KLet
  ++ varWithIdxToXML var idx
  ++ sym SEQ
  ++ exprToXML expr
  ++ sym SSemicolon
  ++ "</letStatement>"
statementToXML (AIf cond ifBody elseBody) = "<ifStatement>"
  ++ kw KIf
  ++ sym SLParen
  ++ exprToXML cond
  ++ sym SRParen
  ++ sym SLCurl
  ++ statementsToXML ifBody
  ++ sym SRCurl
  ++ elseStr
  ++ "</ifStatement>"
  where elseStr = case elseBody of Nothing -> ""
                                   Just elseStmts -> kw KElse ++ sym SLCurl ++ statementsToXML elseStmts ++ sym SRCurl
statementToXML (AWhile cond body) = "<whileStatement>"
  ++ kw KWhile
  ++ sym SLParen
  ++ exprToXML cond
  ++ sym SRParen
  ++ sym SLCurl
  ++ statementsToXML body
  ++ sym SRCurl
statementToXML (ADo call) = "<doStatement>"
  ++ kw KDo
  ++ subroutineCallToXML call
  ++ sym SSemicolon
  ++ "</doStatement>"
statementToXML (AReturn expr) = "<returnStatement>"
  ++ kw KReturn
  ++ exprStr
  ++ sym SSemicolon
  ++ "</returnStatement>"
  where exprStr = case expr of Nothing -> ""
                               Just je -> exprToXML je

statementsToXML :: [AStatement] -> String
statementsToXML stmts = "<statements>"
  ++ (concat $ map statementToXML stmts)
  ++ "</statements>"

exprToXML :: AExpression -> String
exprToXML (AExpression term others) = "<expression>"
  ++ termToXML term
  ++ (concat $ map (\(o, t) -> opToXML o ++ termToXML t) others)
  ++ "</expression>"

exprListToXML :: [AExpression] -> String
exprListToXML exprs = "<expressionList>"
  ++ intercalate (sym SComma) (map exprToXML exprs) 
  ++ "</expressionList>"

termToXML :: ATerm -> String
termToXML term = "<term>" ++ termToXMLHelp term ++ "</term>"

keywordConstToXML :: AKeywordConst -> String
keywordConstToXML AKTrue = kw KTrue
keywordConstToXML AKFalse = kw KFalse
keywordConstToXML AKNull = kw KNull
keywordConstToXML AKThis = kw KThis

termToXMLHelp :: ATerm -> String
termToXMLHelp (AIntConst int) = "<integerConstant>" ++ show int ++ "</integerConstant>"
termToXMLHelp (AStrConst str) = "<stringConstant>" ++ str ++ "</stringConstant>"
termToXMLHelp (AKey key) = keywordConstToXML key
termToXMLHelp (AVarName var idx) = varWithIdxToXML var idx
termToXMLHelp (ASub call) = subroutineCallToXML call
termToXMLHelp (AParenExpr expr) = sym SLParen ++ exprToXML expr ++ sym SRParen
termToXMLHelp (AUnaryOp term) = sym SMinus ++ termToXML term

subroutineBodyToXML :: ASubroutineBody -> String
subroutineBodyToXML (ASubroutineBody decs stmts) = "<subroutineBody>"
  ++ sym SLCurl
  ++ (concat $ map varDecToXML decs)
  ++ statementsToXML stmts
  ++ sym SRCurl
  ++ "</subroutineBody>"

kindToXML :: ASubroutineKind -> String
kindToXML AConstructor = kw KConstructor
kindToXML AFunction = kw KFunction
kindToXML AMethod = kw KMethod

funcReturnToXML :: AFuncReturn -> String 
funcReturnToXML (AFuncReturnType t) = typeToXML t
funcReturnToXML AFuncReturnVoid = kw KVoid

opToXML :: AOp -> String
opToXML AOPlus = sym SPlus
opToXML AOMinus = sym SMinus
opToXML AOStar = sym SStar
opToXML AOSlash = sym SSlash
opToXML AOAnd = sym SAmp
opToXML AOBar = sym SBar
opToXML AOLT = sym SLT
opToXML AOGT = sym SGT
opToXML AOEq = sym SEQ

typeToXML :: AType -> String
typeToXML AInt = "<keyword>int</keyword>"
typeToXML AChar = "<keyword>char</keyword>"
typeToXML ABoolean = "<keyword>boolean</keyword>"
typeToXML (AClassName name) = identToXML name

identToXML :: String -> String
identToXML ident = "<identifier>" ++ ident ++ "</identifier>"

kw :: Keyword -> String
kw x = "<keyword>" ++ keywordToStr x ++ "</keyword>"

sym :: Symbol -> String
sym x = "<symbol>" ++ symbolToStr x ++ "</symbol>"
