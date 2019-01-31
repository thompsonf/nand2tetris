module Analyzer.ToXML
( toXML
) where

import Analyzer.Types
import Data.List (intercalate)
import Tokenizer.ToXML (keywordToStr, symbolToStr)
import Tokenizer.Types

toXML :: AClass -> String
toXML (AClass name varDecs subDecs) = "<class>\n"
  ++ kw KClass
  ++ identToXML name
  ++ sym SLCurl
  ++ (concat $ map classVarDecToXML varDecs)
  ++ (concat $ map subroutineDecToXML subDecs)
  ++ sym SRCurl
  ++ "</class>\n"

classVarDecToXML :: AClassVarDec -> String
classVarDecToXML (AStatic theType names)= "<classVarDec>\n"
  ++ kw KStatic
  ++ (typeToXML theType)
  ++ intercalate (sym SComma) (map identToXML names)
  ++ sym SSemicolon
  ++ "</classVarDec>\n"
classVarDecToXML (AField theType names)= "<classVarDec>\n"
  ++ kw KField
  ++ (typeToXML theType)
  ++ intercalate (sym SComma) (map identToXML names)
  ++ sym SSemicolon
  ++ "</classVarDec>\n"

subroutineDecToXML :: ASubroutineDec -> String
subroutineDecToXML (ASubroutineDec kind ret name params body) = "<subroutineDec>\n"
  ++ kindToXML kind
  ++ funReturnToXML ret
  ++ identToXML name
  ++ sym SLParen
  ++ "<parameterList>\n"
  ++ intercalate (sym SComma) (map parameterToXML params)
  ++ "</parameterList>\n"
  ++ sym SRParen
  ++ subroutineBodyToXML body
  ++ "</subroutineDec>\n"

funReturnToXML :: AFuncReturn -> String
funReturnToXML (AFuncReturnType theType) = typeToXML theType
funReturnToXML AFuncReturnVoid = kw KVoid

parameterToXML :: AParameter -> String
parameterToXML (AParameter theType name) = typeToXML theType ++ identToXML name

varDecToXML :: AVarDec -> String
varDecToXML (AVarDec theType names) = "<varDec>\n"
  ++ kw KVar
  ++ typeToXML theType
  ++ intercalate (sym SComma) (map identToXML names)
  ++ sym SSemicolon
  ++ "</varDec>\n"

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
statementToXML (ALet var idx expr) = "<letStatement>\n"
  ++ kw KLet
  ++ varWithIdxToXML var idx
  ++ sym SEQ
  ++ exprToXML expr
  ++ sym SSemicolon
  ++ "</letStatement>\n"
statementToXML (AIf cond ifBody elseBody) = "<ifStatement>\n"
  ++ kw KIf
  ++ sym SLParen
  ++ exprToXML cond
  ++ sym SRParen
  ++ sym SLCurl
  ++ statementsToXML ifBody
  ++ sym SRCurl
  ++ elseStr
  ++ "</ifStatement>\n"
  where elseStr = case elseBody of Nothing -> ""
                                   Just elseStmts -> kw KElse ++ sym SLCurl ++ statementsToXML elseStmts ++ sym SRCurl
statementToXML (AWhile cond body) = "<whileStatement>\n"
  ++ kw KWhile
  ++ sym SLParen
  ++ exprToXML cond
  ++ sym SRParen
  ++ sym SLCurl
  ++ statementsToXML body
  ++ sym SRCurl
  ++ "</whileStatement>\n"
statementToXML (ADo call) = "<doStatement>\n"
  ++ kw KDo
  ++ subroutineCallToXML call
  ++ sym SSemicolon
  ++ "</doStatement>\n"
statementToXML (AReturn expr) = "<returnStatement>\n"
  ++ kw KReturn
  ++ exprStr
  ++ sym SSemicolon
  ++ "</returnStatement>\n"
  where exprStr = case expr of Nothing -> ""
                               Just je -> exprToXML je

statementsToXML :: [AStatement] -> String
statementsToXML stmts = "<statements>\n"
  ++ (concat $ map statementToXML stmts)
  ++ "</statements>\n"

exprToXML :: AExpression -> String
exprToXML (AExpression term others) = "<expression>\n"
  ++ termToXML term
  ++ (concat $ map (\(o, t) -> opToXML o ++ termToXML t) others)
  ++ "</expression>\n"

exprListToXML :: [AExpression] -> String
exprListToXML exprs = "<expressionList>\n"
  ++ intercalate (sym SComma) (map exprToXML exprs) 
  ++ "</expressionList>\n"

termToXML :: ATerm -> String
termToXML term = "<term>\n" ++ termToXMLHelp term ++ "</term>\n"

keywordConstToXML :: AKeywordConst -> String
keywordConstToXML AKTrue = kw KTrue
keywordConstToXML AKFalse = kw KFalse
keywordConstToXML AKNull = kw KNull
keywordConstToXML AKThis = kw KThis

termToXMLHelp :: ATerm -> String
termToXMLHelp (AIntConst int) = "<integerConstant> " ++ show int ++ " </integerConstant>\n"
termToXMLHelp (AStrConst str) = "<stringConstant> " ++ str ++ " </stringConstant>\n"
termToXMLHelp (AKey key) = keywordConstToXML key
termToXMLHelp (AVarName var idx) = varWithIdxToXML var idx
termToXMLHelp (ASub call) = subroutineCallToXML call
termToXMLHelp (AParenExpr expr) = sym SLParen ++ exprToXML expr ++ sym SRParen
termToXMLHelp (AUnaryOp uop term) = uopToXML uop ++ termToXML term

subroutineBodyToXML :: ASubroutineBody -> String
subroutineBodyToXML (ASubroutineBody decs stmts) = "<subroutineBody>\n"
  ++ sym SLCurl
  ++ (concat $ map varDecToXML decs)
  ++ statementsToXML stmts
  ++ sym SRCurl
  ++ "</subroutineBody>\n"

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

uopToXML :: AUOp -> String
uopToXML AUOMinus = sym SMinus
uopToXML AUOTilde = sym STilde

typeToXML :: AType -> String
typeToXML AInt = kw KInt
typeToXML AChar = kw KChar
typeToXML ABoolean = kw KBoolean
typeToXML (AClassName name) = identToXML name

identToXML :: String -> String
identToXML ident = "<identifier> " ++ ident ++ " </identifier>\n"

kw :: Keyword -> String
kw x = "<keyword> " ++ keywordToStr x ++ " </keyword>\n"

sym :: Symbol -> String
sym x = "<symbol> " ++ symbolToStr x ++ " </symbol>\n"
