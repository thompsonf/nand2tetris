module Analyzer.Analyze
( analyze, tokenize, runParser
) where

import Analyzer.Parser
import Analyzer.Types
import Tokenizer.Types
import Tokenizer.Tokenize (tokenize)
import Debug.Trace

analyze :: [Token] -> AClass
analyze tokens = fst $ runParser parseClass tokens

parseIdent :: Parser String
parseIdent = Parser helper
  where
    helper ((TIdentifier str):rest) = (str, rest)
    helper _ = error "expected identifier"

eatSym :: Symbol -> Parser ()
eatSym sym = Parser helper
  where
    helper ((TSymbol sym):rest) = ((), rest)
    helper _ = error $ "expected symbol " ++ show sym

eatKW :: Keyword -> Parser ()
eatKW kw = Parser helper
  where
    helper ((TKeyword kw):rest) = ((), rest)
    helper _ = error $ "expected keyword " ++ show kw

parseOp :: Parser AOp
parseOp = Parser $ \(token:rest) -> (helper token, rest)
  where
    helper (TSymbol SPlus) = AOPlus
    helper (TSymbol SMinus) = AOMinus
    helper (TSymbol SStar) = AOStar
    helper (TSymbol SSlash) = AOSlash
    helper (TSymbol SAmp) = AOAnd
    helper (TSymbol SBar) = AOBar
    helper (TSymbol SLT) = AOLT
    helper (TSymbol SGT) = AOGT
    helper (TSymbol SEQ) = AOEq
    helper _ = error "expected operator"


takeOp :: Token -> Maybe AOp
takeOp (TSymbol SPlus) = Just AOPlus
takeOp (TSymbol SMinus) = Just AOMinus
takeOp (TSymbol SStar) = Just AOStar
takeOp (TSymbol SSlash) = Just AOSlash
takeOp (TSymbol SAmp) = Just AOAnd
takeOp (TSymbol SBar) = Just AOBar
takeOp (TSymbol SLT) = Just AOLT
takeOp (TSymbol SGT) = Just AOGT
takeOp (TSymbol SEQ) = Just AOEq
takeOp _ = Nothing

toKeywordConst :: Token -> Maybe AKeywordConst
toKeywordConst (TKeyword KTrue) = Just AKTrue
toKeywordConst (TKeyword KFalse) = Just AKFalse
toKeywordConst (TKeyword KNull) = Just AKNull
toKeywordConst (TKeyword KThis) = Just AKThis 
toKeywordConst _ = Nothing

parseExpression :: Parser AExpression
parseExpression = Parser $ \tokens ->
  let (term, rest) = runParser parseTerm tokens
  in case rest of
    [] -> (AExpression term [], [])
    (tok:toks) -> case (takeOp tok) of
      Nothing -> (AExpression term [], rest)
      Just op -> let (AExpression term2 otherTerms, restTokens) = runParser parseExpression toks
        in (AExpression term ((op, term2):otherTerms), restTokens)

parseExpressionList :: Parser [AExpression]
parseExpressionList = Parser $ \tokens ->
  case tokens of
    ((TSymbol SRParen):restTokens) -> ([], restTokens)
    tokens' -> let (expr, restTokens) = runParser parseExpression tokens' in case (restTokens) of
      ((TSymbol SRParen):others) -> ([expr], others)
      ((TSymbol SComma):others) -> let (exprList, leftovers) = runParser parseExpressionList others in
        ((expr:exprList), leftovers)

parseTerm :: Parser ATerm
parseTerm = Parser termHelper

termHelper :: [Token] -> (ATerm, [Token])
termHelper ((TIntConst int):rest) = (AIntConst int, rest)
termHelper ((TStrConst str):rest) = (AStrConst str, rest)
termHelper ((TKeyword KTrue):rest) = (AKey AKTrue, rest)
termHelper ((TKeyword KFalse):rest) = (AKey AKFalse, rest)
termHelper ((TKeyword KNull):rest) = (AKey AKNull, rest)
termHelper ((TKeyword KThis):rest) = (AKey AKThis, rest)
termHelper ((TIdentifier str):(TSymbol SLSquare):rest) =
  runParser ((AVarName str) <$> (Just <$> parseExpression) <* (eatSym SRSquare)) rest
termHelper ((TIdentifier fun):(TSymbol SLParen):rest) = 
  let (exprs, otherTokens) = runParser parseExpressionList rest
  in (ASub $ ASubroutineCall Nothing fun exprs, otherTokens)
termHelper ((TIdentifier cls):(TSymbol SDot):(TIdentifier fun):(TSymbol SLParen):rest) = 
  let (exprs, otherTokens) = runParser parseExpressionList rest
  in (ASub $ ASubroutineCall (Just cls) fun exprs, otherTokens)
termHelper ((TIdentifier str):rest) = (AVarName str Nothing, rest)
termHelper ((TSymbol SLParen):rest) = 
  runParser (AParenExpr <$> parseExpression <* (eatSym SRParen)) rest
termHelper ((TSymbol SMinus):rest) =
  runParser (AUnaryOp AUOMinus <$> parseTerm) rest
termHelper ((TSymbol STilde):rest) =
  runParser (AUnaryOp AUOTilde <$> parseTerm) rest
termHelper other = error ("unexpected keyword term tokens " ++ show other)

parseStatement :: Parser AStatement
parseStatement = Parser statementHelper

statementHelper :: [Token] -> (AStatement, [Token])
statementHelper ((TKeyword KLet):(TIdentifier varName):(TSymbol SLSquare):rest) = 
  runParser (
    (ALet varName)
      <$> (Just <$> parseExpression)
      <* (eatSym SRSquare)
      <* (eatSym SEQ)
      <*> parseExpression
      <* (eatSym SSemicolon)
  ) rest
statementHelper ((TKeyword KLet):(TIdentifier varName):rest) = 
  runParser ((ALet varName Nothing) <$> ((eatSym SEQ) *> parseExpression <* (eatSym SSemicolon))) rest
statementHelper ((TKeyword KDo):rest) =
  runParser (ADo <$> parseSubroutineCall <* (eatSym SSemicolon)) rest
statementHelper ((TKeyword KReturn):(TSymbol SSemicolon):rest) = (AReturn Nothing, rest)
statementHelper ((TKeyword KReturn):rest) =
  runParser (AReturn <$> ((Just <$> parseExpression) <* (eatSym SSemicolon))) rest
statementHelper ((TKeyword KWhile):(TSymbol SLParen):rest) = 
  runParser (AWhile <$> parseExpression <* (eatSym SRParen) <* (eatSym SRCurl) <*> parseStatementList) rest
statementHelper ((TKeyword KIf):(TSymbol SLParen):rest) = 
  runParser (AIf <$> parseExpression <* (eatSym SRParen) <* (eatSym SRCurl) <*> parseStatementList <*> parseElse) rest
statementHelper other = error ("unexpected statement tokens " ++ show other)

parseSubroutineCall :: Parser ASubroutineCall
parseSubroutineCall = Parser $ \tokens -> case tokens of
  ((TIdentifier cls):(TSymbol SDot):_) -> runParser withParser tokens
  _ -> runParser withoutParser tokens
  where
    withParser = ASubroutineCall
      <$> (Just <$> parseIdent)
      <* (eatSym SDot)
      <*> parseIdent
      <* (eatSym SLParen)
      <*> parseExpressionList
    withoutParser = (ASubroutineCall Nothing)
      <$> parseIdent
      <* (eatSym SLParen)
      <*> parseExpressionList

parseElse :: Parser (Maybe [AStatement])
parseElse = Parser $ \tokens -> case tokens of
  ((TKeyword KElse):rest) -> runParser (Just <$> ((eatSym SLCurl) *> parseStatementList)) rest
  other -> (Nothing, other)

parseStatementList :: Parser [AStatement]
parseStatementList = Parser $ \tokens ->
  case tokens of
    ((TSymbol SRCurl):restTokens) -> ([], restTokens)
    tokens' -> let (stmt, restTokens) = runParser parseStatement tokens' in case (restTokens) of
      ((TSymbol SRCurl):others) -> ([stmt], others)
      others -> let (stmtList, leftovers) = runParser parseStatementList others in
        ((stmt:stmtList), leftovers)

parseType :: Parser AType
parseType = Parser $ \all@(theType:rest) -> case theType of
  (TKeyword KInt) -> (AInt, rest)
  (TKeyword KChar) -> (AChar, rest)
  (TKeyword KBoolean) -> (ABoolean, rest)
  (TIdentifier name) -> (AClassName name, rest)
  _ -> error ("unexpected type tokens " ++ show all)

parseVarNames :: Parser [String]
parseVarNames = Parser $ \((TIdentifier name):rest) -> case rest of
  ((TSymbol SComma):tokens') -> let (otherVarNames, lastTokens) = runParser parseVarNames tokens' in (name:otherVarNames, lastTokens)
  others -> ([name], others)

parseClassVarDec :: Parser AClassVarDec
parseClassVarDec = Parser $ \((TKeyword kw):rest) -> case kw of
  KStatic -> runParser (AStatic <$> parseType <*> parseVarNames <* (eatSym SSemicolon)) rest
  KField -> runParser (AField <$> parseType <*> parseVarNames <* (eatSym SSemicolon)) rest

parseClassVarDecs :: Parser [AClassVarDec]
parseClassVarDecs = Parser $ \tokens -> case tokens of
  ((TKeyword KStatic):_) -> help tokens
  ((TKeyword KField):_) -> help tokens
  _ -> ([], tokens)
  where help t = let (firstVarDec, afterFirst) = runParser parseClassVarDec t
                     (restVarDecs, remaining) = runParser parseClassVarDecs afterFirst
                 in (firstVarDec:restVarDecs, remaining)

parseVarDec :: Parser AVarDec
parseVarDec = Parser $ \((TKeyword KVar):rest) ->
  runParser (AVarDec <$> parseType <*> parseVarNames <* (eatSym SSemicolon)) rest

parseVarDecs :: Parser [AVarDec]
parseVarDecs = Parser $ \tokens -> case tokens of
  ((TKeyword KVar):_) -> let (firstVarDec, afterFirst) = runParser parseVarDec tokens
                             (restVarDecs, remaining) = runParser parseVarDecs afterFirst
                         in (firstVarDec:restVarDecs, remaining)
  _ -> ([], tokens)

parseParameter :: Parser AParameter
parseParameter = AParameter <$> parseType <*> parseIdent

parseCommaParameter :: Parser AParameter
parseCommaParameter = AParameter <$ (eatSym SComma) <*> parseType <*> parseIdent

parseRestParameters :: Parser [AParameter]
parseRestParameters = Parser $ \tokens -> case tokens of
  ((TSymbol SRParen):rest) -> ([], rest)
  all@((TSymbol SComma):_) -> let (firstParam, afterFirst) = runParser parseCommaParameter all
                                  (restParams, remaining) = runParser parseRestParameters afterFirst
                              in (firstParam:restParams, remaining)
  others -> error ("unexpected tokens when parsing rest of param list " ++ show others)

parseParameters :: Parser [AParameter]
parseParameters = Parser $ \tokens -> case tokens of
  ((TSymbol SRParen):rest) -> ([], rest)
  others -> let (firstParam, afterFirst) = runParser parseParameter others
                (restParams, remaining) = runParser parseRestParameters afterFirst
            in (firstParam:restParams, remaining)

parseFuncReturn :: Parser AFuncReturn
parseFuncReturn = Parser $ \tokens -> case tokens of
  ((TKeyword KVoid):rest) -> (AFuncReturnVoid, rest)
  others -> let (t, rest) = runParser parseType others in (AFuncReturnType t, rest)

parseSubroutineBody :: Parser ASubroutineBody
parseSubroutineBody = ASubroutineBody <$ (eatSym SLCurl) <*> parseVarDecs <*> parseStatementList

parseSubroutineKind :: Parser ASubroutineKind
parseSubroutineKind = Parser $ \tokens -> case tokens of
  ((TKeyword KConstructor):rest) -> (AConstructor, rest)
  ((TKeyword KFunction):rest) -> (AFunction, rest)
  ((TKeyword KMethod):rest) -> (AMethod, rest)

parseSubroutineDec :: Parser ASubroutineDec
parseSubroutineDec = ASubroutineDec
  <$> parseSubroutineKind
  <*> parseFuncReturn
  <*> parseIdent
  <*  (eatSym SLParen)
  <*> parseParameters
  <*> parseSubroutineBody

parseSubroutineDecs :: Parser [ASubroutineDec]
parseSubroutineDecs = Parser $ \tokens -> case tokens of
  ((TSymbol SRCurl):rest) -> ([], rest)
  others -> let (firstSub, afterFirst) = runParser parseSubroutineDec others
                (restSubs, remaining) = runParser parseSubroutineDecs afterFirst
            in (firstSub:restSubs, remaining)

parseClass :: Parser AClass
parseClass = AClass
  <$ (eatKW KClass)
  <*> parseIdent
  <* (eatSym SLCurl)
  <*> parseClassVarDecs
  <*> parseSubroutineDecs
