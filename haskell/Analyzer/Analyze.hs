module Analyzer.Analyze
( analyze
) where

import Analyzer.Parser
import Analyzer.Types
import Tokenizer.Types

analyze :: [Token] -> AClass
analyze = error "not implemented"

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
    tokens' -> let (expr, restTokens) = runParser parseExpression tokens' in case (tokens') of
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
  runParser (AUnaryOp <$> parseTerm) rest