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
parseOp = Parser \(token:rest) -> (helper token, rest)
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


toOp :: Token -> Maybe AOp
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

parseExpression :: [Token] -> (AExpression, [Token])
parseExpression = error "not implemented"

parseExpressionList :: [Token] -> ([AExpression], [Token])
parseExpressionList = error "not implemented"

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
  let (expr, afterExpr) = parseExpression rest
  in eat (TSymbol SRSquare) afterExpr
termHelper ((TIdentifier str):(TSymbol SLParen):rest) = 
  let (exprs, afterExprs) = parseExpressionList rest
      beforeObj = eat (TSymbol SRParen) afterExprs
      (objName, afterObj) = eatIdent beforeObj
      afterDot = eat (TSymbol SDot) afterIdent
      (funcName, afterFunc) = eatIdent afterDot
      afterLParen = eat (TSymbol SLParen) afterFunc


  in case afterExpr of
    ((TSymbol SRSquare):remain) -> (AVarName str (Just expr), remain)
    _ -> error "could not find right bracket"
termHelper ((TIdentifier str):rest) = (AVarName str Nothing, rest)
termHelper (())
