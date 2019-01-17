module Tokenizer.Types 
( Keyword (..)
, Symbol (..)
, Token (..)
) where

data Keyword = KClass
  | KConstructor
  | KFunction
  | KMethod
  | KField
  | KStatic
  | KVar
  | KInt
  | KChar
  | KBoolean
  | KVoid
  | KTrue
  | KFalse
  | KNull
  | KThis
  | KLet
  | KDo
  | KIf
  | KElse
  | KWhile
  | KReturn
  deriving Show

data Symbol = SLCurl
  | SRCurl
  | SLParen
  | SRParen
  | SLSquare
  | SRSquare
  | SDot
  | SComma
  | SSemicolon
  | SPlus
  | SMinus
  | SStar
  | SSlash
  | SAmp
  | SBar
  | SLT
  | SGT
  | SEQ
  deriving (Eq, Show)
  -- Might be missing one? Looked like repeat '-' in spec

data Token = TKeyword Keyword
  | TSymbol Symbol
  | TIntConst Int
  | TStrConst String
  | TIdentifier String
  deriving Show