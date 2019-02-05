module Compiler.SymTab where

import Analyzer.Types (AType)
import Data.List (find)

data VarKind = VStatic | VField | VArgument | VLocal
  deriving (Eq, Show)

data SymVar = SymVar String AType VarKind Int

type SymTab = [SymVar]

-- odd order is to make it easy to use with foldl'
add :: VarKind -> AType -> SymTab -> String -> SymTab
add kind theType table name = (SymVar name theType kind (nextIndex kind table)):table
  where
    nextIndex kind [] = 0
    nextIndex kind ((SymVar _ _ k idx):rest) =
      if k == kind then idx + 1 else nextIndex kind rest

get :: SymTab -> String -> Maybe SymVar
get table name = find (hasName name) table
  where hasName name (SymVar n _ _ _) = n == name