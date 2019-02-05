module Compiler.Compiler
( compile
) where

import Analyzer.Types
import Compiler.SymTab
import Data.List (foldl')
import VM.Types

compile :: AClass -> [Command]
compile _ = error "not implemented"

addMany :: (SymTab -> String -> SymTab) -> [String] -> SymTab
addMany update strings = foldl' update [] strings

addClassVarDec :: SymTab -> AClassVarDec -> SymTab
addClassVarDec table (AStatic theType names) = addMany (add VStatic theType) names
addClassVarDec table (AField theType names) = addMany (add VField theType) names

getClassSymTab :: [AClassVarDec] -> SymTab
getClassSymTab decs = foldl' addClassVarDec [] decs