module Compiler.Compiler
( compile
) where

import Analyzer.Types
import Compiler.SymTab
import Data.List (foldl')
import VM.Types

compile :: AClass -> [Command]
compile (AClass cName varDecs subDecs) = concat $ map (compileSubDec cName symTab) subDecs
  where symTab = foldl' addClassVarDec [] varDecs

compileSubDec :: String -> SymTab -> ASubroutineDec -> [Command]
compileSubDec cName cTable dec@(ASubroutineDec _ ret sName params body) = error "not implemented"
  where symTab = addSubDec cName cTable dec

addMany :: (SymTab -> String -> SymTab) -> [String] -> SymTab
addMany update strings = foldl' update [] strings

addClassVarDec :: SymTab -> AClassVarDec -> SymTab
addClassVarDec table (AStatic theType names) = addMany (add VStatic theType) names
addClassVarDec table (AField theType names) = addMany (add VField theType) names

addVarDec :: SymTab -> AVarDec -> SymTab
addVarDec table (AVarDec theType names) = addMany (add VLocal theType) names

addParam :: SymTab -> AParameter -> SymTab
addParam table (AParameter theType name) = add VArgument theType table name

addSubDec :: String -> SymTab -> ASubroutineDec -> SymTab
addSubDec cName table (ASubroutineDec subKind _ _ params (ASubroutineBody localDecs _)) = withBody
  where
    withThis = if subKind == AMethod then add VArgument (AClassName cName) table "this" else table
    withParams = foldl' addParam withThis params
    withBody = foldl' addVarDec withParams localDecs

