module Compiler.Compiler
( compile
) where

import Analyzer.Types
import Compiler.SymTab
import Control.Monad.State
import Data.List (foldl')
import VM.Types

type Context = State (String, Int)

compile :: AClass -> [Command]
compile (AClass cName varDecs subDecs) = concat $ map (compileSubDec cName symTab) subDecs
  where symTab = foldl' addClassVarDec [] varDecs

fullName :: String -> String -> String
fullName cName sName = cName ++ "." ++ sName

getLbl :: Context String
getLbl = do
  (fullName, lblCount) <- get
  put (fullName, lblCount + 1)
  return $ fullName ++ "##" ++ (show lblCount)

compileStatement :: AStatement -> Context [Command]
compileStatement (ALet var mArray expr) = error "not implemented"
compileStatement (ADo call) = error "not implemented"
compileStatement (AReturn mExpr) = error "not implemented"
compileStatement (AIf expr ifStmts mElseStmts) = error "not implemented"
compileStatement (AWhile expr stmts) = error "not implemented"

compileSubDec :: String -> SymTab -> ASubroutineDec -> [Command]
compileSubDec cName cTable dec@(ASubroutineDec subKind ret sName params (ASubroutineBody localDecs statements)) =
  (CFun $ Fun (fullName cName sName) nLocals):compiledBody
  where
    symTab = addSubDec cName cTable dec
    nLocalsOfDec (AVarDec _ names) = length names
    nLocals = sum $ map nLocalsOfDec localDecs
    compiledBody = []

addMany :: (SymTab -> String -> SymTab) -> [String] -> SymTab
addMany update strings = foldl' update [] strings

addClassVarDec :: SymTab -> AClassVarDec -> SymTab
addClassVarDec table (AStatic theType names) = addMany (addSym VStatic theType) names
addClassVarDec table (AField theType names) = addMany (addSym VField theType) names

addVarDec :: SymTab -> AVarDec -> SymTab
addVarDec table (AVarDec theType names) = addMany (addSym VLocal theType) names

addParam :: SymTab -> AParameter -> SymTab
addParam table (AParameter theType name) = addSym VArgument theType table name

addSubDec :: String -> SymTab -> ASubroutineDec -> SymTab
addSubDec cName table (ASubroutineDec subKind _ _ params (ASubroutineBody localDecs _)) = withBody
  where
    withThis = if subKind == AMethod then addSym VArgument (AClassName cName) table "this" else table
    withParams = foldl' addParam withThis params
    withBody = foldl' addVarDec withParams localDecs

