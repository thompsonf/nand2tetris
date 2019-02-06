module Compiler.Compiler
( compile
) where

import Analyzer.Types
import Compiler.SymTab
import Control.Monad.State
import Data.Char (ord)
import Data.List (foldl')
import VM.Types

type Context = State (String, Int)

compile :: AClass -> [Command]
compile (AClass cName varDecs subDecs) = concat $ map (compileSubDec symTab cName nFields) subDecs
  where
    symTab = foldl' addClassVarDec [] varDecs
    nFieldsOfDec dec = case dec of (AField _ names) -> length names
                                   (AStatic _ _) -> 0
    nFields = sum $ map nFieldsOfDec varDecs

getFullName :: Maybe String -> String -> String
getFullName (Just cName) sName = cName ++ "." ++ sName
getFullName Nothing sName = sName

getLbl :: Context String
getLbl = do
  (fullName, lblCount) <- get
  put (fullName, lblCount + 1)
  return $ fullName ++ "##" ++ (show lblCount)

toSegment :: VarKind -> Segment
toSegment VStatic = Static
toSegment VField = This
toSegment VArgument = Argument
toSegment VLocal = Local
toSegment VPointer = Pointer

getPop :: SymTab -> String -> [Command]
getPop symTab var = case getSym symTab var of
  Just (SymVar _ _ vk idx) -> [CM $ Pop (toSegment vk) idx]
  Nothing -> error $ "Trying to pop to variable not in symbol table: " ++ var

getPush :: SymTab -> String -> [Command]
getPush symTab var = case getSym symTab var of
  Just (SymVar _ _ vk idx) -> [CM $ Push (toSegment vk) idx]
  Nothing -> error $ "Trying to push variable not in symbol table: " ++ var

compileStatement :: SymTab -> AStatement -> Context [Command]
compileStatement t (ALet var Nothing expr) =  return $ (compileExpr t expr) ++ (getPop t var)
compileStatement t (ALet var (Just idxExpr) expr) = return $ (getPush t var) ++
  (compileExpr t idxExpr) ++
  [CL Add] ++
  (compileExpr t expr) ++
  [CM $ Pop Temp 0, CM $ Pop Pointer 1, CM $ Push Temp 0, CM $ Pop That 0]

compileStatement t (ADo call) = return $ (compileCall t call) ++ [CM $ Pop Temp 0]

compileStatement t (AReturn Nothing) = return [CM $ Push Constant 0, CFun Return]
compileStatement t (AReturn (Just expr)) = return $ (compileExpr t expr) ++ [CFun Return]

compileStatement t (AWhile expr stmts) = do
  startLbl <- getLbl
  endLbl <- getLbl
  compiledBody <- sequence $ map (compileStatement t) stmts
  return $ [CF $ Label startLbl] ++
    (compileExpr t expr) ++
    [CL Not, CF $ IfGoto endLbl] ++
    (concat compiledBody) ++
    [CF $ Goto startLbl, CF $ Label endLbl]

compileStatement t (AIf expr ifStmts Nothing) = do
  endLbl <- getLbl
  compiledIf <- sequence $ map (compileStatement t) ifStmts
  return $ (compileExpr t expr) ++
    [CL Not, CF $ IfGoto endLbl] ++
    (concat compiledIf) ++
    [CF $ Label endLbl]
compileStatement t (AIf expr ifStmts (Just elseStmts)) = do
  elseLbl <- getLbl
  endLbl <- getLbl
  compiledIf <- sequence $ map (compileStatement t) ifStmts
  compiledElse <- sequence $ map (compileStatement t) elseStmts
  return $ (compileExpr t expr) ++
    [CL Not, CF $ IfGoto elseLbl] ++
    (concat compiledIf) ++
    [CF $ Goto endLbl, CF $ Label elseLbl] ++
    (concat compiledElse) ++
    [CF $ Label endLbl]

compileOp :: AOp -> [Command]
compileOp AOPlus = [CL Add]
compileOp AOMinus = [CL Sub]
compileOp AOAnd = [CL And]
compileOp AOBar = [CL Or]
compileOp AOLT = [CL Lt]
compileOp AOGT = [CL Gt]
compileOp AOEq = [CL Eq]
compileOp AOStar = [CFun $ Fun "Math.multiply" 2]
compileOp AOSlash = [CFun $ Fun "Math.divide" 2]

compileUOp :: AUOp -> [Command]
compileUOp AUOMinus = [CL Neg]
compileUOp AUOTilde = [CL Not]

compileExpr :: SymTab -> AExpression -> [Command]
compileExpr t (AExpression term opsTerms) = (compileTerm t term) ++ (concat $ map compPair opsTerms)
  where compPair (op, term2) = (compileTerm t term2) ++ (compileOp op)

compileCall :: SymTab -> ASubroutineCall -> [Command]
compileCall t (ASubroutineCall mClass sName exprs) = thisExpr ++
  (concat $ map (compileExpr t) exprs) ++
  [CFun $ Call (getFullName mClass sName) (length exprs + argAdd)]
  where (thisExpr, argAdd) = case mClass of Nothing -> ([], 0)
                                            Just cName -> case getSym t cName of Just _ -> (getPush t "this", 1)
                                                                                 Nothing -> ([], 0)

compileKey :: SymTab -> AKeywordConst -> [Command]
compileKey _ AKTrue = [CM $ Push Constant 1, CL Neg]
compileKey _ AKFalse = [CM $ Push Constant 0]
compileKey _ AKNull = [CM $ Push Constant 0]
compileKey t AKThis = getPush t "this"

compileTerm :: SymTab -> ATerm -> [Command]
compileTerm _ (AIntConst int) = [CM $ Push Constant int]
compileTerm t (AStrConst str) = newCall ++ (concat $ map appendCall str)
  where
    newCall = compileCall t $ ASubroutineCall (Just "String") "new" [AExpression (AIntConst $ length str) []]
    appendCall char = compileCall t $ ASubroutineCall (Just "String") "appendChar" [AExpression (AIntConst $ ord char) []]
compileTerm t (AKey key) = compileKey t key
compileTerm t (ASub call) = compileCall t call
compileTerm t (AParenExpr expr) = compileExpr t expr
compileTerm t (AUnaryOp uop term) = (compileTerm t term) ++ (compileUOp uop)
compileTerm t (AVarName var Nothing) = getPush t var
compileTerm t (AVarName var (Just idxExpr)) = (getPush t var) ++
  (compileExpr t idxExpr) ++
  [CL Add, CM $ Pop Pointer 1, CM $ Push That 0]

compileSubDec :: SymTab -> String -> Int -> ASubroutineDec -> [Command]
compileSubDec cTable cName nFields dec@(ASubroutineDec subKind ret sName params (ASubroutineBody localDecs stmts)) =
  [CFun $ Fun (getFullName (Just cName) sName) nLocals] ++ inits ++ (concat compiledBody)
  where
    symTab = addSubDec cTable cName dec
    nLocalsOfDec (AVarDec _ names) = length names
    nLocals = sum $ map nLocalsOfDec localDecs
    inits = case subKind of AMethod -> [CM $ Push Argument 0, CM $ Pop Pointer 0]
                            AConstructor -> compileCall symTab (ASubroutineCall (Just "Memory") "alloc" [AExpression (AIntConst nFields) []]) ++
                              [CM $ Pop Pointer 0]
                            AFunction -> []
    compiledBody = evalState (sequence $ map (compileStatement symTab) stmts) (getFullName (Just cName) sName, 0)

addMany :: (SymTab -> String -> SymTab) -> [String] -> SymTab
addMany update strings = foldl' update [] strings

addClassVarDec :: SymTab -> AClassVarDec -> SymTab
addClassVarDec table (AStatic theType names) = addMany (addSym VStatic theType) names
addClassVarDec table (AField theType names) = addMany (addSym VField theType) names

addVarDec :: SymTab -> AVarDec -> SymTab
addVarDec table (AVarDec theType names) = addMany (addSym VLocal theType) names

addParam :: SymTab -> AParameter -> SymTab
addParam table (AParameter theType name) = addSym VArgument theType table name

addSubDec :: SymTab -> String -> ASubroutineDec -> SymTab
addSubDec table cName (ASubroutineDec subKind _ _ params (ASubroutineBody localDecs _)) = withBody
  where
    withThis = if subKind /= AFunction then addSym VPointer (AClassName cName) table "this" else table
    withParams = foldl' addParam withThis params
    withBody = foldl' addVarDec withParams localDecs

