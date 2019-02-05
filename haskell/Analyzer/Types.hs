module Analyzer.Types where

data AClass = AClass String [AClassVarDec] [ASubroutineDec]
  deriving Show
data AClassVarDec = AStatic AType [String]
  | AField AType [String]
  deriving Show
data AType = AInt | AChar | ABoolean | AClassName String
  deriving Show
data AFuncReturn = AFuncReturnType AType | AFuncReturnVoid
  deriving Show
data ASubroutineDec = ASubroutineDec ASubroutineKind AFuncReturn String [AParameter] ASubroutineBody
  deriving Show
data ASubroutineKind = AConstructor | AFunction | AMethod
  deriving (Eq, Show)
data AParameter = AParameter AType String
  deriving Show
data ASubroutineBody = ASubroutineBody [AVarDec] [AStatement]
  deriving Show
data AVarDec = AVarDec AType [String]
  deriving Show

data AStatement = ALet String (Maybe AExpression) AExpression
  | AIf AExpression [AStatement] (Maybe [AStatement])
  | AWhile AExpression [AStatement]
  | ADo ASubroutineCall
  | AReturn (Maybe AExpression)
  deriving Show

data AExpression = AExpression ATerm [(AOp, ATerm)]
  deriving Show
data ATerm = AIntConst Int
  | AStrConst String
  | AKey AKeywordConst
  | AVarName String (Maybe AExpression)
  | ASub ASubroutineCall
  | AParenExpr AExpression
  | AUnaryOp AUOp ATerm -- minus is only unary op
  deriving Show

data ASubroutineCall = ASubroutineCall (Maybe String) String [AExpression]
  deriving Show
data AKeywordConst = AKTrue | AKFalse | AKNull | AKThis
  deriving Show
data AOp = AOPlus | AOMinus | AOStar | AOSlash | AOAnd | AOBar | AOLT | AOGT | AOEq
  deriving Show
data AUOp = AUOMinus | AUOTilde
  deriving Show