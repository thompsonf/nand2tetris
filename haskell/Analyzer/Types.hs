module Analyzer.Types where

data AClass = AClass String [AClassVarDec] [ASubroutineDec]
data AClassVarDec = AStatic AType [String]
  | AField AType [String]
data AType = AInt | AChar | ABoolean | AClassName String
data AFuncReturn = AFuncReturnType AType | AFuncReturnVoid
data ASubroutineDec = AConstructor AFuncReturn String [AParameter] ASubroutineBody
data AParameter = AParameter AType String
data ASubroutineBody = ASubroutineBody [AVarDec] [AStatement]
data AVarDec = AType [String]

data AStatement = ALet String (Maybe AExpression) AExpression
  | AIf AExpression [AStatement] [AStatement]
  | AWhile AExpression [AStatement]
  | ADo ASubroutineCall
  | AReturn (Maybe AExpression)

data AExpression = ATerm [(AOp, ATerm)]
data ATerm = AIntConst Int
  | AStrConst String
  | AKeywordConst
  | AVarName (Maybe AExpression)
  | ASub ASubroutineCall
  -- Need AParenExpression?
  | AUnaryOp ATerm -- minus is only unary op

data ASubroutineCall = ASubroutineCall (Maybe String) String [AExpression]
data AKeywordConst = AKTrue | AKFalse | AKNull | AKThis
data AOp = AOPlus | AOMinus | AOStar | AOSlash | AOAnd | AOBar | AOLT | AOGT | AOEq