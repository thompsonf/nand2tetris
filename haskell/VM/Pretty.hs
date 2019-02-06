module VM.Pretty (prettyCommand) where

import VM.Types

prettyLog :: Logical -> String
prettyLog Add = "add"
prettyLog Sub = "sub"
prettyLog Neg = "neg"
prettyLog Eq = "eq"
prettyLog Gt = "gt"
prettyLog Lt = "lt"
prettyLog And = "and"
prettyLog Or = "or"
prettyLog Not = "not"

prettySegment :: Segment -> String
prettySegment Local = "local"
prettySegment Argument = "argument"
prettySegment This = "this"
prettySegment That = "that"
prettySegment Constant = "constant"
prettySegment Static = "static"
prettySegment Temp = "temp"
prettySegment Pointer = "pointer"

prettyMem :: Memory -> String
prettyMem (Push seg int) = unwords ["push", prettySegment seg, show int]
prettyMem (Pop seg int) = unwords ["pop", prettySegment seg, show int]

prettyFlow :: Flow -> String
prettyFlow (Label lbl) = "label " ++ lbl
prettyFlow (Goto lbl) = "goto " ++ lbl
prettyFlow (IfGoto lbl) = "if-goto " ++ lbl

prettyFunction :: Function -> String
prettyFunction (Fun fun nLocals) = "function " ++ fun ++ " " ++ (show nLocals)
prettyFunction (Call fun nArgs) = "call " ++ fun ++ " " ++ (show  nArgs)
prettyFunction Return = "return"

prettyCommand :: Command -> String
prettyCommand (CL log) = prettyLog log
prettyCommand (CM mem) = prettyMem mem
prettyCommand (CF flow) = prettyFlow flow
prettyCommand (CFun fun) = prettyFunction fun

prettyComment :: String -> String
prettyComment com = "// " ++ com