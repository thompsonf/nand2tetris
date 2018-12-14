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

prettyCommand :: Command -> String
prettyCommand (CL log) = prettyLog log
prettyCommand (CM mem) = prettyMem mem

prettyComment :: String -> String
prettyComment com = "// " ++ com