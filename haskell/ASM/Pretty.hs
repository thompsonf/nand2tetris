module ASM.Pretty (prettyLine) where

import ASM.Types

prettyAInst :: AInst -> String
prettyAInst (AInstSym sym) = '@':sym
prettyAInst (AInstNum num) = '@':(show num)

prettyDest :: Dest -> String
prettyDest NullDest = ""
prettyDest DestA = "A"
prettyDest DestD = "D"
prettyDest DestM = "M"
prettyDest AD = "AD"
prettyDest AM = "AM"
prettyDest DM = "DM"
prettyDest ADM = "ADM"

prettyComp :: Comp -> String
prettyComp Zero = "0"
prettyComp One = "1"
prettyComp NegOne = "-1"
prettyComp A = "A"
prettyComp D = "D"
prettyComp M = "M"
prettyComp NotA = "!A"
prettyComp NotD = "!D"
prettyComp NotM = "!M"
prettyComp NegA = "-A"
prettyComp NegD = "-D"
prettyComp NegM = "-M"
prettyComp APlusOne = "A+1"
prettyComp DPlusOne = "D+1"
prettyComp MPlusOne = "M+1"
prettyComp AMinusOne = "A-1"
prettyComp DMinusOne = "D-1"
prettyComp MMinusOne = "M-1"
prettyComp APlusD = "A+D"
prettyComp DPlusM = "D+M"
prettyComp AMinusD = "A-D"
prettyComp DMinusA = "D-A"
prettyComp DMinusM = "D-M"
prettyComp MMinusD = "M-D"
prettyComp AAndD = "A&D"
prettyComp DAndM = "D&M"
prettyComp DOrA = "D|A"
prettyComp DOrM = "D|M"

prettyJump :: Jump -> String
prettyJump NullJump = ""
prettyJump JGT = "JGT"
prettyJump JEQ = "JEQ"
prettyJump JGE = "JGE"
prettyJump JLT = "JLT"
prettyJump JNE = "JNE"
prettyJump JLE = "JLE"
prettyJump JMP = "JMP"

prettyCInst :: CInst -> String
prettyCInst (CInst dest comp jump) = pd ++ middle ++ end
  where
    pd = prettyDest dest
    pc = prettyComp comp
    middle = if pd == "" then pc else ('=':pc)
    pj = prettyJump jump
    end = if pj == "" then "" else (';':pj)

prettyLabel :: String -> String
prettyLabel lbl = '(':(lbl ++ ")")

prettyComment :: String -> String
prettyComment "" = ""
prettyComment com = '/':'/':com

prettyCommentPost :: String -> String
prettyCommentPost com = if pc == "" then "" else (' ':pc)
  where pc = prettyComment com

prettyLine :: Line -> String
prettyLine (LineE com) = prettyComment com
prettyLine (LineL lbl com) = (prettyLabel lbl) ++ prettyCommentPost com
prettyLine (LineA ainst com) = (prettyAInst ainst) ++ prettyCommentPost com
prettyLine (LineC cinst com) = (prettyCInst cinst) ++ prettyCommentPost com