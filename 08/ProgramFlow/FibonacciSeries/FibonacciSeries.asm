//CM (Push Argument 1)
@R2
D=M
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
//CM (Pop Pointer 1)
@R4
D=A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
//CM (Push Constant 0)
@0
D=A
@SP
A=M
M=D
@SP
M=M+1
//CM (Pop That 0)
@R4
D=M
@0
D=D+A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
//CM (Push Constant 1)
@1
D=A
@SP
A=M
M=D
@SP
M=M+1
//CM (Pop That 1)
@R4
D=M
@1
D=D+A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
//CM (Push Argument 0)
@R2
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
//CM (Push Constant 2)
@2
D=A
@SP
A=M
M=D
@SP
M=M+1
//CL Sub
@SP
AM=M-1
D=M
@R13
M=D
@SP
AM=M-1
D=M
@R13
D=D-M
@SP
A=M
M=D
@SP
M=M+1
//CM (Pop Argument 0)
@R2
D=M
@0
D=D+A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
//CF (Label "MAIN_LOOP_START")
(BOOTSTRAP$MAIN_LOOP_START)
//CM (Push Argument 0)
@R2
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
//CF (IfGoto "COMPUTE_ELEMENT")
@SP
AM=M-1
D=M
@BOOTSTRAP$COMPUTE_ELEMENT
D;JNE
//CF (Goto "END_PROGRAM")
@BOOTSTRAP$END_PROGRAM
0;JMP
//CF (Label "COMPUTE_ELEMENT")
(BOOTSTRAP$COMPUTE_ELEMENT)
//CM (Push That 0)
@R4
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
//CM (Push That 1)
@R4
D=M
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
//CL Add
@SP
AM=M-1
D=M
@R13
M=D
@SP
AM=M-1
D=M
@R13
D=D+M
@SP
A=M
M=D
@SP
M=M+1
//CM (Pop That 2)
@R4
D=M
@2
D=D+A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
//CM (Push Pointer 1)
@R4
D=M
@SP
A=M
M=D
@SP
M=M+1
//CM (Push Constant 1)
@1
D=A
@SP
A=M
M=D
@SP
M=M+1
//CL Add
@SP
AM=M-1
D=M
@R13
M=D
@SP
AM=M-1
D=M
@R13
D=D+M
@SP
A=M
M=D
@SP
M=M+1
//CM (Pop Pointer 1)
@R4
D=A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
//CM (Push Argument 0)
@R2
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
//CM (Push Constant 1)
@1
D=A
@SP
A=M
M=D
@SP
M=M+1
//CL Sub
@SP
AM=M-1
D=M
@R13
M=D
@SP
AM=M-1
D=M
@R13
D=D-M
@SP
A=M
M=D
@SP
M=M+1
//CM (Pop Argument 0)
@R2
D=M
@0
D=D+A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
//CF (Goto "MAIN_LOOP_START")
@BOOTSTRAP$MAIN_LOOP_START
0;JMP
//CF (Label "END_PROGRAM")
(BOOTSTRAP$END_PROGRAM)
