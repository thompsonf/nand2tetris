//CFun (Fun "SimpleFunction.test" 2)
(SimpleFunction.test)
@SP
A=M
M=0
AD=A+1
M=0
AD=A+1
@SP
M=D
//CM (Push Local 0)
@R1
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
//CM (Push Local 1)
@R1
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
//CL Not
@SP
AM=M-1
M=!M
@SP
M=M+1
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
//CFun Return
@LCL
D=M
@R13
M=D
@5
A=D-A
D=M
@R14
M=D
@SP
A=M-1
D=M
@ARG
A=M
M=D
@ARG
D=M
@SP
M=D+1
@R13
AM=M-1
D=M
@THAT
M=D
@R13
AM=M-1
D=M
@THIS
M=D
@R13
AM=M-1
D=M
@ARG
M=D
@R13
AM=M-1
D=M
@LCL
M=D
@R14
A=M
0;JMP
