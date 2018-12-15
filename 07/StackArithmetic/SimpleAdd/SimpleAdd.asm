// CM (Push Constant 7)
@7
D=A
@R0
A=M
M=D
@R0
M=M+1
// CM (Push Constant 8)
@8
D=A
@R0
A=M
M=D
@R0
M=M+1
// CL Add
@R0
AM=M-1
D=M
@R13
M=D
@R0
AM=M-1
D=M
@R13
D=D+M
@R0
A=M
M=D
@R0
M=M+1
