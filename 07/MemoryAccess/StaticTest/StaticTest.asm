// CM (Push Constant 111)
@111
D=A
@R0
A=M
M=D
@R0
M=M+1
// CM (Push Constant 333)
@333
D=A
@R0
A=M
M=D
@R0
M=M+1
// CM (Push Constant 888)
@888
D=A
@R0
A=M
M=D
@R0
M=M+1
// CM (Pop Static 8)
@StaticTest.8
D=A
@R13
M=D
@R0
AM=M-1
D=M
@R13
A=M
M=D
// CM (Pop Static 3)
@StaticTest.3
D=A
@R13
M=D
@R0
AM=M-1
D=M
@R13
A=M
M=D
// CM (Pop Static 1)
@StaticTest.1
D=A
@R13
M=D
@R0
AM=M-1
D=M
@R13
A=M
M=D
// CM (Push Static 3)
@StaticTest.3
D=M
@R0
A=M
M=D
@R0
M=M+1
// CM (Push Static 1)
@StaticTest.1
D=M
@R0
A=M
M=D
@R0
M=M+1
// CL Sub
@R0
AM=M-1
D=M
@R13
M=D
@R0
AM=M-1
D=M
@R13
D=D-M
@R0
A=M
M=D
@R0
M=M+1
// CM (Push Static 8)
@StaticTest.8
D=M
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
