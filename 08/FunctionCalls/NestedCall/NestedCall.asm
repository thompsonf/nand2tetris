//CFun (Fun "Sys.init" 0)
(Sys.init)
//CM (Push Constant 4000)
@4000
D=A
@SP
A=M
M=D
@SP
M=M+1
//CM (Pop Pointer 0)
@R3
D=A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
//CM (Push Constant 5000)
@5000
D=A
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
//CFun (Call "Sys.main" 0)
@Sys.init$ret.0
D=A
@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1
@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1
@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1
@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
D=M
@5
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@Sys.main
0;JMP
(Sys.init$ret.0)
//CM (Pop Temp 1)
@6
D=A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
//CF (Label "LOOP")
(Sys.init$LOOP)
//CF (Goto "LOOP")
@Sys.init$LOOP
0;JMP
//CFun (Fun "Sys.main" 5)
(Sys.main)
@SP
A=M
M=0
AD=A+1
M=0
AD=A+1
M=0
AD=A+1
M=0
AD=A+1
M=0
AD=A+1
@SP
M=D
//CM (Push Constant 4001)
@4001
D=A
@SP
A=M
M=D
@SP
M=M+1
//CM (Pop Pointer 0)
@R3
D=A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
//CM (Push Constant 5001)
@5001
D=A
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
//CM (Push Constant 200)
@200
D=A
@SP
A=M
M=D
@SP
M=M+1
//CM (Pop Local 1)
@R1
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
//CM (Push Constant 40)
@40
D=A
@SP
A=M
M=D
@SP
M=M+1
//CM (Pop Local 2)
@R1
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
//CM (Push Constant 6)
@6
D=A
@SP
A=M
M=D
@SP
M=M+1
//CM (Pop Local 3)
@R1
D=M
@3
D=D+A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
//CM (Push Constant 123)
@123
D=A
@SP
A=M
M=D
@SP
M=M+1
//CFun (Call "Sys.add12" 1)
@Sys.main$ret.1
D=A
@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1
@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1
@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1
@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
D=M
@6
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@Sys.add12
0;JMP
(Sys.main$ret.1)
//CM (Pop Temp 0)
@5
D=A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
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
//CM (Push Local 2)
@R1
D=M
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
//CM (Push Local 3)
@R1
D=M
@3
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
//CM (Push Local 4)
@R1
D=M
@4
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
//CFun (Fun "Sys.add12" 0)
(Sys.add12)
//CM (Push Constant 4002)
@4002
D=A
@SP
A=M
M=D
@SP
M=M+1
//CM (Pop Pointer 0)
@R3
D=A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
//CM (Push Constant 5002)
@5002
D=A
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
//CM (Push Constant 12)
@12
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
