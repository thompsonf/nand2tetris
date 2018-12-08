// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

// color that the screen was last painted, or will be painted
@color
M = 0

(INPUT)
@KBD
D = M
@PAINT_WHITE
D;JEQ

// PAINT_BLACK
@color
D = M
@INPUT
D;JNE
@color
M = -1
@FILL_SCREEN
0;JMP

(PAINT_WHITE)
@color
D = M
@INPUT
D;JEQ
@color
M = 0

(FILL_SCREEN)
// set screenregister to first address of screen memory
@SCREEN
D = A
@screen_register
M = D
// set i to 8192, total number of registers in screen memory
@8192
D = A
@i
M = D
(COLOR_REGISTER)
@color
D = M
@screen_register
A = M
M = D
@screen_register
M = M + 1
@i
M = M - 1
D = M
@INPUT
D;JEQ
@COLOR_REGISTER
0;JMP