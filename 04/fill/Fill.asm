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

@INPUT
0;JMP
(DRAW)             // Writes whatever's in memory location PIXEL to every screen pixel
	@SCREEN
	D=A
	@COUNTER
	M=D        // store at memory location COUNTER the location of the current pixel
(DRAWLOOP)
	@8192
	D=A
	@SCREEN
	D=D+A
	@COUNTER	
	D=D-M
	@INPUT
	D;JEQ      // we're done if we've drawn all the pixels
	@PIXEL
	D=M        // otherwise put the PIXEL value in the D register
	@COUNTER   // load up the counter 
	A=M        // dereference it 
	M=D        // write the correct pixel value
	@COUNTER
	M=M+1      // increment the counter
	@DRAWLOOP  // then loop
	0;JMP
(INPUT)
	@KBD
	D=M        // put the keyboard value in the D register
	@ZERO
	D;JEQ      // if it's 0, then set PIXEL to 0
	@ONE
	0;JMP      // otherwise set it to 1
(ZERO)
	@PIXEL
	M=0
	@DRAW
	0;JMP
(ONE)
	@32767
	D=A
	@PIXEL
	M=D
	@DRAW
	0;JMP
