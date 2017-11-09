// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

@R2 
M=0       // set the initial product equal to 0
@R1       // get the multiplier
D=M       // and put it in D
@COUNTER
M=D       // `COUNTER` will count down the number of additions we have to do

(LOOP)
	@COUNTER
	D=M    // load the COUNTER value into D
	@END
	D;JEQ  // if the counter is 0, we're done
	@R0 
	D=M    // otherwise, put the multiplicand value into D
	@R2
	M=M+D  // and add it to R2
	@COUNTER
	M=M-1  // decrement the counter
	@LOOP
	0;JMP  // and loop
(END)
	@END
	0;JMP  // don't really know why we do this, but the book said to 
