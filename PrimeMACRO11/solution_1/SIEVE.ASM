;---------------------------------------------------------------------
	.TITLE  SIEVE            	 ; Program title
;---------------------------------------------------------------------

	.MCALL  .PRINT,.EXIT,.TTYOUT     ; System macros

;-- String Table -----------------------------------------------------

HELLOMSG:       .ASCIZ  /Sieve of Eratosthenes by Davepl 2024/
DASHESMSG:      .ASCIZ  /----------------------------------/
CBITSMSG:       .ASCIZ  /Clearing byte array.  Sieve Size:/
MSG2:           .ASCIZ  /Setup complete/
RUNMSG:         .ASCIZ  /Running sieve/
DONEMSG:        .ASCIZ  /Sieve complete/
PRIMMSG:        .ASCIZ  /Prime numbers found:/
MARKCMPMSG:     .ASCIZ  /Marking composite: /
CHECKCMPMSG:    .ASCIZ  /Checking if composite: /
EMPTYMSG:       .ASCIZ  //

;-- Constants ---------------------------------------------------------

LIMIT   =       1000.             ; Upper limit for primes
BSIZE   =       1000.             ; Size of byte array (fixed value)

        .EVEN                     ; Ensure we're on a word boundary

;-- Code Entry --------------------------------------------------------

START:  .PRINT  #HELLOMSG
        .PRINT  #DASHESMSG
        .PRINT  #CBITSMSG
        MOV     #LIMIT, R0
        JSR     PC, PRNUM
        .PRINT  #EMPTYMSG

;-- Clear the byte array -----------------------------------------------

        MOV     #BSIZE, R1
        CLR     R2
INITL:
        CLRB    BYTEARR(R2)
        INC     R2
        CMP     R2, R1
        BNE     INITL

        .PRINT  #MSG2
        .PRINT  #RUNMSG

;-- Run the sieve ------------------------------------------------------

        MOV     #3, R1            ; Start with 3 (first odd prime)
SIEVE:
        CMP     R1, #LIMIT        ; Check if we've reached the limit
        BGE     DONE.SV           ; If so, we're done

; Debug Output
;        .PRINT  #CHECKCMPMSG	  ; Print the number we're checking next
;        MOV     R1, R0
;        JSR     PC, PRNUM
;        .PRINT  #EMPTYMSG

        JSR     PC, ISCOMP        ; Check if R1 is composite
        BNE     NXTODD            ; If prime, skip to next odd number

        MOV     R1, R2            ; R2 = R1 (prime number found)
MARK:
        ADD     R1, R2            ; R2 += R1 (next multiple)
        CMP     R2, #LIMIT        ; Check if we've exceeded the limit
        BGE     NXTODD            ; If so, move to next odd number

; Debug Output
;        .PRINT  #MARKCMPMSG	  ; Print the multiple we're marking next
;        MOV     R2, R0
;        JSR     PC, PRNUM
;        .PRINT  #EMPTYMSG

        JSR     PC, SETCMP        ; Mark R2 as composite
        BR      MARK              ; Continue marking multiples

NXTODD:
        ADD     #2, R1            ; Move to next odd number
        BR      SIEVE             ; Continue sieving

DONE.SV:
        .PRINT  #DONEMSG
        .PRINT  #PRIMMSG

;-- Print prime numbers ------------------------------------------------

	MOV	#1, R3		  ; Prime count in R3 

        MOV     #2, R0            ; Start with 2 (only even prime)
        JSR     PC, PRNUM
        MOV     #',, R0
        .TTYOUT R0

        MOV     #3, R1            ; Start checking odd numbers from 3
PRNLP:
        CMP     R1, #LIMIT        ; Check if we've reached the limit
        BGE     DONE              ; If so, we're done

        JSR     PC, ISCOMP        ; Check if R1 is composite
        BNE     PRNXT             ; If prime, skip to next odd number

        MOV     R1, R0
        JSR     PC, PRNUM         ; Print the prime number
	INC	R3		  ; Bump the count
        MOV     #',, R0
        .TTYOUT R0
PRNXT:
        ADD     #2, R1            ; Move to next odd number
        BR      PRNLP             ; Continue printing primes

DONE:	.PRINT	EMPTYMSG
	MOV	R3, R0		  ; Print count of primes
	JSR	PC, PRNUM

        .EXIT                     ; Exit program

;-- Subroutines --------------------------------------------------------

ISCOMP:
        MOV     R1, R0
        MOVB    BYTEARR(R0), R2   ; Load byte from byte array
        TSTB    R2                ; Check if byte is non-zero
        BEQ     ISPRIME           ; If byte is zero, number is prime
        MOV     #1, R0            ; Return 1 if composite
        RTS     PC
ISPRIME:
        CLR     R0                ; Return 0 if prime
        RTS     PC

SETCMP:
        MOV     R2, R0
        MOVB    #1, BYTEARR(R0)   ; Set the byte in the array
        RTS     PC

PRNUM:
BTOA:
        MOV     R0, -(SP)         ; Save R0 on stack
        MOV     R1, -(SP)         ; Save R1 on stack
        MOV     R2, -(SP)         ; Save R2 on stack

        MOV     R0, R1            ; Move number to R1 (low part of dividend)
        CLR     R0                ; Clear R0 (high part of dividend)
        DIV     #10., R0          ; Divide R0:R1 by 10, quotient in R0, remainder in R1

        TST     R0                ; Check if quotient is 0
        BEQ     PRINT             ; If quotient is 0, print digit
        JSR     PC, BTOA          ; Recursive call with quotient

PRINT:  ADD     #'0, R1           ; Convert remainder to ASCII
        MOV     R1, R0            ; Move ASCII digit to R0
        .TTYOUT R0                ; Print the digit

        MOV     (SP)+, R2         ; Restore R2
        MOV     (SP)+, R1         ; Restore R1
        MOV     (SP)+, R0         ; Restore R0
        RTS     PC                ; Return


BYTEARR: .BLKB   BSIZE            ; Byte array for sieve

        .END    START

