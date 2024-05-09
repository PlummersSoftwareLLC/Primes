;---------------------------------------------------------------------
;
;  Sieve of Eratosthenes benchmark for the 8080 processor that
;  duplicates the BASIC algorithm shown below.
;
;  This test runs in 0.6 seconds (6.0 seconds for 10 iterations)
;  on an Altair 8800 clocked at 2 MHz.
;
;  Mike Douglas, August 2023
;
;---------------------------------------------------------------------
;
; 10 REMARKABLE BASIC SIEVE BY DAVEPL
; 20 DIM A%(10000)
; 30 N = 10000
; 40 NSQ% = INT(SQR(N))
; 50 FOR I = 2 TO N
; 60     A%(I) = 1
; 70 NEXT I
; 80 FOR I = 2 TO NSQ%
; 90     IF A%(I) = 0 THEN GOTO 130
; 100    FOR J = I * 2 TO N STEP I
; 110        A%(J) = 0
; 120    NEXT J
; 130 NEXT I
; 140 COUNT% = 0
; 150 FOR I = 2 TO N
; 160     IF A%(I) = 1 THEN COUNT% = COUNT% + 1
; 170 NEXT I
; 180 PRINT "NUMBER OF PRIMES UP TO"; N; "IS:"; COUNT%
; 190 END
;
;-----------------------------------------------------------------------
ITERS	equ	10			;repeat test 10 times
N	equ	10000			;size of primes table
FLAGS	equ	0			;FLAGS array (A%) must be at zero

	org	4000h			;entry address is 4000h
start:	lxi	sp,STACK		;set a stack pointer
	
	lxi	h,mIters		;display "10 iterations"
	call	dspMsg

	mvi	a,ITERS			;init iteration counter
	sta	iterCnt

; Outer iteration loop (repeats the sieve test)

iterLp:	lxi	h,iterCnt		;count iterations of the test
	dcr	m
	jm	count			;display result after final run

; 50 FOR I = 2 TO N
; 60     A%(I) = 1
; 70 NEXT I

init:	lxi	h,FLAGS+2		;HL->FLAGS array
	mvi	d,1			;non-zero = TRUE
	lxi	b,N-1			;number of elements to init		

iniLoop:	mov	m,d			;set all elements of FLAGS true
	inx	h
	dcx	b			;decrement count of elements
	mov	a,b
	ora	c
	jnz	iniLoop			;loop for entire array
					;  (C=0 at this point)
; 80 FOR I = 2 TO NSQ%
; 90     IF A%(I) = 0 THEN GOTO 130

	lxi	h,N			;get B=int(sqrt(N))
	call	sqrt
	lxi	h,FLAGS+1		;HL->FLAGS array	

tblLoop:	inx	h			;move to next table entry
	mov	a,b	 		;reach the end?
	cmp	l			;done?
	jc	iterLp			;yes, run sieve again

	mov	a,m			;flag TRUE?
	ora	a
	jz	tblLoop			;no

; 100    FOR J = I * 2 TO N STEP I
; 110        A%(J) = 0
; 120    NEXT J

	mov	d,h			;DE=I
	mov	e,l
	dad	h			;HL=J=I*2

	mvi	a,(N/256)-1		;A=next to last page of FLAGS array
	
clrLoop:	cmp	h			;still before last page of FLAGS?
	jc	chkLast			;no
	mov	m,c			;else, A%(J)=0
	dad	d			;STEP I
	jmp	clrLoop	
	
chkLast:	inr	a			;on last page?
	cmp	h
	jnz	nextI			;beyond it, do NEXT I

	mvi	a,(N AND 0FFh)		;A=offset of last byte in last page
	
lastLp:	cmp	l			;past last byte?
	jc	nextI			;yes, do NEXT I
	mov	m,c			;else, A%(J)=0
	dad	d			;STEP I
	jmp	lastLp			;stay in last page loop until done

nextI:	xchg				;restore HL with I
	jmp	tblLoop

; 140 COUNT% = 0
; 150 FOR I = 2 TO N
; 160     IF A%(I) = 1 THEN COUNT% = COUNT% + 1
; 170 NEXT I

count:	lxi	d,0			;DE=COUNT
	lxi	h,FLAGS+1		;HL->FLAGS array
	lxi	b,N			;number of elements to check

cntLoop:	inx	h			;move to next element
	dcx	b			;searched all elements?
	mov	a,b
	ora	c
	jz	exit			;yes, done

	mov	a,m			;A%(I) <> 0?
	ora	a
	jz	cntLoop			;no
	inx	d			;else count the prime
	jmp	cntLoop	

; Display result in decimal and exit to CP/M

exit:	lxi	h,mPrimes		;display result and exit
	call	dspMsg
	xchg				;HL contains count
	call	dispHL			;display HL as integer

die:	hlt			;die here

;-----------------------------------------------------------
; sqrt - Compute int(square root of HL) and return in B
;-----------------------------------------------------------
sqrt:	lxi	d,1			;DE will go -1,-3,-5...
	mvi	b,(-1 AND 0FFh)		;B counts number of subtractions

sqrtLp:	dcx	d			;next odd integer
	dcx	d
	inr	b			;count subtractions
	dad	d
	jc	sqrtLp			;until HL crosses zero

	ret

;-----------------------------------------------------------
; dispHL - Display HL as decimal value (for HL < 10,000)
;-----------------------------------------------------------
dispHL:	lxi	d,-1000			;count 1000's
	mvi	a,'0'			;init 1000's digit

cnt1000:	dad	d			;subtract 1000
	jnc	do100			;went below zero
	inr	a
	jmp	cnt1000

do100:	lxi	d,1000			;put back extra 1000
	dad	d
	call	dspChar			;display it
	
	lxi	d,-100			;count 100s
	mvi	a,'0'

cnt100:	dad	d			;subtract 100
	jnc	do10			;went below zero
	inr	a
	jmp	cnt100

do10:	lxi	d,100			;put back extra 100
	dad	d
	call	dspChar			;display it

	mov	a,l
	mvi	b,'0'
	
cnt10:	sui	10
	jc	do1
	inr	b
	jmp	cnt10

do1:	adi	10+'0'
	mov	c,a
	mov	a,b
	call	dspChar
	
	mov	a,c
	jmp	dspChar			;display last digit and return

;-----------------------------------------------------------
; dspMsg - Display null terminated string from HL
;-----------------------------------------------------------
dspMsg:	mov	a,m			;get next character
	ora	a			;null?
	rz				;yes, exit
	call	dspChar	
	inx	h		
	jmp	dspMsg

;-----------------------------------------------------------
; dspChar - display character in A
;-----------------------------------------------------------
dspChar:	push	psw			;save char to display

dcWait:	in	10h			;88-2SIO status register
	ani	02h
	jz	dcWait

	pop	psw			;A=character to display
	out	11h			;88-2SIO data register
	ret

;-----------------------------------------------------------
; Data area
;-----------------------------------------------------------
mIters:	db	13,10,10,'10 iterations of primes through 10000',13,10,10,0
mPrimes:	db	'Primes found: ',0
iterCnt:	ds	1			;test counter
	ds	64			;stack space
STACk	equ	$

	end