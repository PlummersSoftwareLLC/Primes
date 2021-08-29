; This implementation is a faithful implementation in x64 assembly.
; It can be used for sieve sizes up to 100,000,000; beyond that some register widths used will become too narrow for
; (first) the sqrt of the size, and then the size itself.

global main

extern printf
extern malloc
extern free
extern clock_gettime

default rel

struc time
    .sec:       resq    1
    .fract:     resq    1
endstruc

struc sieve
    .sieveSize: resd    1
    .primes:    resq    1
endstruc

section .data

SIEVE_SIZE      equ     1000000             ; sieve size
RUNTIME         equ     5                   ; target run time in seconds
TRUE            equ     1                   ; true constant
FALSE           equ     0                   ; false constant
NULL            equ     0                   ; null pointer
SEMICOLON       equ     59                  ; semicolon ascii
INIT_BLOCK      equ     0ffffffffffffffffh  ; init block for prime array

CLOCK_MONOTONIC equ     1                   ; CLOCK_MONOTONIC
WRITE           equ     1                   ; syscall number for write
STDOUT          equ     1                   ; file descriptor of stdout

MILLION         equ     1000000
BILLION         equ     1000000000

refResults:
                dd      10, 4
                dd      100, 25
                dd      1000, 168
                dd      10000, 1229
                dd      100000, 9592
                dd      1000000, 78498
                dd      10000000 ,664579
                dd      100000000, 5761455
                dd      0

; format string for output
outputFmt:      db      'joonicks_x64ff_unroll4', SEMICOLON, '%d', SEMICOLON, '%d.%03d', SEMICOLON, '1', SEMICOLON, 'algorithm=base,faithful=yes,bits=1', 10, 0
; incorrect result warning message
incorrect:      db      'WARNING: result is incorrect!', 10
; length of previous
incorrectLen:   db      $ - incorrect

section .bss

startTime:      resb    time_size           ; start time of sieve run
duration:       resb    time_size           ; duration
sizeSqrt:       resd    1                   ; square root of sieve size

section .text

main:

; registers (global variables):
; * r14d: runCount
; * r15: sievePtr (&sieve)

    xor         r14d, r14d

    mov         eax, SIEVE_SIZE             ; eax = sieve size
    cvtsi2sd    xmm0, eax                   ; xmm0 = eax
    sqrtsd      xmm0, xmm0                  ; xmm0 = sqrt(xmm0)
    cvttsd2si   eax, xmm0                   ; sizeSqrt = xmm0
    inc         eax                         ; sizeSqrt++, for safety
    mov         [sizeSqrt], eax             ; save sizeSqrt

    mov         rax, INIT_BLOCK             ; rax = INIT_BLOCK
    push        rax                         ; push rax to stack (3 times needed to align to 16 byte)
    push        rax
    push        rax
    movdqa      xmm0, [rsp]                 ; store init block in 128-bit xmm0 register (used in initLoop)
    add         rsp, 24                     ; restore stack pointer

; get start time
    mov         rdi, CLOCK_MONOTONIC        ; * ask for monotonic time
    lea         rsi, [startTime]            ; * struct to store result in
    call        clock_gettime wrt ..plt

    xor         r15, r15                    ; sievePtr = null

runLoop:
    cmp         r15, NULL                   ; if sievePtr == null...
    jz          createSieve                 ; ...skip deletion

    mov         rdi, r15                    ; pass sievePtr
    call        deleteSieve                 ; delete sieve

createSieve:
    mov         rdi, SIEVE_SIZE             ; pass sieve size
    call        newSieve                    ; rax = &sieve

    mov         r15, rax                    ; sievePtr = rax

    mov         rdi, r15                    ; pass sievePtr
    call        runSieve                    ; run sieve

; registers:
; * rax: numNanoseconds/numMilliseconds
; * rbx: numSeconds
; * r14d: runCount
; * r15: sievePtr (&sieve)

    mov         rdi, CLOCK_MONOTONIC        ; * ask for monotonic time
    lea         rsi, [duration]             ; * struct to store result in
    call        clock_gettime wrt ..plt

    mov         rbx, [duration+time.sec]    ; numSeconds = duration.seconds
    sub         rbx, [startTime+time.sec]   ; numSeconds -= startTime.seconds

    mov         rax, [duration+time.fract]  ; numNanoseconds = duration.fraction
    sub         rax, [startTime+time.fract] ; numNanoseconds -= startTime.fraction
    jns         checkTime                   ; if numNanoseconds >= 0 then check the duration...
    dec         rbx                         ; ...else numSeconds--...
    add         rax, BILLION                ; ...and numNanoseconds += 1,000,000,000

checkTime:
    inc         r14d                        ; runCount++
    cmp         rbx, RUNTIME                ; if numSeconds < 5...
    jl          runLoop                     ; ...perform another sieve run

; we're past the 5 second mark, so it's time to store the exact duration of our runs
    mov         [duration+time.sec], rbx    ; duration.seconds = numSeconds

    xor         edx, edx                    ; edx = 0
    mov         ecx, MILLION                ; ecx = 1,000,000
    div         ecx                         ; edx:eax /= ecx, so eax contains numMilliseconds

    mov         [duration+time.fract], rax  ; duration.fraction = numMilliseconds

; let's count our primes
    mov         rdi, r15                    ; pass sievePtr
    call        countPrimes                 ; rax = primeCount

; registers:
; * eax: primeCount
; * rcx: refResultPtr
    mov         rcx, refResults             ; refResultPtr = (int *)&refResults

checkLoop:
    cmp         dword [rcx], 0              ; if *refResults == 0 then we didn't find our sieve size, so...
    je          printWarning                ; ...warn about incorrect result
    cmp         dword [rcx], SIEVE_SIZE     ; if *refResults == sieve size...
    je          checkValue                  ; ...check the reference result value...
    add         rcx, 8                      ; ...else refResultsPtr += 2
    jmp         checkLoop                   ; keep looking for sieve size

checkValue:
    cmp         [rcx+4], eax                ; if *(refResultPtr + 1) == primeCount...
    je          printResults                ; ...print result

; if we're here, something's amiss with our outcome
printWarning:

    mov         rax, WRITE                  ; syscall to make, parameters:
    mov         rdi, STDOUT                 ; * write to stdout
    lea         rsi, [incorrect]            ; * message is warning
    movzx       rdx, byte [incorrectLen]    ; * length of message
    syscall

printResults:
    push        rbp                         ; align stack (SysV ABI requirement)
                                            ; parameters for call to printf:
    lea         rdi, [outputFmt]            ; * format string
    xor         rsi, rsi                    ; * clear...
    mov         esi, r14d                   ; ...and set runCount
    mov         rdx, [duration+time.sec]    ; * duration.seconds
    mov         rcx, [duration+time.fract]  ; * duration.fraction (milliseconds)
    xor         rax, rax                    ; rax = 0 (no argv)
    call        printf wrt ..plt

    pop         rbp                         ; restore stack
    xor         rax, rax                    ; return 0
    ret                                     ; end of main

; parameters:
; * rdi: sieve limit
; returns:
; * rax: &sieve
newSieve:
    mov         r12, rdi                    ; keep parameter, we'll need it later
    mov         rdi, sieve_size             ; ask for sieve_size bytes
    call        malloc wrt ..plt            ; rax = &sieve
    mov         [rax+sieve.sieveSize], r12d ; store sieve size parameter for faithfulness

; registers:
; * rax = primesPtr (&sieve.primes[0])
; * ecx = initBlockIndex
; * rdx = init_block
; * r12 = sievePtr (&sieve)
; * r13d = initBlockCount

    mov         r12, rax                    ; sievePtr = &sieve
    mov         r13d, [r12+sieve.sieveSize] ; sieveBytes = sieve.sieveSize
    shr         r13d, 7                     ; sieveBytes /= 128
    inc         r13d                        ; sieveBytes++
    shl         r13d, 4                     ; sieveBytes *= 16

    mov         edi, r13d                   ; ask sieveBytes
	add			edi, 10000
    call        malloc wrt ..plt            ; primesPtr = &array[0]
	mov         [r12+sieve.primes], rax     ; sieve.primes = rax

; initialize prime array
    xor         rcx, rcx                    ; byteCounter = 0
initLoop:
    movdqa      [rax+rcx], xmm0             ; sieve.primes[byteCounter][0..127] = true
    add         rcx, 16                     ; byteCounter += 16
    cmp         ecx, r13d                   ; if byteCounter < sieveBytes...
    jb          initLoop                    ; ...continue initialization
    mov         rax, r12                    ; return &sieve
    ret                                     ; end of newSieve

; parameters:
; * rdi: sievePtr (&sieve)
deleteSieve:
    mov         r12, rdi                    ; keep sievePtr, we'll need it later

    mov         rdi, [r12+sieve.primes]     ; ask to free sieve.primes
    call        free wrt ..plt

    mov         rdi, r12                    ; ask to free sieve
    call        free wrt ..plt

    ret                                     ; end of deleteSieve

;-------------------------------------------------------------------------------------------------------------------------------------
; parameters:
; * rdi: sievePtr (&sieve)
; returns:
; * &sieve.primes[0]
runSieve:
; used registers:
; * eax:  number
; * rsi:  primesPtr (&sieve.primes[0])
; * rcx:  clrBitNumber/getCurWord/clrRollBits
; * rdx:  clrBitSelect
; * r8:   factor
; * r9:   clrSkipValue
; * r10:  clrCurWord
; * r11d: sieveSize

	xor			rax, rax
    mov			rsi, [rdi+sieve.primes]     ; primesPtr = &sieve.primes[0]
    mov         r11d, [rdi+sieve.sieveSize] ; sieveSize = sieve.sieveSize
    mov         r8, 1						; factor = 1

sieveLoop:
	; thanks to sqrt checking, this only loops 500 times when searchspace is 1,000,000
	lea			r8d, [r8d+2]				; factor += 2 (skip even numbers)
	cmp			r8d, [sizeSqrt]				; factor >= sizeSqrt?
	jge			sieveFinal
	bt			[esi], r8d					; bit[factor] --> carry flag
	jnc			sieveLoop					; bit not set, move on to next factor
	
clearBitInit:
	lea			eax, [r8d]
    imul		eax, eax					; number = factor * factor

	mov			dl, 0xfd
	lea			ecx, [r8d*2]				; skipvalue = factor + factor

	lea			ebx, [eax+ecx*4]
	cmp			ebx, r11d
	jge			clearBitTail

	lea			edi, [ecx+ecx*2]			; rdi = skipvalue * 3

	bt			ecx, 2
	jnc			clearBitLoopSkip2
	
	; rax = starting number to clear
	; rbx = byte pointer conversion
	; rcx = skipvalue
	;  dl = bitmask
	; edi = skipvalue * 3
	; esi = (&sieve.primes[0])
	; r11 = sieveSize
clearBitLoopSkip6:
	; pattern progression for step 6: fd -> 7f -> df -> f7
	lea			ebx, [eax]					; number 1 = starter number
    shr         ebx, 3						; clrCurWord = numer / 8
    and         byte [esi + ebx], 0xfd		; clear the bit
	lea			ebx, [eax+ecx]				; number 2 = starter number + skipvalue
    shr         ebx, 3						; clrCurWord = numer / 8
    and         byte [esi + ebx], 0x7f		; clear the bit
    lea         ebx, [eax+ecx*2]			; number 3 = starter number + skipvalue * 2
    shr         ebx, 3						; clrCurWord = numer / 8
    and         byte [esi + ebx], 0xdf		; clear the bit
    lea         ebx, [eax+edi]				; number 4 = starter number + skipvalue * 3 (edi)
    shr         ebx, 3						; clrCurWord = numer / 8
    and         byte [esi + ebx], 0xf7		; clear the bit
	lea			eax, [eax+ecx*4]
    cmp         eax, r11d					; if number < sieveSize...
    jb          clearBitLoopSkip6			; ...continue clearing bits
	jmp			clearBitTail

clearBitLoopSkip2:
	; pattern progression for step 2: fd -> f7 -> df -> 7f
	lea			ebx, [eax]					; number 1 = starter number
    shr         ebx, 3						; clrCurWord = numer / 8
    and         byte [esi + ebx], 0xfd		; clear the bit
	lea			ebx, [eax+ecx]				; number 2 = starter number + skipvalue
    shr         ebx, 3						; clrCurWord = numer / 8
    and         byte [esi + ebx], 0xf7		; clear the bit
    lea         ebx, [eax+ecx*2]			; number 3 = starter number + skipvalue * 2
    shr         ebx, 3						; clrCurWord = numer / 8
    and         byte [esi + ebx], 0xdf		; clear the bit
    lea         ebx, [eax+edi]				; number 4 = starter number + skipvalue * 3 (edi)
    shr         ebx, 3						; clrCurWord = numer / 8
    and         byte [esi + ebx], 0x7f		; clear the bit
	lea			eax, [eax+ecx*4]
    cmp         eax, r11d					; if number < sieveSize...
    jb          clearBitLoopSkip2			; ...continue clearing bits
	
clearBitTail:
	lea			r10d, [eax]
	shr			r10d, 3
	and			[esi + r10d], dl
	rol			dl, cl
	add			eax, ecx
	cmp			eax, r11d
	jl			clearBitTail
    jmp         sieveLoop                   ; next factor

sieveFinal:
    lea         rax, [rsi]                  ; return &sieve.primes[0]
    ret                                     ; end of runSieve

; parameters:
; * rdi: sievePtr (&sieve)
; returns:
; * primeCount
countPrimes:

; registers:
; * eax: primeCount
; * rbx: primesPtr (&sieve.primes[0])
; * ecx: bitIndex

; This procedure could definitely be made faster by loading (q)words and shifting bits. As the counting of the
; primes is not included in the timed part of the implementation and executed just once, I just didn't bother.

    mov         rbx, [rdi+sieve.primes]     ; primesPtr = &sieve.primes[0]
    mov         eax, 1                      ; primeCount = 1
    mov         rcx, 3                      ; bitIndex = 3

countLoop:
    bt          [rbx], ecx                  ; if !sieve.primes[0][bitIndex]...
    jnc         nextItem                    ; ...move on to next array member
    inc         eax                         ; ...else primeCount++

nextItem:
    add         ecx, 2                      ; bitIndex += 2
    cmp         ecx, [rdi+sieve.sieveSize]  ; if bitIndex < sieveSize
    jb          countLoop                   ; ...continue counting

    ret                                     ; end of countPrimes
