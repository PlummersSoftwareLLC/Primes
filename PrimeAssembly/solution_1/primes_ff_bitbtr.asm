; This implementation is a faithful implementation in x64 assembly.
; It can be used for sieve sizes up to 100,000,000; beyond that some register widths used will become too narrow for
; (first) the sqrt of the size, and then the size itself.

global main

extern printf
extern malloc
extern free

default rel

struc time
    .sec:       resq    1
    .fract:     resq    1
endstruc

struc sieve
    .bitSize: resd    1
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

CLOCK_GETTIME   equ     228                 ; syscall number for clock_gettime
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
outputFmt:      db      'rbergen_x64ff_bitbtr', SEMICOLON, '%d', SEMICOLON, '%d.%03d', SEMICOLON, '1', SEMICOLON, 'algorithm=base,faithful=yes,bits=1', 10, 0   
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

; get start time
    mov         rax, CLOCK_GETTIME          ; syscall to make, parameters:
    mov         rdi, CLOCK_MONOTONIC        ; * ask for monotonic time
    lea         rsi, [startTime]            ; * struct to store result in
    syscall

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

    mov         rax, CLOCK_GETTIME          ; syscall to make, parameters:
    mov         rdi, CLOCK_MONOTONIC        ; * ask for monotonic time
    lea         rsi, [duration]             ; * struct to store result in
    syscall

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

    inc         r12d                        ; array_size = sieve limit + 1
    shr         r12d, 1                     ; array_size /= 2
    mov         [rax+sieve.bitSize], r12d   ; sieve.bitSize = array_size

; registers:
; * rax = primesPtr (&sieve.primes[0])
; * ecx = initBlockIndex
; * rdx = init_block
; * r12 = sievePtr (&sieve)
; * r13d = initBlockCount

    mov         r12, rax                    ; sievePtr = &sieve
    mov         r13d, [r12+sieve.bitSize]   ; initBlockCount = sieve.bitSize
    shr         r13d, 6                     ; initBlockCount /= 64
    inc         r13d                        ; initBlockCount++
    
    lea         rdi, [8*r13d]               ; ask for initBlockCount * 8 bytes
    call        malloc wrt ..plt            ; primesPtr = &array[0]

    mov         [r12+sieve.primes], rax     ; sieve.primes = rax

; initialize prime array   
    xor         rcx, rcx                    ; initBlockIndex = 0                       
    mov         rdx, INIT_BLOCK             ; init_block = INIT_BLOCK

initLoop:
    mov         [rax+8*rcx], rdx            ; sieve.primes[initBlockIndex*8][0..63] = true
    inc         ecx                         ; initBlockIndex++
    cmp         ecx, r13d                   ; if initBlockIndex < initBlockCount...
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

; parameters:
; * rdi: sievePtr (&sieve)
; returns:
; * &sieve.primes[0]
runSieve:

; registers:
; * eax: number
; * rbx: primesPtr (&sieve.primes[0])
; * rcx: wordIndex
; * r8d: factor
; * r9: curWord
; * r10: bitSelect
; * r13d: sizeSqrt

    mov         rbx, [rdi+sieve.primes]     ; primesPtr = &sieve.primes[0]
    mov         r13d, [sizeSqrt]            ; sizeSqrt = global.sizeSqrt
    mov         r8, 3                       ; factor = 3
    mov         rcx, 0                      ; wordIndex = 0
    mov         r10, 2                      ; bitSelect = 0b00000010

sieveLoop:
    mov         rax, r8                     ; number = factor...
    mul         r8d                         ; ... * factor
    shr         eax, 1                      ; number /= 2

; clear multiples of factor
unsetLoop:
    btr         [rbx], eax                  ; sieve.primes[0][number] = false
    add         eax, r8d                    ; number += factor
    cmp         eax, [rdi+sieve.bitSize]    ; if number < sieve.bitSize...
    jb          unsetLoop                   ; ...continue marking non-primes

; if the factor <= sqrt 129 then we (re)load the first qword of bits, because it was changed by the marking of non-primes 
    cmp         r8d, 11                     ; if factor > 11...
    ja          factorLoop                  ; ...we can start looking for the next factor...
    mov         r9, [rbx]                   ; ...else curWord = (long)sieve.primes[0]

; find next factor
factorLoop:
    add         r8d, 2                      ; factor += 2
    cmp         r8d, r13d                   ; if factor > sizeSqrt...
    ja          endRun                      ; ...end this run

    shl         r10, 1                      ; bitSelect <<= 1
    jnz         checkBit                    ; if bitSelect != 0 then check bit

; we just shifted the select bit out of the register, so we need to move on the next word
    inc         ecx                         ; wordIndex++
    mov         r10, 1                      ; bitSelect = 1
    mov         r9, [rbx+8*rcx]             ; curWord = (long)sieve.primes[8 * wordIndex]

checkBit:
    test        r9, r10                     ; if curWord & bitSelect != 0...
    jnz         sieveLoop                   ; ...continue this run
    jmp         factorLoop                  ; keep looking for next factor

endRun:
    lea         rax, [rbx]                  ; return &sieve.primes[0]

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
    mov         rcx, 1                      ; bitIndex = 1
    
countLoop:
    bt          [rbx], ecx                  ; if !sieve.primes[0][bitIndex]...
    jnc         nextItem                    ; ...move on to next array member
    inc         eax                         ; ...else primeCount++

nextItem:
    inc         ecx                         ; bitIndex++
    cmp         ecx, [rdi+sieve.bitSize]    ; if bitIndex < sieve.bitSize...
    jb          countLoop                   ; ...continue counting

    ret                                     ; end of countPrimes