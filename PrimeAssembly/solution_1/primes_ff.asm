; This implementation is the most straightforward sieve runner I could code in assembly, and as such unfaithful.
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

struct sieve
    .limit:     resd    1
    .arraySize  resd    1
    .primes:    resq    1
endstruc

section .data

SIEVE_SIZE      equ     1000000                     ; sieve size
RUNTIME         equ     5                           ; target run time in seconds
TRUE            equ     1                           ; true constant
FALSE           equ     0                           ; false constant
NULL            equ     0                           ; null pointer
SEMICOLON       equ     59                          ; semicolon ascii
INIT_BLOCK      equ     0101010101010101h           ; init block for prime array            

CLOCK_GETTIME   equ     228                         ; syscall number for clock_gettime
CLOCK_MONOTONIC equ     1                           ; CLOCK_MONOTONIC
WRITE           equ     1                           ; syscall number for write
STDOUT          equ     1                           ; file descriptor of stdout
EXIT            equ     60                          ; syscall number for exit

MILLION         equ     1000000
BILLION         equ     1000000000

refResults:
                dd      10
                dd      4
                dd      100
                dd      25
                dd      1000
                dd      168
                dd      10000
                dd      1229
                dd      100000
                dd      9592
                dd      1000000
                dd      78498
                dd      10000000
                dd      664579
                dd      100000000
                dd      5761455
                dd      0

; format string for output
outputFmt:      db      'rbergen_x64uff', SEMICOLON, '%d', SEMICOLON, '%d.%03d', SEMICOLON, '1', 10, 0   
; incorrect result warning message
incorrect:      db      'WARNING: result is incorrect!', 10
; length of previous
incorrectLen:   db      $ - incorrect

section .bss

startTime:      resb    time_size                   ; start time of sieve run
duration:       resb    time_size                   ; duration

section .text

main:

; registers (global variables):
; * r12d: run count, throughout program
; * r13d: sizeSqrt (square root of sieve size)

    xor         r12d, r12d

    mov         eax, SIEVE_SIZE                     ; eax = sieve size
    cvtsi2sd    xmm0, eax                           ; xmm0 = eax
    sqrtsd      xmm0, xmm0                          ; xmm0 = sqrt(xmm0)
    cvttsd2si   r13d, xmm0                          ; sizeSqrt = xmm0 
    inc         r13d                                ; sizeSqrt++, for safety 


; registers:
; * rcx: sievePtr

; get start time
    mov         rax, CLOCK_GETTIME                  ; syscall to make, parameters:
    mov         rdi, CLOCK_MONOTONIC                ; * ask for real time
    lea         rsi, [startTime]                    ; * struct to store result in
    syscall

    xor         rcx, rcx

runLoop:
    cmp         rcx, 0                              ; if sievePtr == NULL...
    jz          createSieve                         ; ...skip deletion    
    
    mov         rdi, rcx                            ; pass sievePtr
    call        deleteSieve                         ; delete sieve

createSieve:    
    mov         rdi, SIEVE_SIZE                     ; pass sieve size
    call        newSieve                            ; rax = &sieve

    mov         rcx, rax                            ; sievPtr = rax

    mov         rdi, rcx                            ; pass sievePtr
    call        runSieve                            ; run sieve

; registers: 
; * rax: numNanoseconds/numMilliseconds
; * rbx: numSeconds
; * rcx: sievePtr

    mov         rax, CLOCK_GETTIME                  ; syscall to make, parameters:
    mov         rdi, CLOCK_MONOTONIC                ; * ask for monotonic time
    lea         rsi, [duration]                     ; * struct to store result in
    syscall

    mov         rbx, qword [duration+time.sec]      ; rbx = duration.seconds
    sub         rbx, qword [startTime+time.sec]     ; rbx -= startTime.seconds

    mov         rax, qword [duration+time.fract]    ; numNanoseconds = duration.fraction    
    sub         rax, qword [startTime+time.fract]   ; numNanoseconds -= startTime.fraction
    jns         checkTime                           ; if numNanoseconds >= 0 then check the duration...
    dec         rbx                                 ; ...else numSeconds--...
    add         rax, BILLION                        ; ...and numNanoseconds += 1,000,000,000

checkTime:
    inc         r12d                                ; runCount++
    cmp         rbx, RUNTIME                        ; if numSeconds < 5...
    jl          runLoop                             ; ...perform another sieve run

; we're past the 5 second mark, so it's time to store the exact duration of our runs
    mov         qword [duration+time.sec], rbx      ; duration.seconds = numSeconds

    mov         ecx, MILLION                        ; ecx = 1,000,000
    div         ecx                                 ; eax /= ecx, so eax contains numMilliseconds

    mov         qword [duration+time.fract], rax    ; duration.fraction = numMilliseconds

    mov         rdi, rcx                            ; pass sievePtr
    call        countPrimes                         ; rax = primeCount

; registers:
; * eax: primeCount
; * rcx: refResultPtr
    mov         rcx, refResults                     ; refResultPtr = (int *)&refResults

checkLoop:
    cmp         dword [rcx], 0                      ; if *refResults == 0 then we didn't find our sieve size, so...
    je          printWarning                        ; ...warn about incorrect result
    cmp         dword [rcx], SIEVE_SIZE             ; if *refResults == sieve size...
    je          checkValue                          ; ...check the reference result value...
    add         rcx, 8                              ; ...else refResults += 2 
    jmp         checkLoop                           ; keep looking for sieve size

checkValue:
    cmp         dword [rcx+4], eax                  ; if *(refResultPtr + 1) == primeCount... 
    je          printResults                        ; ...print result

; if we're here, something's amiss with our outcome
printWarning:

    mov         rax, WRITE                          ; syscall to make, parameters:
    mov         rdi, STDOUT                         ; * write to stdout
    lea         rsi, [incorrect]                    ; * message is warning
    movzx       rdx, byte [incorrectLen]            ; * length of message
    syscall

printResults:
    push        rbp                                 ; align stack (SysV ABI requirement)
                                                    ; parameters for call to printf:
    lea         rdi, [outputFmt]                    ; * format string
    xor         rsi, rsi                            
    mov         esi, r12d                           ; * runCount
    mov         rdx, qword [duration+time.sec]      ; * duration.seconds
    mov         rcx, qword [duration+time.fract]    ; * duration.fraction (milliseconds)
    xor         eax, eax                            ; eax = 0 (no argv)
    call        printf                              

    pop         rbp                                 ; restore stack

    xor         rax, rax                            ; return 0

    ret                                             ; end of main

; parameters:
; * rdi: sieve size
; returns:
; * rax: &sieve
newSieve:
    mov         r8, rdi                             ; keep parameter, we'll need it later

    mov         rdi, sieve_size                     ; ask for sieve_size bytes
    call        malloc                              ; rax = &sieve

    mov         dword [rax+sieve.limit], r8d        ; sieve.limit = save sieve size (limit)
    shr         r8d, 1                              ; array_size = sieve.limit / 2
    inc         r8d                                 ; array_size++
    mov         dword [rax+sieve.arraySize], r8d    ; sieve.arraySize = array_size

; registers:
; * r8 = sievePtr
; * eax = initBlockCount
; * ecx = initBlockIndex
    mov         r8, rax                             ; sievePtr = &sieve
    mov         eax, dword [r8+sieve.arraySize]     ; initBlockCount = sieve.arraySize
    shr         eax, 3                              ; initBlockCount /= 8
    inc         eax                                 ; initBlockCount++
    
    lea         rdi, [8*eax]                        ; ask for initBlockCount * 8 bytes
    call        malloc                              ; rax = &array[0]

    mov         qword [r8+sieve.primes], rax        ; sieve.primes = rax

    ; initialize prime array   
    mov         ecx, 0                              ; initBlockIndex = 0                       
    mov         rdx, INIT_BLOCK                     ; rdx = &init_block

initLoop:
    mov         qword [r8+8*ecx+sieve.primes], rdx  ; sieve.primes[initBlockIndex*8..(initBlockIndex*8 + 7)] = true
    inc         ecx                                 ; initBlockIndex++
    cmp         ecx, eax                            ; if initBlockIndex < initBlockCount...
    jb          initLoop                            ; ...continue initialization

    mov         rax, r8                             ; return &sieve

    ret                                             ; end of newSieve

; parameters:
; * rdi: sievePtr
deleteSieve:
    mov         r8, rdi                             ; keep sievePtr, we'll need it later

    mov         rdi, [r8+sieve.primes]              ; ask to free sieve.primes
    call        free

    mov         rdi, r8                             ; ask to free sieve
    call        free         

    ret                                             ; end of deleteSieve

; parameters:
; * rdi: sievePtr
; returns:
; * &sieve.primes[0]
runSieve:

; registers:
; * eax: number
; * ecx: factor
; * edx: arrayIndex
; * r13d: sizeSqrt (global)

    mov         ecx, 3                              ; factor = 3

sieveLoop:
    mov         eax, ecx                            ; number = ...
    mul         ecx                                 ; ... factor * factor

; clear multiples of factor
unsetLoop:
    mov         edx, eax                            ; arrayIndex = number                         
    shr         edx, 1                              ; arrayIndex /= 2
    mov         byte [rdi+edx+sieve.primes], FALSE  ; sieve.primes[arrayIndex] = false
    lea         eax, [eax, 2*ecx]                   ; number += 2*factor
    cmp         eax, [rdi+sieve.limit]              ; if number <= sieve.limit...
    jbe         unsetLoop                           ; ...continue marking non-primes

; find next factor
factorLoop:
    add         ecx, 2                              ; factor += 2
    cmp         ecx, r13d                           ; if factor > sizeSqrt...
    ja          endRun                              ; ...end this run
    
    mov         edx, ecx                            ; arrayIndex = factor
    shr         edx, 1                              ; arrayIndex /= 2
    cmp         byte [rdi+edx+sieve.primes], TRUE   ; if bPrimes[factor]...
    je          sieveLoop                           ; ...continue run
    jmp         factorLoop                          ; continue looking

    lea         rax, [rdi+sieve.primes]             ; return &sieve.primes[0]

    ret                                             ; end of runSieve


; parameters:
; * rdi: sievePtr
; returns:
; * primeCount
countPrimes:

; registers:
; * eax: primeCount
; * ecx: arrayIndex

    xor         eax, eax                            ; primeCount = 0
    mov         ecx, 2                              ; arrayIndex = 2
    
countLoop:    
    cmp         byte [bPrimes+ecx], TRUE            ; if !bPrimes[cx]...
    jne         nextItem                            ; ...move on to next array member
    inc         eax                                 ; ...else primeCount++

nextItem:
    inc         ecx                                 ; arrayIndex++
    cmp         ecx, ARRAY_SIZE                     ; if arrayIndex <= array size...
    jbe         countLoop                           ; ...continue counting

    ret                                             ; end of countPrimes