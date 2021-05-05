; This implementation is the most straightforward sieve runner I could code in assembly, and as such unfaithful.
; It can be used for sieve sizes up to 100,000,000; beyond that some register widths used will become too narrow for
; (first) the sqrt of the size, and then the size itself.

global main
extern printf

struc time
    .sec:       resq    1
    .fract:     resq    1
endstruc

section .data

SIEVE_SIZE      equ     1000000                     ; sieve size
RUNTIME         equ     5                           ; target run time in seconds
ARRAY_SIZE      equ     (SIEVE_SIZE/2)+1            ; prime candidate array size
TRUE            equ     1                           ; true constant
FALSE           equ     0                           ; false constant
SEMICOLON       equ     59                          ; semicolon ascii

CLOCK_GETTIME   equ     228                         ; syscall number for clock_gettime
CLOCK_REALTIME  equ     0                           ; CLOCK_REALTIME
WRITE           equ     4                           ; syscall number for write
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
bPrimes:        resb    ARRAY_SIZE                  ; array with prime candidates

section .text

main:

; registers:
; * r12d: run count, throughout program

    xor         r12d,r12d

; registers: all except r12d operational

; get start time
    mov         rax, CLOCK_GETTIME                  ; syscall to make, parameters:
    mov         rdi, CLOCK_REALTIME                 ; * ask for real time
    mov         rsi, startTime                      ; * struct to store result in
    syscall
runLoop:

; registers:
; * ebx: factor
; * ecx: number
; * r8d: sizeSqrt
; * r9d: arrayIndex
; * r12d: runCount

; initialize prime array   
    mov         r9d, 1                              ; arrayIndex = 1                       

initLoop:
    mov         byte [bPrimes+r9d], TRUE            ; bPrimes[arrayIndex] = true
    inc         r9d                                 ; arrayIndex++
    cmp         r9d, ARRAY_SIZE                     ; if arrayIndex <= array size...
    jbe         initLoop                            ; ...continue initialization

; calculate square root of sieve size
    mov         eax, SIEVE_SIZE                     ; eax = sieve size
    cvtsi2sd    xmm0, eax                           ; xmm0 = eax
    sqrtsd      xmm0,xmm0                           ; xmm0 = sqrt(xmm0)
    cvttsd2si   r8d, xmm0                           ; sizeSqrt = xmm0 
    inc         r8d                                 ; sizeSqrt++, for safety 

; run the sieve
    mov         ebx, 3                              ; factor = 3

sieveLoop:
    mov         eax, ebx                            ; eax = ...
    mul         ebx                                 ; ... factor * factor
    mov         ecx, eax                            ; number = eax

; clear multiples of factor
unsetLoop:
    mov         r9d, ecx                            ; arrayIndex = number                         
    shr         r9d, 1                              ; arrayIndex /= 2
    mov         byte [bPrimes+r9d], FALSE           ; bPrimes[arrayIndex] = false
    add         ecx, ebx                            ; number += factor
    add         ecx, ebx                            ; number += factor
    cmp         ecx, SIEVE_SIZE                     ; if number <= sieve size...
    jbe         unsetLoop                           ; ...continue marking non-primes

; find next factor
factorLoop:
    add         ebx, 2                              ; factor += 2
    cmp         ebx, r8d                            ; if factor > sizeSqrt...
    ja          endRun                              ; ...end this run
    
    cmp         byte [bPrimes+ebx], TRUE             ; if bPrimes[factor]...
    je          sieveLoop                           ; ...continue run
    jmp         factorLoop                          ; continue looking

endRun:

; registers: 
; * (r/e)ax: numNanoseconds/numMilliseconds
; * (r/e)bx: numSeconds
; * r12d: runCount

    mov         rax, CLOCK_GETTIME                  ; syscall to make, parameters:
    mov         rdi, CLOCK_REALTIME                 ; * ask for real time
    mov         rsi, duration                       ; * struct to store result in
    syscall

    mov         rax, qword [duration+time.sec]      ; rax = duration.seconds
    sub         rax, qword [startTime+time.sec]     ; rax -= startTime.seconds
    mov         rbx, rax                            ; numSeconds = ax

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

; let's count our primes

; registers:
; * ebx: primeCount
; * ecx: arrayIndex
; * r12d: runCount

    xor         ebx, ebx                            ; primeCount = 0
    mov         ecx, 2                              ; arrayIndex = 2
    
countLoop:    
    cmp         byte [bPrimes+ecx], TRUE            ; if !bPrimes[cx]...
    jne         nextItem                            ; ...move on to next array member
    inc         ebx                                 ; ...else primeCount++

nextItem:
    inc         ecx                                 ; arrayIndex++
    cmp         ecx, ARRAY_SIZE                     ; if arrayIndex <= array size...
    jbe         countLoop                           ; ...continue counting

; we're done counting, let's check our result

; registers:
; * ebx: primeCount
; * rcx: refResultPtr
; * r12d: runCount
    mov         rcx, refResults                     ; refResultPtr = (int *)&refResults

checkLoop:
    cmp         dword [rcx], 0                      ; if *refResults == 0 then we didn't find our sieve size, so...
    je          printWarning                        ; ...warn about incorrect result
    cmp         dword [rcx], SIEVE_SIZE             ; if *refResults == sieve size...
    je          checkValue                          ; ...check the reference result value...
    add         rcx, 8                              ; ...else refResults += 2 
    jmp         checkLoop                           ; keep looking for sieve size

checkValue:
    cmp         dword [rcx+4], ebx                  ; if *(refResultPtr + 1) == primeCount... 
    je          printResults                        ; ...print result

; if we're here, something's amiss with our outcome
printWarning:
    mov         rax, WRITE                          ; syscall to make, parameters:
    mov         rdi, STDOUT                         ; * write to stdout
    mov         rsi, incorrect                      ; * message is warning
    movzx       rdx, byte [incorrectLen]            ; * length of message
    syscall

printResults:
    push        rbp
                                                    ; parameters for call to printf:
    mov         rdi, outputFmt                      ; * format string
    xor         rsi, rsi                            
    mov         esi, r12d                           ; * runCount
    mov         rdx, qword [duration+time.sec]      ; * duration.seconds
    mov         rcx, qword [duration+time.fract]    ; * duration.fraction (milliseconds)
    xor         eax, eax                            ; eax = 0 (no argv)
    call        printf                              

    pop         rbp

    xor         rax,rax

    ret                                             ; done!