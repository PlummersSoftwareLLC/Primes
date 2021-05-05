; This implementation is the most straightforward sieve runner I could code in assembly, and as such unfaithful.
; It can be used for sieve sizes up to 100,000,000; beyond that some register widths used will become too narrow for
; (first) the sqrt of the size, and then the size itself

global _start
extern printf

struc timespec
    .sec:       resq    1
    .fract:     resq    1
endstruc

section .data

SIEVE_SIZE      equ     1000000                     ; sieve size
ARRAY_SIZE      equ     (SIEVE_SIZE/2)+1
TRUE            equ     1                           ; true constant
FALSE           equ     0                           ; false constant
SEMICOLON       equ     59                          ; semicolon ascii

CLOCK_GETTIME   equ     228                         ; syscall number for clock_gettime
CLOCK_REALTIME  equ     0                           ; CLOCK_REALTIME
WRITE           equ     4
STDOUT          equ     1

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


outputFmt:      db      'rbergen_amd64', SEMICOLON, '%d', SEMICOLON, '%d,%03d', SEMICOLON, '1', 10, 0   ; format string for output
incorrect:      db      'WARNING: result is incorrect!', 10
incorrectLen:   db      $-incorrect

section .bss

startTime:     resb    timespec_size               ; start time of sieve run
duration:       resb    timespec_size
bPrimes:        resb    ARRAY_SIZE                  ; array with primes


section .text

_start:

; registers: all operational

; get start time
    mov         rax, CLOCK_GETTIME                  ; syscall to make
    mov         rdi, CLOCK_REALTIME                 ; ask for real time
    mov         rsi, startTime                     ; struct to store result in
    syscall
runLoop:

; registers:
; * bx: factor
; * ecx: number
; * r8w: sqrt of size
; * r9d: prime array index

; initialize prime array   
    mov         ecx, 1                              ; ecx = array index = 1                       
initLoop:
    mov         byte [bPrimes+ecx], TRUE            ; bPrimes[ecx] = TRUE
    inc         ecx                                 ; ecx++
    cmp         ecx, ARRAY_SIZE                     ; if ecx <= last index...
    jbe         initLoop                             ; ...continue initialization

; calculate square root of sieve size
    mov         ecx, SIEVE_SIZE                           ; ecx = size
    cvtsi2sd    xmm0, ecx                           ; xmm0 = ecx
    sqrtsd      xmm0,xmm0                           ; xmm0 = sqrt(xmm0)
    cvttsd2si   r8d, xmm0                           ; r8d = square root of size = xmm0 
    inc         r8d                                 ; r8d++, for safety 

; initialize sieve run
    mov         ebx, 3                              ; (e)bx = factor = 3

sieveLoop:
    mov         ax, bx                              ; dx:ax = ...
    mul         bx                                  ; ... factor * factor
    mov         cx, dx                              ; get 2 higher bytes of number (dx)...
    shl         ecx, 16                             ; ...shift them left...
    mov         cx, ax                              ; and add lower bytes (ax)

; clear multiples of factor
unsetLoop:
    mov         r9d, ecx                            ; r9d = number                         
    shr         r9d, 1                              ; r9d /= 2
    mov         byte [bPrimes+r9d], FALSE           ; bPrimes[r9d] = FALSE
    add         ecx, ebx                            ; number += factor
    add         ecx, ebx                            ; number += factor
    cmp         ecx, SIEVE_SIZE                           ; if number <= size...
    jbe         unsetLoop                           ; ...continue marking non-primes

    add         bx, 2                               ; factor += 2
    cmp         bx, r8w                             ; if factor > sqrt(size)...
    ja          endRun                              ; ...end this run
    mov         cx, bx                              ; number = faxtor

factorLoop:
    cmp         byte [bPrimes+cx], TRUE             ; if bPrimes[cx]...
    je          nextFactor                          ; ...we found the next factor
    add         cx, 2                               ; number += 2
    cmp         cx, r8w                             ; if number <= sqrt(size)
    jbe         factorLoop                          ; ...keep looking
    jmp         endRun                              ; end this run

nextFactor:
    mov         bx, cx                              ; factor = number
    jmp         sieveLoop                           ; continue run


endRun:

; registers: 
; * (r/e)ax: duration fraction
; * (r/e)bx: duration seconds

    mov         rax, CLOCK_GETTIME                  ; syscall to make
    mov         rdi, CLOCK_REALTIME                 ; ask for real time
    mov         rsi, duration                     ; struct to store result in
    syscall

    mov         rax, qword [duration+timespec.sec]
    sub         rax, qword [startTime+timespec.sec]
    mov         bx, ax

    mov         rax, qword [duration+timespec.fract]      
    sub         rax, qword [startTime+timespec.fract]
    jns         checkTime
    dec         bx
    add         rax, BILLION

checkTime:
    cmp         bx, 5
    jl          runLoop

; we're past the 5 second mark, so it's time to store the exact duration of our runs
    mov         qword [duration+timespec.sec], bx

    mov         rdx, rax
    shr         rdx, 32
    mov         ecx, MILLION
    div         ecx

    mov         qword [duration+timespec.fract], cx

; let's count our primes


; registers:
; * ebx: counted primes
; * ecx: array index

    xor         ebx, ebx
    mov         ecx, 2
    
countLoop:    
    cmp         byte [bPrimes+ecx], TRUE             ; if bPrimes[cx]...
    jne         nextItem
    inc         ebx       

nextItem:
    inc         ecx
    cmp         ecx, ARRAY_SIZE
    jbe         countLoop

; we're done counting, let's check our results
    mov         ecx, refResults

checkLoop:
    cmp         dword [ecx], 0
    je          printWarning   
    cmp         dword [ecx], SIEVE_SIZE    
    je          checkValue
    add         ecx, 8
    jmp         checkLoop

checkValue:
    cmp         ebx, dword [ecx+4]
    je          printResults

printWarning:
    mov         rax, WRITE
    mov         rdi, STDOUT
    mov         rsi, incorrect
    movzx       rdx, byte [incorrectLen]

printResults:



