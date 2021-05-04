; bx: factor
; ecx: counter
; r8w: sqrt of size

global _start

struc t_timespec
    .tv_sec:    resq    1
    .number:    resq    1
endstruc

section .data

SIZE            equ     1000000                     ; sieve size
SV_START        equ     3                           ; first prime
TRUE            equ     1                           ; true constant
FALSE           equ     0                           ; false constant
SEMICOLON       equ     59                          ; semicolon ascii

CLOCK_GETTIME   equ     228                         ; syscall number for clock_gettime
CLOCK_REALTIME  equ     0                           ; CLOCK_REALTIME


fmt             db      'rbergen_amd64', SEMICOLON, '%d', SEMICOLON, '%.3f', SEMICOLON, '1', 10   ; format string for output


section .bss

start_time:     resb    t_timespec_size             ; start time of sieve run
cur_time:       resb    t_timespec_size             ; current time
bPrimes:        resb    (SIZE/2)+1                  ; array with primes


section .text

_start:

; get start time
    mov         rax, CLOCK_GETTIME                  ; syscall to make
    mov         rdi, CLOCK_REALTIME                 ; ask for real time
    mov         rsi, start_time                     ; struct to store result in
    syscall

runLoop:

; initialize prime array   
    mov         ecx, 1                              ; start setting prime array to TRUE at 1                       
 initLoop:
    mov         byte [bPrimes+ecx], TRUE            ; set current array item             
    inc         ecx
    cmp         ecx, (SIZE/2)+1
    jbe         initLoop

; calculate square root of sieve size
    dec         ecx
    cvtsi2sd    xmm0, ecx
    sqrtsd      xmm0,xmm0
    cvttsd2si   r8d, xmm0

; initialize sieve run
    mov         ecx, SV_START
    mov         
sieveLoop:
    mov         ax, bx
    mul         bx

