global main
extern printf
extern clock_gettime
extern malloc

struc time
    .sec:   resq 1
    .nsec:  resq 1
endstruc

section .data

SIEVE_SIZE  equ     1000000

clockErrorString:       db      "Error getting time",0xA
clockErrorSize          equ     $ - clockErrorString
mallocErrorString:      db      "Error allocating memory",0xA
mallocErrorSize         equ     $ - mallocErrorString
primesErrorString:      db      "Incorrect number of primes detected",0xA
primesErrorSize:        equ     $ - primesErrorString
result:                 db      "weirddan455;%d;%f;1;algorithm=base,faithful=yes,bits=1",0xA,0
billion:                dq      1000000000.0

section .bss

startingTime:   resb    time_size
currentTime:    resb    time_size

section .text

main:

push    rbp

mov     rdi, 4
lea     rsi, [startingTime]
call    clock_gettime
test    rax, rax
jnz     clockError

mov     r12, [startingTime+time.sec]
mov     r13, [startingTime+time.nsec]

mov     rdi, 125000
call    malloc
test    rax, rax
jz      mallocError
mov     r15, rax

xor     rbx, rbx

mov     r14, 0xffffffffffffffff
push    r14
push    r14
movdqa  xmm0, [rsp]

add     rsp, 16

loopStart:

inc     rbx
xor     rbp, rbp

resetBitArray:

movdqa  [r15 + rbp], xmm0
add     rbp, 16
cmp     rbp, 124992
jne     resetBitArray

mov     [r15 + rbp], r14

mov     rbp, 3

runSieveLoop:

mov     rdi, rbp

getBitLoop:

call    getBit
jnz     getBitEarlyEnd
add     rdi, 2
cmp     rdi, SIEVE_SIZE
jl      getBitLoop
jmp     clearBitInit

getBitEarlyEnd:

mov     rbp, rdi

clearBitInit:

mov     rdi, rbp
imul    rdi, rdi
cmp     rdi, SIEVE_SIZE
jge     clearBitEnd

mov     cl, dil
and     cl, 31

mov     eax, 1
shl     eax, cl
not     eax

mov     r8, rbp
add     r8, r8

mov     cl, r8b
and     cl, 31

clearBitLoop:

mov     r9, rdi
shr     r9, 5

and     [r15 + r9 * 4], eax
rol     eax, cl

add     rdi, r8
cmp     rdi, SIEVE_SIZE
jl      clearBitLoop

clearBitEnd:

add     rbp, 2
cmp     rbp, 1000
jle     runSieveLoop

mov     rdi, 4
lea     rsi, [currentTime]
call    clock_gettime
test    rax, rax
jnz     clockError

mov     rax, [currentTime+time.sec]
sub     rax, r12
cmp     rax, 5
jl      loopStart

cmp     rax, 6
jge     loopEnd

cmp     [currentTime+time.nsec], r13
jl      loopStart

loopEnd:

cvtsi2sd xmm0, [currentTime+time.sec]
cvtsi2sd xmm1, [currentTime+time.nsec]
cvtsi2sd xmm2, r12
cvtsi2sd xmm3, r13
movsd   xmm4, [billion]

divsd   xmm1, xmm4
divsd   xmm3, xmm4
addsd   xmm0, xmm1
addsd   xmm2, xmm3
subsd   xmm0, xmm2

mov     al, 1
lea     rdi, [result]
mov     rsi, rbx
call    printf

xor     rsi, rsi
mov     rdi, 1

countPrimes:

call    getBit
jz      notPrime
inc     rsi

notPrime:

add     rdi, 2
cmp     rdi, SIEVE_SIZE
jl      countPrimes

cmp     rsi, 78498
jne     incorrectPrimesError

pop     rbp
xor     rax, rax
ret

getBit:

mov     r8, rdi
shr     r8, 5

mov     cl, dil
and     cl, 31

mov     eax, 1
shl     eax, cl

and     eax, [r15 + r8 * 4]

ret

clockError:

mov     rax, 1
mov     rdi, 1
lea     rsi, [clockErrorString]
mov     rdx, clockErrorSize
syscall
pop     rbp
mov     rax, 1
ret

mallocError:

mov     rax, 1
mov     rdi, 1
lea     rsi, [mallocErrorString]
mov     rdx, mallocErrorSize
syscall
pop     rbp
mov     rax, 1
ret

incorrectPrimesError:

mov     rax, 1
mov     rdi, 1
lea     rsi, [primesErrorString]
mov     rdx, primesErrorSize
syscall
pop     rbp
mov     rax, 1
ret
