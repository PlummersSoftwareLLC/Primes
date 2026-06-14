global main

extern printf
extern malloc
extern free
extern pthread_create
extern pthread_join
extern get_nprocs

default rel

; The benchmark state is heap-allocated once per program run and owns the
; shared runtime information for this multithreaded benchmark instance.
struc benchmark_state
    .start_sec:    resq 1
    .start_nsec:   resq 1
    .dur_sec:      resq 1
    .dur_ms:       resd 1
    .thread_count: resd 1
    .handles_ptr:  resq 1
    .workers_ptr:  resq 1
endstruc

; Each worker owns the full runtime state of the sieve it is executing:
; sieve metadata, pass count, and the current/final sieve buffer pointer.
struc worker_state
    .bench_ptr:    resq 1
    .sieve_size:   resd 1
    .bit_count:    resd 1
    .word_count:   resd 1
    .pad:          resd 1
    .pass_count:   resq 1
    .sieve_ptr:    resq 1
endstruc

section .data

SIEVE_SIZE       equ 1000000
RUNTIME          equ 5
CLOCK_GETTIME    equ 228
CLOCK_MONOTONIC  equ 1
STDOUT           equ 1
SYS_WRITE        equ 1
EXPECTED_COUNT   equ 78498

fmt  db "cwager_x64ff_mt;%d;%d.%03d;%d;algorithm=base,faithful=yes,bits=1",10,0
warn db "WARNING: result is incorrect",10
warn_len equ $ - warn

section .text

main:
    push rbp
    mov rbp, rsp
    push r12
    push r13
    push r14
    push r15

    ; Allocate the top-level benchmark object dynamically to keep the
    ; benchmark/runtime state out of static globals.
    mov edi, benchmark_state_size
    call malloc wrt ..plt
    test rax, rax
    jz .fail
    mov r12, rax

    call get_nprocs wrt ..plt
    test eax, eax
    jg .have_threads
    mov eax, 1

.have_threads:
    mov [r12 + benchmark_state.thread_count], eax

    mov edi, eax
    shl edi, 3
    call malloc wrt ..plt
    test rax, rax
    jz .free_bench
    mov [r12 + benchmark_state.handles_ptr], rax

    mov eax, [r12 + benchmark_state.thread_count]
    imul edi, eax, worker_state_size
    call malloc wrt ..plt
    test rax, rax
    jz .free_handles
    mov [r12 + benchmark_state.workers_ptr], rax

    mov rdi, r12
    call init_benchmark

    mov rdi, r12
    call run_benchmark

    mov r13, rax                       ; total passes

    mov rdi, r12
    call validate_result

    lea rdi, [fmt]
    mov esi, r13d
    mov rdx, [r12 + benchmark_state.dur_sec]
    mov ecx, [r12 + benchmark_state.dur_ms]
    mov r8d, [r12 + benchmark_state.thread_count]
    xor eax, eax
    call printf wrt ..plt

    mov rdi, r12
    call cleanup_benchmark

    xor eax, eax
    jmp .done

.free_handles:
    mov rdi, [r12 + benchmark_state.handles_ptr]
    call free wrt ..plt

.free_bench:
    mov rdi, r12
    call free wrt ..plt

.fail:
    mov eax, 1

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbp
    ret

init_benchmark:
    push r12
    push r13

    mov r12, rdi
    mov r13, [r12 + benchmark_state.workers_ptr]
    mov ecx, [r12 + benchmark_state.thread_count]

.worker_init_loop:
    test ecx, ecx
    jz .time_init

    ; Initialize each worker object with runtime sieve metadata so the
    ; sieve implementation reads its state from the object, not constants.
    mov [r13 + worker_state.bench_ptr], r12
    mov dword [r13 + worker_state.sieve_size], SIEVE_SIZE
    mov eax, SIEVE_SIZE
    shr eax, 1
    mov [r13 + worker_state.bit_count], eax
    add eax, 63
    shr eax, 6
    mov [r13 + worker_state.word_count], eax
    mov qword [r13 + worker_state.pass_count], 0
    mov qword [r13 + worker_state.sieve_ptr], 0
    add r13, worker_state_size
    dec ecx
    jmp .worker_init_loop

.time_init:
    sub rsp, 16
    mov eax, CLOCK_GETTIME
    mov edi, CLOCK_MONOTONIC
    mov rsi, rsp
    syscall
    mov rax, [rsp]
    mov [r12 + benchmark_state.start_sec], rax
    mov rax, [rsp + 8]
    mov [r12 + benchmark_state.start_nsec], rax
    add rsp, 16

    pop r13
    pop r12
    ret

run_benchmark:
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r12, rdi
    mov r13, [r12 + benchmark_state.handles_ptr]
    mov r14, [r12 + benchmark_state.workers_ptr]
    xor r15d, r15d                     ; created count

.create_loop:
    cmp r15d, [r12 + benchmark_state.thread_count]
    jae .join_phase

    lea rdi, [r13 + r15*8]
    xor esi, esi
    lea rdx, [rel worker_main]
    mov rcx, r15
    lea rcx, [rcx + rcx*4]
    shl rcx, 3
    lea rcx, [r14 + rcx]
    call pthread_create wrt ..plt
    test eax, eax
    jnz .join_phase

    inc r15d
    jmp .create_loop

.join_phase:
    xor ebx, ebx
    xor rax, rax

.join_loop:
    cmp ebx, r15d
    jae .timing

    mov rdi, [r13 + rbx*8]
    xor esi, esi
    call pthread_join wrt ..plt
    inc ebx
    jmp .join_loop

.timing:
    sub rsp, 16
    mov eax, CLOCK_GETTIME
    mov edi, CLOCK_MONOTONIC
    mov rsi, rsp
    syscall
    mov rax, [rsp]
    mov rdx, [rsp + 8]
    add rsp, 16

    sub rax, [r12 + benchmark_state.start_sec]
    sub rdx, [r12 + benchmark_state.start_nsec]
    jns .time_ok
    dec rax
    add rdx, 1000000000

.time_ok:
    mov [r12 + benchmark_state.dur_sec], rax
    mov rcx, 1000000
    mov r10, rax
    mov rax, rdx
    xor edx, edx
    div rcx
    mov [r12 + benchmark_state.dur_ms], eax
    mov [r12 + benchmark_state.dur_sec], r10

    xor eax, eax                       ; total passes
    mov r14, [r12 + benchmark_state.workers_ptr]
    mov ecx, [r12 + benchmark_state.thread_count]

.sum_loop:
    test ecx, ecx
    jz .return

    add rax, [r14 + worker_state.pass_count]
    add r14, worker_state_size
    dec ecx
    jmp .sum_loop

.return:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

validate_result:
    push r12
    push r13
    push r14

    mov r12, rdi
    mov r13, [r12 + benchmark_state.workers_ptr]
    mov r14d, [r12 + benchmark_state.thread_count]

.find_sieve:
    test r14d, r14d
    jz .done

    mov rdi, [r13 + worker_state.sieve_ptr]
    test rdi, rdi
    jnz .check
    add r13, worker_state_size
    dec r14d
    jmp .find_sieve

.check:
    mov rdi, r13
    call count_primes
    cmp eax, EXPECTED_COUNT
    je .done

    mov eax, SYS_WRITE
    mov edi, STDOUT
    lea rsi, [warn]
    mov edx, warn_len
    syscall

.done:
    pop r14
    pop r13
    pop r12
    ret

cleanup_benchmark:
    push r12
    push r13
    push r14

    mov r12, rdi
    mov r13, [r12 + benchmark_state.workers_ptr]
    mov r14d, [r12 + benchmark_state.thread_count]

.free_sieves:
    test r14d, r14d
    jz .free_arrays

    mov rdi, [r13 + worker_state.sieve_ptr]
    test rdi, rdi
    jz .next_sieve
    call free wrt ..plt

.next_sieve:
    add r13, worker_state_size
    dec r14d
    jmp .free_sieves

.free_arrays:
    mov rdi, [r12 + benchmark_state.workers_ptr]
    call free wrt ..plt
    mov rdi, [r12 + benchmark_state.handles_ptr]
    call free wrt ..plt
    mov rdi, r12
    call free wrt ..plt

    pop r14
    pop r13
    pop r12
    ret

worker_main:
    push rbp
    mov rbp, rsp
    push r12
    push r13
    push r14
    push r15

    mov r15, rdi                       ; worker_state*
    mov r14, [r15 + worker_state.bench_ptr]
    xor r13d, r13d

.loop:
    ; Each timed pass recreates a fresh sieve buffer from scratch.
    mov edi, [r15 + worker_state.word_count]
    shl edi, 3
    call malloc wrt ..plt
    test rax, rax
    jz .store
    mov r12, rax
    mov [r15 + worker_state.sieve_ptr], r12

    mov rdi, r12
    mov ecx, [r15 + worker_state.word_count]
    mov rax, -1
    rep stosq

    ; Run the base sieve using the worker-owned runtime sieve state.
    mov rdi, r15
    call run_sieve

    inc r13

    mov rdi, r14
    call elapsed_ge_runtime
    test eax, eax
    jnz .keep_sieve

    mov rdi, r12
    call free wrt ..plt
    mov qword [r15 + worker_state.sieve_ptr], 0
    jmp .loop

.keep_sieve:
    jmp .store

.store:
    mov [r15 + worker_state.pass_count], r13
    xor eax, eax
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbp
    ret

elapsed_ge_runtime:
    mov r8, rdi
    sub rsp, 16
    mov eax, CLOCK_GETTIME
    mov edi, CLOCK_MONOTONIC
    mov rsi, rsp
    syscall
    mov rax, [rsp]
    mov rdx, [rsp + 8]
    add rsp, 16

    sub rax, [r8 + benchmark_state.start_sec]
    sub rdx, [r8 + benchmark_state.start_nsec]
    jns .time_ok
    dec rax
    add rdx, 1000000000

.time_ok:
    xor ecx, ecx
    cmp rax, RUNTIME
    setae cl
    mov eax, ecx
    ret

run_sieve:
    push r12
    push r13
    push r14
    push r15

    ; The worker object is the closest assembly equivalent of the sieve
    ; class: it carries the runtime metadata and buffer used by the sieve.
    mov r15, rdi
    mov r14, [r15 + worker_state.sieve_ptr]
    mov r12d, [r15 + worker_state.bit_count]

    mov r8d, 3
    mov r10, 2
    xor edi, edi
    mov r13, [r14]
    mov r11d, 4

.outer:
    cmp r11d, r12d
    jae .done

    test r13, r10
    jz .next

    cmp r8d, 65
    jb .mark_small

.mark_large:
    mov eax, r11d
    mov edx, eax
    shr edx, 6

    mov ecx, eax
    and ecx, 63

    lea rsi, [r14 + rdx*8]
    mov r9, [rsi]

.inner_large:
    btr r9, rcx

    add eax, r8d
    cmp eax, r12d
    jae .store_exit

    add ecx, r8d
    mov edx, ecx
    shr edx, 6
    and ecx, 63
    test edx, edx
    jz .inner_large

    mov [rsi], r9
    lea rsi, [rsi + rdx*8]
    mov r9, [rsi]
    jmp .inner_large

.mark_small:
    mov eax, r11d
    mov edx, eax
    shr edx, 6

    mov ecx, eax
    and ecx, 63

    lea rsi, [r14 + rdx*8]
    mov r9, [rsi]

.inner_small:
    btr r9, rcx

    add eax, r8d
    cmp eax, r12d
    jae .store_exit

    add ecx, r8d
    cmp ecx, 64
    jae .cross_small

    btr r9, rcx

    add eax, r8d
    cmp eax, r12d
    jae .store_exit

    add ecx, r8d
    cmp ecx, 64
    jae .cross_small

    btr r9, rcx

    add eax, r8d
    cmp eax, r12d
    jae .store_exit

    add ecx, r8d
    cmp ecx, 64
    jae .cross_small

    btr r9, rcx

    add eax, r8d
    cmp eax, r12d
    jae .store_exit

    add ecx, r8d
    cmp ecx, 64
    jb .inner_small

.cross_small:
    sub ecx, 64
    mov [rsi], r9
    add rsi, 8
    mov r9, [rsi]
    jmp .inner_small

.store_exit:
    mov [rsi], r9

.next:
    cmp r8d, 11
    ja .advance
    mov r13, [r14]

.advance:
    lea r11d, [r11d + r8d*2 + 2]
    add r8d, 2
    shl r10, 1
    jnz .outer

    inc edi
    mov r10, 1
    mov r13, [r14 + rdi*8]
    jmp .outer

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    ret

count_primes:
    mov rsi, [rdi + worker_state.sieve_ptr]
    mov eax, 1
    mov ecx, 1

.count_loop:
    bt [rsi], ecx
    jnc .count_next
    inc eax

.count_next:
    inc ecx
    cmp ecx, [rdi + worker_state.bit_count]
    jb .count_loop

    ret

section .note.GNU-stack noalloc noexec nowrite progbits
