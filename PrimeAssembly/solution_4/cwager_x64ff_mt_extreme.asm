global main

extern printf
extern malloc
extern free
extern clock_gettime
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

fmt  db "cwager_x64ff_mt_extreme;%d;%d.%03d;%d;algorithm=base,faithful=yes,bits=1",10,0
warn db "WARNING: result is incorrect",10
warn_len equ $ - warn

dense_jump_table:
    ; Uniform dense dispatch for every odd skip in the contiguous range
    ; 3..129 inclusive. Composite entries are present as well as prime
    ; entries; composite entries are normally not reached at runtime because
    ; earlier discovered factors have already cleared those candidates.
    dq run_sieve_dense_003
    dq run_sieve_dense_005
    dq run_sieve_dense_007
    dq run_sieve_dense_009
    dq run_sieve_dense_011
    dq run_sieve_dense_013
    dq run_sieve_dense_015
    dq run_sieve_dense_017
    dq run_sieve_dense_019
    dq run_sieve_dense_021
    dq run_sieve_dense_023
    dq run_sieve_dense_025
    dq run_sieve_dense_027
    dq run_sieve_dense_029
    dq run_sieve_dense_031
    dq run_sieve_dense_033
    dq run_sieve_dense_035
    dq run_sieve_dense_037
    dq run_sieve_dense_039
    dq run_sieve_dense_041
    dq run_sieve_dense_043
    dq run_sieve_dense_045
    dq run_sieve_dense_047
    dq run_sieve_dense_049
    dq run_sieve_dense_051
    dq run_sieve_dense_053
    dq run_sieve_dense_055
    dq run_sieve_dense_057
    dq run_sieve_dense_059
    dq run_sieve_dense_061
    dq run_sieve_dense_063
    dq run_sieve_dense_065
    dq run_sieve_dense_067
    dq run_sieve_dense_069
    dq run_sieve_dense_071
    dq run_sieve_dense_073
    dq run_sieve_dense_075
    dq run_sieve_dense_077
    dq run_sieve_dense_079
    dq run_sieve_dense_081
    dq run_sieve_dense_083
    dq run_sieve_dense_085
    dq run_sieve_dense_087
    dq run_sieve_dense_089
    dq run_sieve_dense_091
    dq run_sieve_dense_093
    dq run_sieve_dense_095
    dq run_sieve_dense_097
    dq run_sieve_dense_099
    dq run_sieve_dense_101
    dq run_sieve_dense_103
    dq run_sieve_dense_105
    dq run_sieve_dense_107
    dq run_sieve_dense_109
    dq run_sieve_dense_111
    dq run_sieve_dense_113
    dq run_sieve_dense_115
    dq run_sieve_dense_117
    dq run_sieve_dense_119
    dq run_sieve_dense_121
    dq run_sieve_dense_123
    dq run_sieve_dense_125
    dq run_sieve_dense_127
    dq run_sieve_dense_129


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
    mov edi, CLOCK_MONOTONIC
    mov rsi, rsp
    call clock_gettime wrt ..plt
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
    mov edi, CLOCK_MONOTONIC
    mov rsi, rsp
    call clock_gettime wrt ..plt
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
    mov edi, CLOCK_MONOTONIC
    mov rsi, rsp
    call clock_gettime wrt ..plt
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
    push rbx
    push rbp
    push r12
    push r13
    push r14
    push r15

    mov r15, rdi
    mov r14, [r15 + worker_state.sieve_ptr]
    mov r12d, [r15 + worker_state.bit_count]

    mov r8d, 3
    mov r10, 2
    xor edi, edi
    mov r13, [r14]
    mov r11d, 4

align 16
run_sieve_outer:
    cmp r11d, r12d
    jae run_sieve_done

    test r13, r10
    jz run_sieve_next

    ; This path is entered only after the factor has been discovered from the
    ; runtime sieve bitset. All odd skips in the contiguous dense range
    ; 3..129 use the same dense dispatch mechanism, and that table includes
    ; composite as well as prime skip values. Composite entries normally are
    ; not reached because composites have already been cleared by earlier
    ; discovered factors. No dispatch decision here is based on prior
    ; knowledge of primeness.
    cmp r8d, 129
    jbe run_sieve_dense_dispatch
    jmp run_sieve_sparse_dispatch

align 16
run_sieve_sparse_dispatch:
    ; This path is entered only after the factor has been discovered from the
    ; runtime sieve bitset. Sparse periodic dispatch is based only on the odd
    ; modulo-16 residue class of the factor, not on prior knowledge of
    ; primeness or on any prime-specific value selection.
    push r8
    push r10
    push r11

    mov r13d, [rsp + 16]              ; saved skip factor
    mov ebp, [r15 + worker_state.word_count]
    shl ebp, 3                        ; total bytes in the sieve

    mov eax, r13d
    shr eax, 4
    imul eax, r13d                    ; start chunk = floor(skip / 16) * skip
    lea rsi, [r14 + rax]
    sub ebp, eax

    mov eax, r13d
    shr eax, 1
    mov ebx, eax
    shr ebx, 3
    add eax, r13d
    mov ecx, eax
    shr ecx, 3
    add eax, r13d
    mov edx, eax
    shr edx, 3
    add eax, r13d
    mov r8d, eax
    shr r8d, 3
    add eax, r13d
    mov r9d, eax
    shr r9d, 3
    add eax, r13d
    mov r10d, eax
    shr r10d, 3
    add eax, r13d
    mov r11d, eax
    shr r11d, 3
    add eax, r13d
    mov r12d, eax
    shr r12d, 3

    mov eax, r13d
    and eax, 15
    cmp eax, 1
    je run_sieve_sparse_residue_01
    cmp eax, 3
    je run_sieve_sparse_residue_03
    cmp eax, 5
    je run_sieve_sparse_residue_05
    cmp eax, 7
    je run_sieve_sparse_residue_07
    cmp eax, 9
    je run_sieve_sparse_residue_09
    cmp eax, 11
    je run_sieve_sparse_residue_11
    cmp eax, 13
    je run_sieve_sparse_residue_13
    ; only remaining odd residue is 15
    jmp run_sieve_sparse_residue_15

align 16
run_sieve_sparse_residue_03:
    cmp ebp, r13d
    jb run_sieve_sparse_tail_residue_03
align 16
run_sieve_sparse_loop_residue_03:
    and byte [rsi + rbx], 0xfd
    and byte [rsi + rcx], 0xef
    and byte [rsi + rdx], 0x7f
    and byte [rsi + r8], 0xfb
    and byte [rsi + r9], 0xdf
    and byte [rsi + r10], 0xfe
    and byte [rsi + r11], 0xf7
    and byte [rsi + r12], 0xbf
    add rsi, r13
    sub ebp, r13d
    cmp ebp, r13d
    jae run_sieve_sparse_loop_residue_03
run_sieve_sparse_tail_residue_03:
    cmp ebx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rbx], 0xfd
    cmp ecx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rcx], 0xef
    cmp edx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rdx], 0x7f
    cmp r8d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r8], 0xfb
    cmp r9d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r9], 0xdf
    cmp r10d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r10], 0xfe
    cmp r11d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r11], 0xf7
    cmp r12d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r12], 0xbf
    jmp run_sieve_sparse_done

align 16
run_sieve_sparse_residue_05:
    cmp ebp, r13d
    jb run_sieve_sparse_tail_residue_05
align 16
run_sieve_sparse_loop_residue_05:
    and byte [rsi + rbx], 0xfb
    and byte [rsi + rcx], 0x7f
    and byte [rsi + rdx], 0xef
    and byte [rsi + r8], 0xfd
    and byte [rsi + r9], 0xbf
    and byte [rsi + r10], 0xf7
    and byte [rsi + r11], 0xfe
    and byte [rsi + r12], 0xdf
    add rsi, r13
    sub ebp, r13d
    cmp ebp, r13d
    jae run_sieve_sparse_loop_residue_05
run_sieve_sparse_tail_residue_05:
    cmp ebx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rbx], 0xfb
    cmp ecx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rcx], 0x7f
    cmp edx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rdx], 0xef
    cmp r8d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r8], 0xfd
    cmp r9d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r9], 0xbf
    cmp r10d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r10], 0xf7
    cmp r11d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r11], 0xfe
    cmp r12d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r12], 0xdf
    jmp run_sieve_sparse_done

align 16
run_sieve_sparse_residue_07:
    cmp ebp, r13d
    jb run_sieve_sparse_tail_residue_07
align 16
run_sieve_sparse_loop_residue_07:
    and byte [rsi + rbx], 0xf7
    and byte [rsi + rcx], 0xfb
    and byte [rsi + rdx], 0xfd
    and byte [rsi + r8], 0xfe
    and byte [rsi + r9], 0x7f
    and byte [rsi + r10], 0xbf
    and byte [rsi + r11], 0xdf
    and byte [rsi + r12], 0xef
    add rsi, r13
    sub ebp, r13d
    cmp ebp, r13d
    jae run_sieve_sparse_loop_residue_07
run_sieve_sparse_tail_residue_07:
    cmp ebx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rbx], 0xf7
    cmp ecx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rcx], 0xfb
    cmp edx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rdx], 0xfd
    cmp r8d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r8], 0xfe
    cmp r9d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r9], 0x7f
    cmp r10d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r10], 0xbf
    cmp r11d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r11], 0xdf
    cmp r12d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r12], 0xef
    jmp run_sieve_sparse_done

align 16
run_sieve_sparse_residue_09:
    cmp ebp, r13d
    jb run_sieve_sparse_tail_residue_09
align 16
run_sieve_sparse_loop_residue_09:
    and byte [rsi + rbx], 0xef
    and byte [rsi + rcx], 0xdf
    and byte [rsi + rdx], 0xbf
    and byte [rsi + r8], 0x7f
    and byte [rsi + r9], 0xfe
    and byte [rsi + r10], 0xfd
    and byte [rsi + r11], 0xfb
    and byte [rsi + r12], 0xf7
    add rsi, r13
    sub ebp, r13d
    cmp ebp, r13d
    jae run_sieve_sparse_loop_residue_09
run_sieve_sparse_tail_residue_09:
    cmp ebx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rbx], 0xef
    cmp ecx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rcx], 0xdf
    cmp edx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rdx], 0xbf
    cmp r8d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r8], 0x7f
    cmp r9d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r9], 0xfe
    cmp r10d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r10], 0xfd
    cmp r11d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r11], 0xfb
    cmp r12d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r12], 0xf7
    jmp run_sieve_sparse_done

align 16
run_sieve_sparse_residue_11:
    cmp ebp, r13d
    jb run_sieve_sparse_tail_residue_11
align 16
run_sieve_sparse_loop_residue_11:
    and byte [rsi + rbx], 0xdf
    and byte [rsi + rcx], 0xfe
    and byte [rsi + rdx], 0xf7
    and byte [rsi + r8], 0xbf
    and byte [rsi + r9], 0xfd
    and byte [rsi + r10], 0xef
    and byte [rsi + r11], 0x7f
    and byte [rsi + r12], 0xfb
    add rsi, r13
    sub ebp, r13d
    cmp ebp, r13d
    jae run_sieve_sparse_loop_residue_11
run_sieve_sparse_tail_residue_11:
    cmp ebx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rbx], 0xdf
    cmp ecx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rcx], 0xfe
    cmp edx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rdx], 0xf7
    cmp r8d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r8], 0xbf
    cmp r9d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r9], 0xfd
    cmp r10d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r10], 0xef
    cmp r11d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r11], 0x7f
    cmp r12d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r12], 0xfb
    jmp run_sieve_sparse_done

align 16
run_sieve_sparse_residue_13:
    cmp ebp, r13d
    jb run_sieve_sparse_tail_residue_13
align 16
run_sieve_sparse_loop_residue_13:
    and byte [rsi + rbx], 0xbf
    and byte [rsi + rcx], 0xf7
    and byte [rsi + rdx], 0xfe
    and byte [rsi + r8], 0xdf
    and byte [rsi + r9], 0xfb
    and byte [rsi + r10], 0x7f
    and byte [rsi + r11], 0xef
    and byte [rsi + r12], 0xfd
    add rsi, r13
    sub ebp, r13d
    cmp ebp, r13d
    jae run_sieve_sparse_loop_residue_13
run_sieve_sparse_tail_residue_13:
    cmp ebx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rbx], 0xbf
    cmp ecx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rcx], 0xf7
    cmp edx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rdx], 0xfe
    cmp r8d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r8], 0xdf
    cmp r9d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r9], 0xfb
    cmp r10d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r10], 0x7f
    cmp r11d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r11], 0xef
    cmp r12d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r12], 0xfd
    jmp run_sieve_sparse_done

align 16
run_sieve_sparse_residue_15:
    cmp ebp, r13d
    jb run_sieve_sparse_tail_residue_15
align 16
run_sieve_sparse_loop_residue_15:
    and byte [rsi + rbx], 0x7f
    and byte [rsi + rcx], 0xbf
    and byte [rsi + rdx], 0xdf
    and byte [rsi + r8], 0xef
    and byte [rsi + r9], 0xf7
    and byte [rsi + r10], 0xfb
    and byte [rsi + r11], 0xfd
    and byte [rsi + r12], 0xfe
    add rsi, r13
    sub ebp, r13d
    cmp ebp, r13d
    jae run_sieve_sparse_loop_residue_15
run_sieve_sparse_tail_residue_15:
    cmp ebx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rbx], 0x7f
    cmp ecx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rcx], 0xbf
    cmp edx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rdx], 0xdf
    cmp r8d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r8], 0xef
    cmp r9d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r9], 0xf7
    cmp r10d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r10], 0xfb
    cmp r11d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r11], 0xfd
    cmp r12d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r12], 0xfe
    jmp run_sieve_sparse_done

align 16
run_sieve_sparse_residue_01:
    cmp ebp, r13d
    jb run_sieve_sparse_tail_residue_01
align 16
run_sieve_sparse_loop_residue_01:
    and byte [rsi + rbx], 0xfe
    and byte [rsi + rcx], 0xfd
    and byte [rsi + rdx], 0xfb
    and byte [rsi + r8], 0xf7
    and byte [rsi + r9], 0xef
    and byte [rsi + r10], 0xdf
    and byte [rsi + r11], 0xbf
    and byte [rsi + r12], 0x7f
    add rsi, r13
    sub ebp, r13d
    cmp ebp, r13d
    jae run_sieve_sparse_loop_residue_01
run_sieve_sparse_tail_residue_01:
    cmp ebx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rbx], 0xfe
    cmp ecx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rcx], 0xfd
    cmp edx, ebp
    jae run_sieve_sparse_done
    and byte [rsi + rdx], 0xfb
    cmp r8d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r8], 0xf7
    cmp r9d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r9], 0xef
    cmp r10d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r10], 0xdf
    cmp r11d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r11], 0xbf
    cmp r12d, ebp
    jae run_sieve_sparse_done
    and byte [rsi + r12], 0x7f

run_sieve_sparse_done:
    mov r12d, [r15 + worker_state.bit_count]
    mov r13, [r14 + rdi*8]
    pop r11
    pop r10
    pop r8
    jmp run_sieve_next

run_sieve_dense_dispatch:
    ; Dense periodic dispatch uses only the discovered odd skip value:
    ; index = (skip - 3) / 2 for the contiguous table 3,5,7,...,129.
    lea rax, [rel dense_jump_table]
    mov edx, r8d
    sub edx, 3
    shr edx, 1
    jmp qword [rax + rdx*8]

align 16
run_sieve_dense_003:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 3
    jb run_sieve_dense_tail_003
align 16
run_sieve_dense_loop_003:
    mov rax, qword [rsi]
    btr rax, 1
    btr rax, 4
    btr rax, 7
    btr rax, 10
    btr rax, 13
    btr rax, 16
    btr rax, 19
    btr rax, 22
    btr rax, 25
    btr rax, 28
    btr rax, 31
    btr rax, 34
    btr rax, 37
    btr rax, 40
    btr rax, 43
    btr rax, 46
    btr rax, 49
    btr rax, 52
    btr rax, 55
    btr rax, 58
    btr rax, 61
    mov qword [rsi], rax
    mov rax, qword [rsi + 8]
    btr rax, 0
    btr rax, 3
    btr rax, 6
    btr rax, 9
    btr rax, 12
    btr rax, 15
    btr rax, 18
    btr rax, 21
    btr rax, 24
    btr rax, 27
    btr rax, 30
    btr rax, 33
    btr rax, 36
    btr rax, 39
    btr rax, 42
    btr rax, 45
    btr rax, 48
    btr rax, 51
    btr rax, 54
    btr rax, 57
    btr rax, 60
    btr rax, 63
    mov qword [rsi + 8], rax
    mov rax, qword [rsi + 16]
    btr rax, 2
    btr rax, 5
    btr rax, 8
    btr rax, 11
    btr rax, 14
    btr rax, 17
    btr rax, 20
    btr rax, 23
    btr rax, 26
    btr rax, 29
    btr rax, 32
    btr rax, 35
    btr rax, 38
    btr rax, 41
    btr rax, 44
    btr rax, 47
    btr rax, 50
    btr rax, 53
    btr rax, 56
    btr rax, 59
    btr rax, 62
    mov qword [rsi + 16], rax
    add rsi, 24
    sub ecx, 3
    cmp ecx, 3
    jae run_sieve_dense_loop_003
run_sieve_dense_tail_003:
    test ecx, ecx
    jz run_sieve_dense_restore_003
    cmp ecx, 1
    jb run_sieve_dense_restore_003
    mov rax, qword [rsi]
    btr rax, 1
    btr rax, 4
    btr rax, 7
    btr rax, 10
    btr rax, 13
    btr rax, 16
    btr rax, 19
    btr rax, 22
    btr rax, 25
    btr rax, 28
    btr rax, 31
    btr rax, 34
    btr rax, 37
    btr rax, 40
    btr rax, 43
    btr rax, 46
    btr rax, 49
    btr rax, 52
    btr rax, 55
    btr rax, 58
    btr rax, 61
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_003
    mov rax, qword [rsi + 8]
    btr rax, 0
    btr rax, 3
    btr rax, 6
    btr rax, 9
    btr rax, 12
    btr rax, 15
    btr rax, 18
    btr rax, 21
    btr rax, 24
    btr rax, 27
    btr rax, 30
    btr rax, 33
    btr rax, 36
    btr rax, 39
    btr rax, 42
    btr rax, 45
    btr rax, 48
    btr rax, 51
    btr rax, 54
    btr rax, 57
    btr rax, 60
    btr rax, 63
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_003
    mov rax, qword [rsi + 16]
    btr rax, 2
    btr rax, 5
    btr rax, 8
    btr rax, 11
    btr rax, 14
    btr rax, 17
    btr rax, 20
    btr rax, 23
    btr rax, 26
    btr rax, 29
    btr rax, 32
    btr rax, 35
    btr rax, 38
    btr rax, 41
    btr rax, 44
    btr rax, 47
    btr rax, 50
    btr rax, 53
    btr rax, 56
    btr rax, 59
    btr rax, 62
    mov qword [rsi + 16], rax
run_sieve_dense_restore_003:
    bts qword [r14], 1
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_005:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 5
    jb run_sieve_dense_tail_005
align 16
run_sieve_dense_loop_005:
    mov rax, qword [rsi]
    btr rax, 2
    btr rax, 7
    btr rax, 12
    btr rax, 17
    btr rax, 22
    btr rax, 27
    btr rax, 32
    btr rax, 37
    btr rax, 42
    btr rax, 47
    btr rax, 52
    btr rax, 57
    btr rax, 62
    mov qword [rsi], rax
    mov rax, qword [rsi + 8]
    btr rax, 3
    btr rax, 8
    btr rax, 13
    btr rax, 18
    btr rax, 23
    btr rax, 28
    btr rax, 33
    btr rax, 38
    btr rax, 43
    btr rax, 48
    btr rax, 53
    btr rax, 58
    btr rax, 63
    mov qword [rsi + 8], rax
    mov rax, qword [rsi + 16]
    btr rax, 4
    btr rax, 9
    btr rax, 14
    btr rax, 19
    btr rax, 24
    btr rax, 29
    btr rax, 34
    btr rax, 39
    btr rax, 44
    btr rax, 49
    btr rax, 54
    btr rax, 59
    mov qword [rsi + 16], rax
    mov rax, qword [rsi + 24]
    btr rax, 0
    btr rax, 5
    btr rax, 10
    btr rax, 15
    btr rax, 20
    btr rax, 25
    btr rax, 30
    btr rax, 35
    btr rax, 40
    btr rax, 45
    btr rax, 50
    btr rax, 55
    btr rax, 60
    mov qword [rsi + 24], rax
    mov rax, qword [rsi + 32]
    btr rax, 1
    btr rax, 6
    btr rax, 11
    btr rax, 16
    btr rax, 21
    btr rax, 26
    btr rax, 31
    btr rax, 36
    btr rax, 41
    btr rax, 46
    btr rax, 51
    btr rax, 56
    btr rax, 61
    mov qword [rsi + 32], rax
    add rsi, 40
    sub ecx, 5
    cmp ecx, 5
    jae run_sieve_dense_loop_005
run_sieve_dense_tail_005:
    test ecx, ecx
    jz run_sieve_dense_restore_005
    cmp ecx, 1
    jb run_sieve_dense_restore_005
    mov rax, qword [rsi]
    btr rax, 2
    btr rax, 7
    btr rax, 12
    btr rax, 17
    btr rax, 22
    btr rax, 27
    btr rax, 32
    btr rax, 37
    btr rax, 42
    btr rax, 47
    btr rax, 52
    btr rax, 57
    btr rax, 62
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_005
    mov rax, qword [rsi + 8]
    btr rax, 3
    btr rax, 8
    btr rax, 13
    btr rax, 18
    btr rax, 23
    btr rax, 28
    btr rax, 33
    btr rax, 38
    btr rax, 43
    btr rax, 48
    btr rax, 53
    btr rax, 58
    btr rax, 63
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_005
    mov rax, qword [rsi + 16]
    btr rax, 4
    btr rax, 9
    btr rax, 14
    btr rax, 19
    btr rax, 24
    btr rax, 29
    btr rax, 34
    btr rax, 39
    btr rax, 44
    btr rax, 49
    btr rax, 54
    btr rax, 59
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_005
    mov rax, qword [rsi + 24]
    btr rax, 0
    btr rax, 5
    btr rax, 10
    btr rax, 15
    btr rax, 20
    btr rax, 25
    btr rax, 30
    btr rax, 35
    btr rax, 40
    btr rax, 45
    btr rax, 50
    btr rax, 55
    btr rax, 60
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_005
    mov rax, qword [rsi + 32]
    btr rax, 1
    btr rax, 6
    btr rax, 11
    btr rax, 16
    btr rax, 21
    btr rax, 26
    btr rax, 31
    btr rax, 36
    btr rax, 41
    btr rax, 46
    btr rax, 51
    btr rax, 56
    btr rax, 61
    mov qword [rsi + 32], rax
run_sieve_dense_restore_005:
    bts qword [r14], 2
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_007:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 7
    jb run_sieve_dense_tail_007
align 16
run_sieve_dense_loop_007:
    mov rax, qword [rsi]
    btr rax, 3
    btr rax, 10
    btr rax, 17
    btr rax, 24
    btr rax, 31
    btr rax, 38
    btr rax, 45
    btr rax, 52
    btr rax, 59
    mov qword [rsi], rax
    mov rax, qword [rsi + 8]
    btr rax, 2
    btr rax, 9
    btr rax, 16
    btr rax, 23
    btr rax, 30
    btr rax, 37
    btr rax, 44
    btr rax, 51
    btr rax, 58
    mov qword [rsi + 8], rax
    mov rax, qword [rsi + 16]
    btr rax, 1
    btr rax, 8
    btr rax, 15
    btr rax, 22
    btr rax, 29
    btr rax, 36
    btr rax, 43
    btr rax, 50
    btr rax, 57
    mov qword [rsi + 16], rax
    mov rax, qword [rsi + 24]
    btr rax, 0
    btr rax, 7
    btr rax, 14
    btr rax, 21
    btr rax, 28
    btr rax, 35
    btr rax, 42
    btr rax, 49
    btr rax, 56
    btr rax, 63
    mov qword [rsi + 24], rax
    mov rax, qword [rsi + 32]
    btr rax, 6
    btr rax, 13
    btr rax, 20
    btr rax, 27
    btr rax, 34
    btr rax, 41
    btr rax, 48
    btr rax, 55
    btr rax, 62
    mov qword [rsi + 32], rax
    mov rax, qword [rsi + 40]
    btr rax, 5
    btr rax, 12
    btr rax, 19
    btr rax, 26
    btr rax, 33
    btr rax, 40
    btr rax, 47
    btr rax, 54
    btr rax, 61
    mov qword [rsi + 40], rax
    mov rax, qword [rsi + 48]
    btr rax, 4
    btr rax, 11
    btr rax, 18
    btr rax, 25
    btr rax, 32
    btr rax, 39
    btr rax, 46
    btr rax, 53
    btr rax, 60
    mov qword [rsi + 48], rax
    add rsi, 56
    sub ecx, 7
    cmp ecx, 7
    jae run_sieve_dense_loop_007
run_sieve_dense_tail_007:
    test ecx, ecx
    jz run_sieve_dense_restore_007
    cmp ecx, 1
    jb run_sieve_dense_restore_007
    mov rax, qword [rsi]
    btr rax, 3
    btr rax, 10
    btr rax, 17
    btr rax, 24
    btr rax, 31
    btr rax, 38
    btr rax, 45
    btr rax, 52
    btr rax, 59
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_007
    mov rax, qword [rsi + 8]
    btr rax, 2
    btr rax, 9
    btr rax, 16
    btr rax, 23
    btr rax, 30
    btr rax, 37
    btr rax, 44
    btr rax, 51
    btr rax, 58
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_007
    mov rax, qword [rsi + 16]
    btr rax, 1
    btr rax, 8
    btr rax, 15
    btr rax, 22
    btr rax, 29
    btr rax, 36
    btr rax, 43
    btr rax, 50
    btr rax, 57
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_007
    mov rax, qword [rsi + 24]
    btr rax, 0
    btr rax, 7
    btr rax, 14
    btr rax, 21
    btr rax, 28
    btr rax, 35
    btr rax, 42
    btr rax, 49
    btr rax, 56
    btr rax, 63
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_007
    mov rax, qword [rsi + 32]
    btr rax, 6
    btr rax, 13
    btr rax, 20
    btr rax, 27
    btr rax, 34
    btr rax, 41
    btr rax, 48
    btr rax, 55
    btr rax, 62
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_007
    mov rax, qword [rsi + 40]
    btr rax, 5
    btr rax, 12
    btr rax, 19
    btr rax, 26
    btr rax, 33
    btr rax, 40
    btr rax, 47
    btr rax, 54
    btr rax, 61
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_007
    mov rax, qword [rsi + 48]
    btr rax, 4
    btr rax, 11
    btr rax, 18
    btr rax, 25
    btr rax, 32
    btr rax, 39
    btr rax, 46
    btr rax, 53
    btr rax, 60
    mov qword [rsi + 48], rax
run_sieve_dense_restore_007:
    bts qword [r14], 3
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_009:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 9
    jb run_sieve_dense_tail_009
align 16
run_sieve_dense_loop_009:
    mov rax, qword [rsi]
    btr rax, 4
    btr rax, 13
    btr rax, 22
    btr rax, 31
    btr rax, 40
    btr rax, 49
    btr rax, 58
    mov qword [rsi], rax
    mov rax, qword [rsi + 8]
    btr rax, 3
    btr rax, 12
    btr rax, 21
    btr rax, 30
    btr rax, 39
    btr rax, 48
    btr rax, 57
    mov qword [rsi + 8], rax
    mov rax, qword [rsi + 16]
    btr rax, 2
    btr rax, 11
    btr rax, 20
    btr rax, 29
    btr rax, 38
    btr rax, 47
    btr rax, 56
    mov qword [rsi + 16], rax
    mov rax, qword [rsi + 24]
    btr rax, 1
    btr rax, 10
    btr rax, 19
    btr rax, 28
    btr rax, 37
    btr rax, 46
    btr rax, 55
    mov qword [rsi + 24], rax
    mov rax, qword [rsi + 32]
    btr rax, 0
    btr rax, 9
    btr rax, 18
    btr rax, 27
    btr rax, 36
    btr rax, 45
    btr rax, 54
    btr rax, 63
    mov qword [rsi + 32], rax
    mov rax, qword [rsi + 40]
    btr rax, 8
    btr rax, 17
    btr rax, 26
    btr rax, 35
    btr rax, 44
    btr rax, 53
    btr rax, 62
    mov qword [rsi + 40], rax
    mov rax, qword [rsi + 48]
    btr rax, 7
    btr rax, 16
    btr rax, 25
    btr rax, 34
    btr rax, 43
    btr rax, 52
    btr rax, 61
    mov qword [rsi + 48], rax
    mov rax, qword [rsi + 56]
    btr rax, 6
    btr rax, 15
    btr rax, 24
    btr rax, 33
    btr rax, 42
    btr rax, 51
    btr rax, 60
    mov qword [rsi + 56], rax
    mov rax, qword [rsi + 64]
    btr rax, 5
    btr rax, 14
    btr rax, 23
    btr rax, 32
    btr rax, 41
    btr rax, 50
    btr rax, 59
    mov qword [rsi + 64], rax
    add rsi, 72
    sub ecx, 9
    cmp ecx, 9
    jae run_sieve_dense_loop_009
run_sieve_dense_tail_009:
    test ecx, ecx
    jz run_sieve_dense_restore_009
    cmp ecx, 1
    jb run_sieve_dense_restore_009
    mov rax, qword [rsi]
    btr rax, 4
    btr rax, 13
    btr rax, 22
    btr rax, 31
    btr rax, 40
    btr rax, 49
    btr rax, 58
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_009
    mov rax, qword [rsi + 8]
    btr rax, 3
    btr rax, 12
    btr rax, 21
    btr rax, 30
    btr rax, 39
    btr rax, 48
    btr rax, 57
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_009
    mov rax, qword [rsi + 16]
    btr rax, 2
    btr rax, 11
    btr rax, 20
    btr rax, 29
    btr rax, 38
    btr rax, 47
    btr rax, 56
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_009
    mov rax, qword [rsi + 24]
    btr rax, 1
    btr rax, 10
    btr rax, 19
    btr rax, 28
    btr rax, 37
    btr rax, 46
    btr rax, 55
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_009
    mov rax, qword [rsi + 32]
    btr rax, 0
    btr rax, 9
    btr rax, 18
    btr rax, 27
    btr rax, 36
    btr rax, 45
    btr rax, 54
    btr rax, 63
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_009
    mov rax, qword [rsi + 40]
    btr rax, 8
    btr rax, 17
    btr rax, 26
    btr rax, 35
    btr rax, 44
    btr rax, 53
    btr rax, 62
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_009
    mov rax, qword [rsi + 48]
    btr rax, 7
    btr rax, 16
    btr rax, 25
    btr rax, 34
    btr rax, 43
    btr rax, 52
    btr rax, 61
    mov qword [rsi + 48], rax
    cmp ecx, 8
    jb run_sieve_dense_restore_009
    mov rax, qword [rsi + 56]
    btr rax, 6
    btr rax, 15
    btr rax, 24
    btr rax, 33
    btr rax, 42
    btr rax, 51
    btr rax, 60
    mov qword [rsi + 56], rax
    cmp ecx, 9
    jb run_sieve_dense_restore_009
    mov rax, qword [rsi + 64]
    btr rax, 5
    btr rax, 14
    btr rax, 23
    btr rax, 32
    btr rax, 41
    btr rax, 50
    btr rax, 59
    mov qword [rsi + 64], rax
run_sieve_dense_restore_009:
    bts qword [r14], 4
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_011:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 11
    jb run_sieve_dense_tail_011
align 16
run_sieve_dense_loop_011:
    mov rax, qword [rsi]
    btr rax, 5
    btr rax, 16
    btr rax, 27
    btr rax, 38
    btr rax, 49
    btr rax, 60
    mov qword [rsi], rax
    mov rax, qword [rsi + 8]
    btr rax, 7
    btr rax, 18
    btr rax, 29
    btr rax, 40
    btr rax, 51
    btr rax, 62
    mov qword [rsi + 8], rax
    mov rax, qword [rsi + 16]
    btr rax, 9
    btr rax, 20
    btr rax, 31
    btr rax, 42
    btr rax, 53
    mov qword [rsi + 16], rax
    mov rax, qword [rsi + 24]
    btr rax, 0
    btr rax, 11
    btr rax, 22
    btr rax, 33
    btr rax, 44
    btr rax, 55
    mov qword [rsi + 24], rax
    mov rax, qword [rsi + 32]
    btr rax, 2
    btr rax, 13
    btr rax, 24
    btr rax, 35
    btr rax, 46
    btr rax, 57
    mov qword [rsi + 32], rax
    mov rax, qword [rsi + 40]
    btr rax, 4
    btr rax, 15
    btr rax, 26
    btr rax, 37
    btr rax, 48
    btr rax, 59
    mov qword [rsi + 40], rax
    mov rax, qword [rsi + 48]
    btr rax, 6
    btr rax, 17
    btr rax, 28
    btr rax, 39
    btr rax, 50
    btr rax, 61
    mov qword [rsi + 48], rax
    mov rax, qword [rsi + 56]
    btr rax, 8
    btr rax, 19
    btr rax, 30
    btr rax, 41
    btr rax, 52
    btr rax, 63
    mov qword [rsi + 56], rax
    mov rax, qword [rsi + 64]
    btr rax, 10
    btr rax, 21
    btr rax, 32
    btr rax, 43
    btr rax, 54
    mov qword [rsi + 64], rax
    mov rax, qword [rsi + 72]
    btr rax, 1
    btr rax, 12
    btr rax, 23
    btr rax, 34
    btr rax, 45
    btr rax, 56
    mov qword [rsi + 72], rax
    mov rax, qword [rsi + 80]
    btr rax, 3
    btr rax, 14
    btr rax, 25
    btr rax, 36
    btr rax, 47
    btr rax, 58
    mov qword [rsi + 80], rax
    add rsi, 88
    sub ecx, 11
    cmp ecx, 11
    jae run_sieve_dense_loop_011
run_sieve_dense_tail_011:
    test ecx, ecx
    jz run_sieve_dense_restore_011
    cmp ecx, 1
    jb run_sieve_dense_restore_011
    mov rax, qword [rsi]
    btr rax, 5
    btr rax, 16
    btr rax, 27
    btr rax, 38
    btr rax, 49
    btr rax, 60
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_011
    mov rax, qword [rsi + 8]
    btr rax, 7
    btr rax, 18
    btr rax, 29
    btr rax, 40
    btr rax, 51
    btr rax, 62
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_011
    mov rax, qword [rsi + 16]
    btr rax, 9
    btr rax, 20
    btr rax, 31
    btr rax, 42
    btr rax, 53
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_011
    mov rax, qword [rsi + 24]
    btr rax, 0
    btr rax, 11
    btr rax, 22
    btr rax, 33
    btr rax, 44
    btr rax, 55
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_011
    mov rax, qword [rsi + 32]
    btr rax, 2
    btr rax, 13
    btr rax, 24
    btr rax, 35
    btr rax, 46
    btr rax, 57
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_011
    mov rax, qword [rsi + 40]
    btr rax, 4
    btr rax, 15
    btr rax, 26
    btr rax, 37
    btr rax, 48
    btr rax, 59
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_011
    mov rax, qword [rsi + 48]
    btr rax, 6
    btr rax, 17
    btr rax, 28
    btr rax, 39
    btr rax, 50
    btr rax, 61
    mov qword [rsi + 48], rax
    cmp ecx, 8
    jb run_sieve_dense_restore_011
    mov rax, qword [rsi + 56]
    btr rax, 8
    btr rax, 19
    btr rax, 30
    btr rax, 41
    btr rax, 52
    btr rax, 63
    mov qword [rsi + 56], rax
    cmp ecx, 9
    jb run_sieve_dense_restore_011
    mov rax, qword [rsi + 64]
    btr rax, 10
    btr rax, 21
    btr rax, 32
    btr rax, 43
    btr rax, 54
    mov qword [rsi + 64], rax
    cmp ecx, 10
    jb run_sieve_dense_restore_011
    mov rax, qword [rsi + 72]
    btr rax, 1
    btr rax, 12
    btr rax, 23
    btr rax, 34
    btr rax, 45
    btr rax, 56
    mov qword [rsi + 72], rax
    cmp ecx, 11
    jb run_sieve_dense_restore_011
    mov rax, qword [rsi + 80]
    btr rax, 3
    btr rax, 14
    btr rax, 25
    btr rax, 36
    btr rax, 47
    btr rax, 58
    mov qword [rsi + 80], rax
run_sieve_dense_restore_011:
    bts qword [r14], 5
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_013:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 13
    jb run_sieve_dense_tail_013
align 16
run_sieve_dense_loop_013:
    mov rax, qword [rsi]
    btr rax, 6
    btr rax, 19
    btr rax, 32
    btr rax, 45
    btr rax, 58
    mov qword [rsi], rax
    mov rax, qword [rsi + 8]
    btr rax, 7
    btr rax, 20
    btr rax, 33
    btr rax, 46
    btr rax, 59
    mov qword [rsi + 8], rax
    mov rax, qword [rsi + 16]
    btr rax, 8
    btr rax, 21
    btr rax, 34
    btr rax, 47
    btr rax, 60
    mov qword [rsi + 16], rax
    mov rax, qword [rsi + 24]
    btr rax, 9
    btr rax, 22
    btr rax, 35
    btr rax, 48
    btr rax, 61
    mov qword [rsi + 24], rax
    mov rax, qword [rsi + 32]
    btr rax, 10
    btr rax, 23
    btr rax, 36
    btr rax, 49
    btr rax, 62
    mov qword [rsi + 32], rax
    mov rax, qword [rsi + 40]
    btr rax, 11
    btr rax, 24
    btr rax, 37
    btr rax, 50
    btr rax, 63
    mov qword [rsi + 40], rax
    mov rax, qword [rsi + 48]
    btr rax, 12
    btr rax, 25
    btr rax, 38
    btr rax, 51
    mov qword [rsi + 48], rax
    mov rax, qword [rsi + 56]
    btr rax, 0
    btr rax, 13
    btr rax, 26
    btr rax, 39
    btr rax, 52
    mov qword [rsi + 56], rax
    mov rax, qword [rsi + 64]
    btr rax, 1
    btr rax, 14
    btr rax, 27
    btr rax, 40
    btr rax, 53
    mov qword [rsi + 64], rax
    mov rax, qword [rsi + 72]
    btr rax, 2
    btr rax, 15
    btr rax, 28
    btr rax, 41
    btr rax, 54
    mov qword [rsi + 72], rax
    mov rax, qword [rsi + 80]
    btr rax, 3
    btr rax, 16
    btr rax, 29
    btr rax, 42
    btr rax, 55
    mov qword [rsi + 80], rax
    mov rax, qword [rsi + 88]
    btr rax, 4
    btr rax, 17
    btr rax, 30
    btr rax, 43
    btr rax, 56
    mov qword [rsi + 88], rax
    mov rax, qword [rsi + 96]
    btr rax, 5
    btr rax, 18
    btr rax, 31
    btr rax, 44
    btr rax, 57
    mov qword [rsi + 96], rax
    add rsi, 104
    sub ecx, 13
    cmp ecx, 13
    jae run_sieve_dense_loop_013
run_sieve_dense_tail_013:
    test ecx, ecx
    jz run_sieve_dense_restore_013
    cmp ecx, 1
    jb run_sieve_dense_restore_013
    mov rax, qword [rsi]
    btr rax, 6
    btr rax, 19
    btr rax, 32
    btr rax, 45
    btr rax, 58
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_013
    mov rax, qword [rsi + 8]
    btr rax, 7
    btr rax, 20
    btr rax, 33
    btr rax, 46
    btr rax, 59
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_013
    mov rax, qword [rsi + 16]
    btr rax, 8
    btr rax, 21
    btr rax, 34
    btr rax, 47
    btr rax, 60
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_013
    mov rax, qword [rsi + 24]
    btr rax, 9
    btr rax, 22
    btr rax, 35
    btr rax, 48
    btr rax, 61
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_013
    mov rax, qword [rsi + 32]
    btr rax, 10
    btr rax, 23
    btr rax, 36
    btr rax, 49
    btr rax, 62
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_013
    mov rax, qword [rsi + 40]
    btr rax, 11
    btr rax, 24
    btr rax, 37
    btr rax, 50
    btr rax, 63
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_013
    mov rax, qword [rsi + 48]
    btr rax, 12
    btr rax, 25
    btr rax, 38
    btr rax, 51
    mov qword [rsi + 48], rax
    cmp ecx, 8
    jb run_sieve_dense_restore_013
    mov rax, qword [rsi + 56]
    btr rax, 0
    btr rax, 13
    btr rax, 26
    btr rax, 39
    btr rax, 52
    mov qword [rsi + 56], rax
    cmp ecx, 9
    jb run_sieve_dense_restore_013
    mov rax, qword [rsi + 64]
    btr rax, 1
    btr rax, 14
    btr rax, 27
    btr rax, 40
    btr rax, 53
    mov qword [rsi + 64], rax
    cmp ecx, 10
    jb run_sieve_dense_restore_013
    mov rax, qword [rsi + 72]
    btr rax, 2
    btr rax, 15
    btr rax, 28
    btr rax, 41
    btr rax, 54
    mov qword [rsi + 72], rax
    cmp ecx, 11
    jb run_sieve_dense_restore_013
    mov rax, qword [rsi + 80]
    btr rax, 3
    btr rax, 16
    btr rax, 29
    btr rax, 42
    btr rax, 55
    mov qword [rsi + 80], rax
    cmp ecx, 12
    jb run_sieve_dense_restore_013
    mov rax, qword [rsi + 88]
    btr rax, 4
    btr rax, 17
    btr rax, 30
    btr rax, 43
    btr rax, 56
    mov qword [rsi + 88], rax
    cmp ecx, 13
    jb run_sieve_dense_restore_013
    mov rax, qword [rsi + 96]
    btr rax, 5
    btr rax, 18
    btr rax, 31
    btr rax, 44
    btr rax, 57
    mov qword [rsi + 96], rax
run_sieve_dense_restore_013:
    bts qword [r14], 6
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_015:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 15
    jb run_sieve_dense_tail_015
align 16
run_sieve_dense_loop_015:
    mov rax, qword [rsi]
    btr rax, 7
    btr rax, 22
    btr rax, 37
    btr rax, 52
    mov qword [rsi], rax
    mov rax, qword [rsi + 8]
    btr rax, 3
    btr rax, 18
    btr rax, 33
    btr rax, 48
    btr rax, 63
    mov qword [rsi + 8], rax
    mov rax, qword [rsi + 16]
    btr rax, 14
    btr rax, 29
    btr rax, 44
    btr rax, 59
    mov qword [rsi + 16], rax
    mov rax, qword [rsi + 24]
    btr rax, 10
    btr rax, 25
    btr rax, 40
    btr rax, 55
    mov qword [rsi + 24], rax
    mov rax, qword [rsi + 32]
    btr rax, 6
    btr rax, 21
    btr rax, 36
    btr rax, 51
    mov qword [rsi + 32], rax
    mov rax, qword [rsi + 40]
    btr rax, 2
    btr rax, 17
    btr rax, 32
    btr rax, 47
    btr rax, 62
    mov qword [rsi + 40], rax
    mov rax, qword [rsi + 48]
    btr rax, 13
    btr rax, 28
    btr rax, 43
    btr rax, 58
    mov qword [rsi + 48], rax
    mov rax, qword [rsi + 56]
    btr rax, 9
    btr rax, 24
    btr rax, 39
    btr rax, 54
    mov qword [rsi + 56], rax
    mov rax, qword [rsi + 64]
    btr rax, 5
    btr rax, 20
    btr rax, 35
    btr rax, 50
    mov qword [rsi + 64], rax
    mov rax, qword [rsi + 72]
    btr rax, 1
    btr rax, 16
    btr rax, 31
    btr rax, 46
    btr rax, 61
    mov qword [rsi + 72], rax
    mov rax, qword [rsi + 80]
    btr rax, 12
    btr rax, 27
    btr rax, 42
    btr rax, 57
    mov qword [rsi + 80], rax
    mov rax, qword [rsi + 88]
    btr rax, 8
    btr rax, 23
    btr rax, 38
    btr rax, 53
    mov qword [rsi + 88], rax
    mov rax, qword [rsi + 96]
    btr rax, 4
    btr rax, 19
    btr rax, 34
    btr rax, 49
    mov qword [rsi + 96], rax
    mov rax, qword [rsi + 104]
    btr rax, 0
    btr rax, 15
    btr rax, 30
    btr rax, 45
    btr rax, 60
    mov qword [rsi + 104], rax
    mov rax, qword [rsi + 112]
    btr rax, 11
    btr rax, 26
    btr rax, 41
    btr rax, 56
    mov qword [rsi + 112], rax
    add rsi, 120
    sub ecx, 15
    cmp ecx, 15
    jae run_sieve_dense_loop_015
run_sieve_dense_tail_015:
    test ecx, ecx
    jz run_sieve_dense_restore_015
    cmp ecx, 1
    jb run_sieve_dense_restore_015
    mov rax, qword [rsi]
    btr rax, 7
    btr rax, 22
    btr rax, 37
    btr rax, 52
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_015
    mov rax, qword [rsi + 8]
    btr rax, 3
    btr rax, 18
    btr rax, 33
    btr rax, 48
    btr rax, 63
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_015
    mov rax, qword [rsi + 16]
    btr rax, 14
    btr rax, 29
    btr rax, 44
    btr rax, 59
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_015
    mov rax, qword [rsi + 24]
    btr rax, 10
    btr rax, 25
    btr rax, 40
    btr rax, 55
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_015
    mov rax, qword [rsi + 32]
    btr rax, 6
    btr rax, 21
    btr rax, 36
    btr rax, 51
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_015
    mov rax, qword [rsi + 40]
    btr rax, 2
    btr rax, 17
    btr rax, 32
    btr rax, 47
    btr rax, 62
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_015
    mov rax, qword [rsi + 48]
    btr rax, 13
    btr rax, 28
    btr rax, 43
    btr rax, 58
    mov qword [rsi + 48], rax
    cmp ecx, 8
    jb run_sieve_dense_restore_015
    mov rax, qword [rsi + 56]
    btr rax, 9
    btr rax, 24
    btr rax, 39
    btr rax, 54
    mov qword [rsi + 56], rax
    cmp ecx, 9
    jb run_sieve_dense_restore_015
    mov rax, qword [rsi + 64]
    btr rax, 5
    btr rax, 20
    btr rax, 35
    btr rax, 50
    mov qword [rsi + 64], rax
    cmp ecx, 10
    jb run_sieve_dense_restore_015
    mov rax, qword [rsi + 72]
    btr rax, 1
    btr rax, 16
    btr rax, 31
    btr rax, 46
    btr rax, 61
    mov qword [rsi + 72], rax
    cmp ecx, 11
    jb run_sieve_dense_restore_015
    mov rax, qword [rsi + 80]
    btr rax, 12
    btr rax, 27
    btr rax, 42
    btr rax, 57
    mov qword [rsi + 80], rax
    cmp ecx, 12
    jb run_sieve_dense_restore_015
    mov rax, qword [rsi + 88]
    btr rax, 8
    btr rax, 23
    btr rax, 38
    btr rax, 53
    mov qword [rsi + 88], rax
    cmp ecx, 13
    jb run_sieve_dense_restore_015
    mov rax, qword [rsi + 96]
    btr rax, 4
    btr rax, 19
    btr rax, 34
    btr rax, 49
    mov qword [rsi + 96], rax
    cmp ecx, 14
    jb run_sieve_dense_restore_015
    mov rax, qword [rsi + 104]
    btr rax, 0
    btr rax, 15
    btr rax, 30
    btr rax, 45
    btr rax, 60
    mov qword [rsi + 104], rax
    cmp ecx, 15
    jb run_sieve_dense_restore_015
    mov rax, qword [rsi + 112]
    btr rax, 11
    btr rax, 26
    btr rax, 41
    btr rax, 56
    mov qword [rsi + 112], rax
run_sieve_dense_restore_015:
    bts qword [r14], 7
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_017:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 17
    jb run_sieve_dense_tail_017
align 16
run_sieve_dense_loop_017:
    mov rax, qword [rsi]
    btr rax, 8
    btr rax, 25
    btr rax, 42
    btr rax, 59
    mov qword [rsi], rax
    mov rax, qword [rsi + 8]
    btr rax, 12
    btr rax, 29
    btr rax, 46
    btr rax, 63
    mov qword [rsi + 8], rax
    mov rax, qword [rsi + 16]
    btr rax, 16
    btr rax, 33
    btr rax, 50
    mov qword [rsi + 16], rax
    mov rax, qword [rsi + 24]
    btr rax, 3
    btr rax, 20
    btr rax, 37
    btr rax, 54
    mov qword [rsi + 24], rax
    mov rax, qword [rsi + 32]
    btr rax, 7
    btr rax, 24
    btr rax, 41
    btr rax, 58
    mov qword [rsi + 32], rax
    mov rax, qword [rsi + 40]
    btr rax, 11
    btr rax, 28
    btr rax, 45
    btr rax, 62
    mov qword [rsi + 40], rax
    mov rax, qword [rsi + 48]
    btr rax, 15
    btr rax, 32
    btr rax, 49
    mov qword [rsi + 48], rax
    mov rax, qword [rsi + 56]
    btr rax, 2
    btr rax, 19
    btr rax, 36
    btr rax, 53
    mov qword [rsi + 56], rax
    mov rax, qword [rsi + 64]
    btr rax, 6
    btr rax, 23
    btr rax, 40
    btr rax, 57
    mov qword [rsi + 64], rax
    mov rax, qword [rsi + 72]
    btr rax, 10
    btr rax, 27
    btr rax, 44
    btr rax, 61
    mov qword [rsi + 72], rax
    mov rax, qword [rsi + 80]
    btr rax, 14
    btr rax, 31
    btr rax, 48
    mov qword [rsi + 80], rax
    mov rax, qword [rsi + 88]
    btr rax, 1
    btr rax, 18
    btr rax, 35
    btr rax, 52
    mov qword [rsi + 88], rax
    mov rax, qword [rsi + 96]
    btr rax, 5
    btr rax, 22
    btr rax, 39
    btr rax, 56
    mov qword [rsi + 96], rax
    mov rax, qword [rsi + 104]
    btr rax, 9
    btr rax, 26
    btr rax, 43
    btr rax, 60
    mov qword [rsi + 104], rax
    mov rax, qword [rsi + 112]
    btr rax, 13
    btr rax, 30
    btr rax, 47
    mov qword [rsi + 112], rax
    mov rax, qword [rsi + 120]
    btr rax, 0
    btr rax, 17
    btr rax, 34
    btr rax, 51
    mov qword [rsi + 120], rax
    mov rax, qword [rsi + 128]
    btr rax, 4
    btr rax, 21
    btr rax, 38
    btr rax, 55
    mov qword [rsi + 128], rax
    add rsi, 136
    sub ecx, 17
    cmp ecx, 17
    jae run_sieve_dense_loop_017
run_sieve_dense_tail_017:
    test ecx, ecx
    jz run_sieve_dense_restore_017
    cmp ecx, 1
    jb run_sieve_dense_restore_017
    mov rax, qword [rsi]
    btr rax, 8
    btr rax, 25
    btr rax, 42
    btr rax, 59
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_017
    mov rax, qword [rsi + 8]
    btr rax, 12
    btr rax, 29
    btr rax, 46
    btr rax, 63
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_017
    mov rax, qword [rsi + 16]
    btr rax, 16
    btr rax, 33
    btr rax, 50
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_017
    mov rax, qword [rsi + 24]
    btr rax, 3
    btr rax, 20
    btr rax, 37
    btr rax, 54
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_017
    mov rax, qword [rsi + 32]
    btr rax, 7
    btr rax, 24
    btr rax, 41
    btr rax, 58
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_017
    mov rax, qword [rsi + 40]
    btr rax, 11
    btr rax, 28
    btr rax, 45
    btr rax, 62
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_017
    mov rax, qword [rsi + 48]
    btr rax, 15
    btr rax, 32
    btr rax, 49
    mov qword [rsi + 48], rax
    cmp ecx, 8
    jb run_sieve_dense_restore_017
    mov rax, qword [rsi + 56]
    btr rax, 2
    btr rax, 19
    btr rax, 36
    btr rax, 53
    mov qword [rsi + 56], rax
    cmp ecx, 9
    jb run_sieve_dense_restore_017
    mov rax, qword [rsi + 64]
    btr rax, 6
    btr rax, 23
    btr rax, 40
    btr rax, 57
    mov qword [rsi + 64], rax
    cmp ecx, 10
    jb run_sieve_dense_restore_017
    mov rax, qword [rsi + 72]
    btr rax, 10
    btr rax, 27
    btr rax, 44
    btr rax, 61
    mov qword [rsi + 72], rax
    cmp ecx, 11
    jb run_sieve_dense_restore_017
    mov rax, qword [rsi + 80]
    btr rax, 14
    btr rax, 31
    btr rax, 48
    mov qword [rsi + 80], rax
    cmp ecx, 12
    jb run_sieve_dense_restore_017
    mov rax, qword [rsi + 88]
    btr rax, 1
    btr rax, 18
    btr rax, 35
    btr rax, 52
    mov qword [rsi + 88], rax
    cmp ecx, 13
    jb run_sieve_dense_restore_017
    mov rax, qword [rsi + 96]
    btr rax, 5
    btr rax, 22
    btr rax, 39
    btr rax, 56
    mov qword [rsi + 96], rax
    cmp ecx, 14
    jb run_sieve_dense_restore_017
    mov rax, qword [rsi + 104]
    btr rax, 9
    btr rax, 26
    btr rax, 43
    btr rax, 60
    mov qword [rsi + 104], rax
    cmp ecx, 15
    jb run_sieve_dense_restore_017
    mov rax, qword [rsi + 112]
    btr rax, 13
    btr rax, 30
    btr rax, 47
    mov qword [rsi + 112], rax
    cmp ecx, 16
    jb run_sieve_dense_restore_017
    mov rax, qword [rsi + 120]
    btr rax, 0
    btr rax, 17
    btr rax, 34
    btr rax, 51
    mov qword [rsi + 120], rax
    cmp ecx, 17
    jb run_sieve_dense_restore_017
    mov rax, qword [rsi + 128]
    btr rax, 4
    btr rax, 21
    btr rax, 38
    btr rax, 55
    mov qword [rsi + 128], rax
run_sieve_dense_restore_017:
    bts qword [r14], 8
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_019:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 19
    jb run_sieve_dense_tail_019
align 16
run_sieve_dense_loop_019:
    mov rax, qword [rsi]
    btr rax, 9
    btr rax, 28
    btr rax, 47
    mov qword [rsi], rax
    mov rax, qword [rsi + 8]
    btr rax, 2
    btr rax, 21
    btr rax, 40
    btr rax, 59
    mov qword [rsi + 8], rax
    mov rax, qword [rsi + 16]
    btr rax, 14
    btr rax, 33
    btr rax, 52
    mov qword [rsi + 16], rax
    mov rax, qword [rsi + 24]
    btr rax, 7
    btr rax, 26
    btr rax, 45
    mov qword [rsi + 24], rax
    mov rax, qword [rsi + 32]
    btr rax, 0
    btr rax, 19
    btr rax, 38
    btr rax, 57
    mov qword [rsi + 32], rax
    mov rax, qword [rsi + 40]
    btr rax, 12
    btr rax, 31
    btr rax, 50
    mov qword [rsi + 40], rax
    mov rax, qword [rsi + 48]
    btr rax, 5
    btr rax, 24
    btr rax, 43
    btr rax, 62
    mov qword [rsi + 48], rax
    mov rax, qword [rsi + 56]
    btr rax, 17
    btr rax, 36
    btr rax, 55
    mov qword [rsi + 56], rax
    mov rax, qword [rsi + 64]
    btr rax, 10
    btr rax, 29
    btr rax, 48
    mov qword [rsi + 64], rax
    mov rax, qword [rsi + 72]
    btr rax, 3
    btr rax, 22
    btr rax, 41
    btr rax, 60
    mov qword [rsi + 72], rax
    mov rax, qword [rsi + 80]
    btr rax, 15
    btr rax, 34
    btr rax, 53
    mov qword [rsi + 80], rax
    mov rax, qword [rsi + 88]
    btr rax, 8
    btr rax, 27
    btr rax, 46
    mov qword [rsi + 88], rax
    mov rax, qword [rsi + 96]
    btr rax, 1
    btr rax, 20
    btr rax, 39
    btr rax, 58
    mov qword [rsi + 96], rax
    mov rax, qword [rsi + 104]
    btr rax, 13
    btr rax, 32
    btr rax, 51
    mov qword [rsi + 104], rax
    mov rax, qword [rsi + 112]
    btr rax, 6
    btr rax, 25
    btr rax, 44
    btr rax, 63
    mov qword [rsi + 112], rax
    mov rax, qword [rsi + 120]
    btr rax, 18
    btr rax, 37
    btr rax, 56
    mov qword [rsi + 120], rax
    mov rax, qword [rsi + 128]
    btr rax, 11
    btr rax, 30
    btr rax, 49
    mov qword [rsi + 128], rax
    mov rax, qword [rsi + 136]
    btr rax, 4
    btr rax, 23
    btr rax, 42
    btr rax, 61
    mov qword [rsi + 136], rax
    mov rax, qword [rsi + 144]
    btr rax, 16
    btr rax, 35
    btr rax, 54
    mov qword [rsi + 144], rax
    add rsi, 152
    sub ecx, 19
    cmp ecx, 19
    jae run_sieve_dense_loop_019
run_sieve_dense_tail_019:
    test ecx, ecx
    jz run_sieve_dense_restore_019
    cmp ecx, 1
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi]
    btr rax, 9
    btr rax, 28
    btr rax, 47
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi + 8]
    btr rax, 2
    btr rax, 21
    btr rax, 40
    btr rax, 59
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi + 16]
    btr rax, 14
    btr rax, 33
    btr rax, 52
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi + 24]
    btr rax, 7
    btr rax, 26
    btr rax, 45
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi + 32]
    btr rax, 0
    btr rax, 19
    btr rax, 38
    btr rax, 57
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi + 40]
    btr rax, 12
    btr rax, 31
    btr rax, 50
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi + 48]
    btr rax, 5
    btr rax, 24
    btr rax, 43
    btr rax, 62
    mov qword [rsi + 48], rax
    cmp ecx, 8
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi + 56]
    btr rax, 17
    btr rax, 36
    btr rax, 55
    mov qword [rsi + 56], rax
    cmp ecx, 9
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi + 64]
    btr rax, 10
    btr rax, 29
    btr rax, 48
    mov qword [rsi + 64], rax
    cmp ecx, 10
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi + 72]
    btr rax, 3
    btr rax, 22
    btr rax, 41
    btr rax, 60
    mov qword [rsi + 72], rax
    cmp ecx, 11
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi + 80]
    btr rax, 15
    btr rax, 34
    btr rax, 53
    mov qword [rsi + 80], rax
    cmp ecx, 12
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi + 88]
    btr rax, 8
    btr rax, 27
    btr rax, 46
    mov qword [rsi + 88], rax
    cmp ecx, 13
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi + 96]
    btr rax, 1
    btr rax, 20
    btr rax, 39
    btr rax, 58
    mov qword [rsi + 96], rax
    cmp ecx, 14
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi + 104]
    btr rax, 13
    btr rax, 32
    btr rax, 51
    mov qword [rsi + 104], rax
    cmp ecx, 15
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi + 112]
    btr rax, 6
    btr rax, 25
    btr rax, 44
    btr rax, 63
    mov qword [rsi + 112], rax
    cmp ecx, 16
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi + 120]
    btr rax, 18
    btr rax, 37
    btr rax, 56
    mov qword [rsi + 120], rax
    cmp ecx, 17
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi + 128]
    btr rax, 11
    btr rax, 30
    btr rax, 49
    mov qword [rsi + 128], rax
    cmp ecx, 18
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi + 136]
    btr rax, 4
    btr rax, 23
    btr rax, 42
    btr rax, 61
    mov qword [rsi + 136], rax
    cmp ecx, 19
    jb run_sieve_dense_restore_019
    mov rax, qword [rsi + 144]
    btr rax, 16
    btr rax, 35
    btr rax, 54
    mov qword [rsi + 144], rax
run_sieve_dense_restore_019:
    bts qword [r14], 9
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_021:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 21
    jb run_sieve_dense_tail_021
align 16
run_sieve_dense_loop_021:
    mov rax, qword [rsi]
    btr rax, 10
    btr rax, 31
    btr rax, 52
    mov qword [rsi], rax
    mov rax, qword [rsi + 8]
    btr rax, 9
    btr rax, 30
    btr rax, 51
    mov qword [rsi + 8], rax
    mov rax, qword [rsi + 16]
    btr rax, 8
    btr rax, 29
    btr rax, 50
    mov qword [rsi + 16], rax
    mov rax, qword [rsi + 24]
    btr rax, 7
    btr rax, 28
    btr rax, 49
    mov qword [rsi + 24], rax
    mov rax, qword [rsi + 32]
    btr rax, 6
    btr rax, 27
    btr rax, 48
    mov qword [rsi + 32], rax
    mov rax, qword [rsi + 40]
    btr rax, 5
    btr rax, 26
    btr rax, 47
    mov qword [rsi + 40], rax
    mov rax, qword [rsi + 48]
    btr rax, 4
    btr rax, 25
    btr rax, 46
    mov qword [rsi + 48], rax
    mov rax, qword [rsi + 56]
    btr rax, 3
    btr rax, 24
    btr rax, 45
    mov qword [rsi + 56], rax
    mov rax, qword [rsi + 64]
    btr rax, 2
    btr rax, 23
    btr rax, 44
    mov qword [rsi + 64], rax
    mov rax, qword [rsi + 72]
    btr rax, 1
    btr rax, 22
    btr rax, 43
    mov qword [rsi + 72], rax
    mov rax, qword [rsi + 80]
    btr rax, 0
    btr rax, 21
    btr rax, 42
    btr rax, 63
    mov qword [rsi + 80], rax
    mov rax, qword [rsi + 88]
    btr rax, 20
    btr rax, 41
    btr rax, 62
    mov qword [rsi + 88], rax
    mov rax, qword [rsi + 96]
    btr rax, 19
    btr rax, 40
    btr rax, 61
    mov qword [rsi + 96], rax
    mov rax, qword [rsi + 104]
    btr rax, 18
    btr rax, 39
    btr rax, 60
    mov qword [rsi + 104], rax
    mov rax, qword [rsi + 112]
    btr rax, 17
    btr rax, 38
    btr rax, 59
    mov qword [rsi + 112], rax
    mov rax, qword [rsi + 120]
    btr rax, 16
    btr rax, 37
    btr rax, 58
    mov qword [rsi + 120], rax
    mov rax, qword [rsi + 128]
    btr rax, 15
    btr rax, 36
    btr rax, 57
    mov qword [rsi + 128], rax
    mov rax, qword [rsi + 136]
    btr rax, 14
    btr rax, 35
    btr rax, 56
    mov qword [rsi + 136], rax
    mov rax, qword [rsi + 144]
    btr rax, 13
    btr rax, 34
    btr rax, 55
    mov qword [rsi + 144], rax
    mov rax, qword [rsi + 152]
    btr rax, 12
    btr rax, 33
    btr rax, 54
    mov qword [rsi + 152], rax
    mov rax, qword [rsi + 160]
    btr rax, 11
    btr rax, 32
    btr rax, 53
    mov qword [rsi + 160], rax
    add rsi, 168
    sub ecx, 21
    cmp ecx, 21
    jae run_sieve_dense_loop_021
run_sieve_dense_tail_021:
    test ecx, ecx
    jz run_sieve_dense_restore_021
    cmp ecx, 1
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi]
    btr rax, 10
    btr rax, 31
    btr rax, 52
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 8]
    btr rax, 9
    btr rax, 30
    btr rax, 51
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 16]
    btr rax, 8
    btr rax, 29
    btr rax, 50
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 24]
    btr rax, 7
    btr rax, 28
    btr rax, 49
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 32]
    btr rax, 6
    btr rax, 27
    btr rax, 48
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 40]
    btr rax, 5
    btr rax, 26
    btr rax, 47
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 48]
    btr rax, 4
    btr rax, 25
    btr rax, 46
    mov qword [rsi + 48], rax
    cmp ecx, 8
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 56]
    btr rax, 3
    btr rax, 24
    btr rax, 45
    mov qword [rsi + 56], rax
    cmp ecx, 9
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 64]
    btr rax, 2
    btr rax, 23
    btr rax, 44
    mov qword [rsi + 64], rax
    cmp ecx, 10
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 72]
    btr rax, 1
    btr rax, 22
    btr rax, 43
    mov qword [rsi + 72], rax
    cmp ecx, 11
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 80]
    btr rax, 0
    btr rax, 21
    btr rax, 42
    btr rax, 63
    mov qword [rsi + 80], rax
    cmp ecx, 12
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 88]
    btr rax, 20
    btr rax, 41
    btr rax, 62
    mov qword [rsi + 88], rax
    cmp ecx, 13
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 96]
    btr rax, 19
    btr rax, 40
    btr rax, 61
    mov qword [rsi + 96], rax
    cmp ecx, 14
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 104]
    btr rax, 18
    btr rax, 39
    btr rax, 60
    mov qword [rsi + 104], rax
    cmp ecx, 15
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 112]
    btr rax, 17
    btr rax, 38
    btr rax, 59
    mov qword [rsi + 112], rax
    cmp ecx, 16
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 120]
    btr rax, 16
    btr rax, 37
    btr rax, 58
    mov qword [rsi + 120], rax
    cmp ecx, 17
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 128]
    btr rax, 15
    btr rax, 36
    btr rax, 57
    mov qword [rsi + 128], rax
    cmp ecx, 18
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 136]
    btr rax, 14
    btr rax, 35
    btr rax, 56
    mov qword [rsi + 136], rax
    cmp ecx, 19
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 144]
    btr rax, 13
    btr rax, 34
    btr rax, 55
    mov qword [rsi + 144], rax
    cmp ecx, 20
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 152]
    btr rax, 12
    btr rax, 33
    btr rax, 54
    mov qword [rsi + 152], rax
    cmp ecx, 21
    jb run_sieve_dense_restore_021
    mov rax, qword [rsi + 160]
    btr rax, 11
    btr rax, 32
    btr rax, 53
    mov qword [rsi + 160], rax
run_sieve_dense_restore_021:
    bts qword [r14], 10
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_023:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 23
    jb run_sieve_dense_tail_023
align 16
run_sieve_dense_loop_023:
    mov rax, qword [rsi]
    btr rax, 11
    btr rax, 34
    btr rax, 57
    mov qword [rsi], rax
    mov rax, qword [rsi + 8]
    btr rax, 16
    btr rax, 39
    btr rax, 62
    mov qword [rsi + 8], rax
    mov rax, qword [rsi + 16]
    btr rax, 21
    btr rax, 44
    mov qword [rsi + 16], rax
    mov rax, qword [rsi + 24]
    btr rax, 3
    btr rax, 26
    btr rax, 49
    mov qword [rsi + 24], rax
    mov rax, qword [rsi + 32]
    btr rax, 8
    btr rax, 31
    btr rax, 54
    mov qword [rsi + 32], rax
    mov rax, qword [rsi + 40]
    btr rax, 13
    btr rax, 36
    btr rax, 59
    mov qword [rsi + 40], rax
    mov rax, qword [rsi + 48]
    btr rax, 18
    btr rax, 41
    mov qword [rsi + 48], rax
    mov rax, qword [rsi + 56]
    btr rax, 0
    btr rax, 23
    btr rax, 46
    mov qword [rsi + 56], rax
    mov rax, qword [rsi + 64]
    btr rax, 5
    btr rax, 28
    btr rax, 51
    mov qword [rsi + 64], rax
    mov rax, qword [rsi + 72]
    btr rax, 10
    btr rax, 33
    btr rax, 56
    mov qword [rsi + 72], rax
    mov rax, qword [rsi + 80]
    btr rax, 15
    btr rax, 38
    btr rax, 61
    mov qword [rsi + 80], rax
    mov rax, qword [rsi + 88]
    btr rax, 20
    btr rax, 43
    mov qword [rsi + 88], rax
    mov rax, qword [rsi + 96]
    btr rax, 2
    btr rax, 25
    btr rax, 48
    mov qword [rsi + 96], rax
    mov rax, qword [rsi + 104]
    btr rax, 7
    btr rax, 30
    btr rax, 53
    mov qword [rsi + 104], rax
    mov rax, qword [rsi + 112]
    btr rax, 12
    btr rax, 35
    btr rax, 58
    mov qword [rsi + 112], rax
    mov rax, qword [rsi + 120]
    btr rax, 17
    btr rax, 40
    btr rax, 63
    mov qword [rsi + 120], rax
    mov rax, qword [rsi + 128]
    btr rax, 22
    btr rax, 45
    mov qword [rsi + 128], rax
    mov rax, qword [rsi + 136]
    btr rax, 4
    btr rax, 27
    btr rax, 50
    mov qword [rsi + 136], rax
    mov rax, qword [rsi + 144]
    btr rax, 9
    btr rax, 32
    btr rax, 55
    mov qword [rsi + 144], rax
    mov rax, qword [rsi + 152]
    btr rax, 14
    btr rax, 37
    btr rax, 60
    mov qword [rsi + 152], rax
    mov rax, qword [rsi + 160]
    btr rax, 19
    btr rax, 42
    mov qword [rsi + 160], rax
    mov rax, qword [rsi + 168]
    btr rax, 1
    btr rax, 24
    btr rax, 47
    mov qword [rsi + 168], rax
    mov rax, qword [rsi + 176]
    btr rax, 6
    btr rax, 29
    btr rax, 52
    mov qword [rsi + 176], rax
    add rsi, 184
    sub ecx, 23
    cmp ecx, 23
    jae run_sieve_dense_loop_023
run_sieve_dense_tail_023:
    test ecx, ecx
    jz run_sieve_dense_restore_023
    cmp ecx, 1
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi]
    btr rax, 11
    btr rax, 34
    btr rax, 57
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 8]
    btr rax, 16
    btr rax, 39
    btr rax, 62
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 16]
    btr rax, 21
    btr rax, 44
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 24]
    btr rax, 3
    btr rax, 26
    btr rax, 49
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 32]
    btr rax, 8
    btr rax, 31
    btr rax, 54
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 40]
    btr rax, 13
    btr rax, 36
    btr rax, 59
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 48]
    btr rax, 18
    btr rax, 41
    mov qword [rsi + 48], rax
    cmp ecx, 8
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 56]
    btr rax, 0
    btr rax, 23
    btr rax, 46
    mov qword [rsi + 56], rax
    cmp ecx, 9
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 64]
    btr rax, 5
    btr rax, 28
    btr rax, 51
    mov qword [rsi + 64], rax
    cmp ecx, 10
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 72]
    btr rax, 10
    btr rax, 33
    btr rax, 56
    mov qword [rsi + 72], rax
    cmp ecx, 11
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 80]
    btr rax, 15
    btr rax, 38
    btr rax, 61
    mov qword [rsi + 80], rax
    cmp ecx, 12
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 88]
    btr rax, 20
    btr rax, 43
    mov qword [rsi + 88], rax
    cmp ecx, 13
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 96]
    btr rax, 2
    btr rax, 25
    btr rax, 48
    mov qword [rsi + 96], rax
    cmp ecx, 14
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 104]
    btr rax, 7
    btr rax, 30
    btr rax, 53
    mov qword [rsi + 104], rax
    cmp ecx, 15
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 112]
    btr rax, 12
    btr rax, 35
    btr rax, 58
    mov qword [rsi + 112], rax
    cmp ecx, 16
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 120]
    btr rax, 17
    btr rax, 40
    btr rax, 63
    mov qword [rsi + 120], rax
    cmp ecx, 17
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 128]
    btr rax, 22
    btr rax, 45
    mov qword [rsi + 128], rax
    cmp ecx, 18
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 136]
    btr rax, 4
    btr rax, 27
    btr rax, 50
    mov qword [rsi + 136], rax
    cmp ecx, 19
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 144]
    btr rax, 9
    btr rax, 32
    btr rax, 55
    mov qword [rsi + 144], rax
    cmp ecx, 20
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 152]
    btr rax, 14
    btr rax, 37
    btr rax, 60
    mov qword [rsi + 152], rax
    cmp ecx, 21
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 160]
    btr rax, 19
    btr rax, 42
    mov qword [rsi + 160], rax
    cmp ecx, 22
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 168]
    btr rax, 1
    btr rax, 24
    btr rax, 47
    mov qword [rsi + 168], rax
    cmp ecx, 23
    jb run_sieve_dense_restore_023
    mov rax, qword [rsi + 176]
    btr rax, 6
    btr rax, 29
    btr rax, 52
    mov qword [rsi + 176], rax
run_sieve_dense_restore_023:
    bts qword [r14], 11
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_025:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 25
    jb run_sieve_dense_tail_025
align 16
run_sieve_dense_loop_025:
    mov rax, qword [rsi]
    btr rax, 12
    btr rax, 37
    btr rax, 62
    mov qword [rsi], rax
    mov rax, qword [rsi + 8]
    btr rax, 23
    btr rax, 48
    mov qword [rsi + 8], rax
    mov rax, qword [rsi + 16]
    btr rax, 9
    btr rax, 34
    btr rax, 59
    mov qword [rsi + 16], rax
    mov rax, qword [rsi + 24]
    btr rax, 20
    btr rax, 45
    mov qword [rsi + 24], rax
    mov rax, qword [rsi + 32]
    btr rax, 6
    btr rax, 31
    btr rax, 56
    mov qword [rsi + 32], rax
    mov rax, qword [rsi + 40]
    btr rax, 17
    btr rax, 42
    mov qword [rsi + 40], rax
    mov rax, qword [rsi + 48]
    btr rax, 3
    btr rax, 28
    btr rax, 53
    mov qword [rsi + 48], rax
    mov rax, qword [rsi + 56]
    btr rax, 14
    btr rax, 39
    mov qword [rsi + 56], rax
    mov rax, qword [rsi + 64]
    btr rax, 0
    btr rax, 25
    btr rax, 50
    mov qword [rsi + 64], rax
    mov rax, qword [rsi + 72]
    btr rax, 11
    btr rax, 36
    btr rax, 61
    mov qword [rsi + 72], rax
    mov rax, qword [rsi + 80]
    btr rax, 22
    btr rax, 47
    mov qword [rsi + 80], rax
    mov rax, qword [rsi + 88]
    btr rax, 8
    btr rax, 33
    btr rax, 58
    mov qword [rsi + 88], rax
    mov rax, qword [rsi + 96]
    btr rax, 19
    btr rax, 44
    mov qword [rsi + 96], rax
    mov rax, qword [rsi + 104]
    btr rax, 5
    btr rax, 30
    btr rax, 55
    mov qword [rsi + 104], rax
    mov rax, qword [rsi + 112]
    btr rax, 16
    btr rax, 41
    mov qword [rsi + 112], rax
    mov rax, qword [rsi + 120]
    btr rax, 2
    btr rax, 27
    btr rax, 52
    mov qword [rsi + 120], rax
    mov rax, qword [rsi + 128]
    btr rax, 13
    btr rax, 38
    btr rax, 63
    mov qword [rsi + 128], rax
    mov rax, qword [rsi + 136]
    btr rax, 24
    btr rax, 49
    mov qword [rsi + 136], rax
    mov rax, qword [rsi + 144]
    btr rax, 10
    btr rax, 35
    btr rax, 60
    mov qword [rsi + 144], rax
    mov rax, qword [rsi + 152]
    btr rax, 21
    btr rax, 46
    mov qword [rsi + 152], rax
    mov rax, qword [rsi + 160]
    btr rax, 7
    btr rax, 32
    btr rax, 57
    mov qword [rsi + 160], rax
    mov rax, qword [rsi + 168]
    btr rax, 18
    btr rax, 43
    mov qword [rsi + 168], rax
    mov rax, qword [rsi + 176]
    btr rax, 4
    btr rax, 29
    btr rax, 54
    mov qword [rsi + 176], rax
    mov rax, qword [rsi + 184]
    btr rax, 15
    btr rax, 40
    mov qword [rsi + 184], rax
    mov rax, qword [rsi + 192]
    btr rax, 1
    btr rax, 26
    btr rax, 51
    mov qword [rsi + 192], rax
    add rsi, 200
    sub ecx, 25
    cmp ecx, 25
    jae run_sieve_dense_loop_025
run_sieve_dense_tail_025:
    test ecx, ecx
    jz run_sieve_dense_restore_025
    cmp ecx, 1
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi]
    btr rax, 12
    btr rax, 37
    btr rax, 62
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 8]
    btr rax, 23
    btr rax, 48
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 16]
    btr rax, 9
    btr rax, 34
    btr rax, 59
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 24]
    btr rax, 20
    btr rax, 45
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 32]
    btr rax, 6
    btr rax, 31
    btr rax, 56
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 40]
    btr rax, 17
    btr rax, 42
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 48]
    btr rax, 3
    btr rax, 28
    btr rax, 53
    mov qword [rsi + 48], rax
    cmp ecx, 8
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 56]
    btr rax, 14
    btr rax, 39
    mov qword [rsi + 56], rax
    cmp ecx, 9
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 64]
    btr rax, 0
    btr rax, 25
    btr rax, 50
    mov qword [rsi + 64], rax
    cmp ecx, 10
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 72]
    btr rax, 11
    btr rax, 36
    btr rax, 61
    mov qword [rsi + 72], rax
    cmp ecx, 11
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 80]
    btr rax, 22
    btr rax, 47
    mov qword [rsi + 80], rax
    cmp ecx, 12
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 88]
    btr rax, 8
    btr rax, 33
    btr rax, 58
    mov qword [rsi + 88], rax
    cmp ecx, 13
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 96]
    btr rax, 19
    btr rax, 44
    mov qword [rsi + 96], rax
    cmp ecx, 14
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 104]
    btr rax, 5
    btr rax, 30
    btr rax, 55
    mov qword [rsi + 104], rax
    cmp ecx, 15
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 112]
    btr rax, 16
    btr rax, 41
    mov qword [rsi + 112], rax
    cmp ecx, 16
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 120]
    btr rax, 2
    btr rax, 27
    btr rax, 52
    mov qword [rsi + 120], rax
    cmp ecx, 17
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 128]
    btr rax, 13
    btr rax, 38
    btr rax, 63
    mov qword [rsi + 128], rax
    cmp ecx, 18
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 136]
    btr rax, 24
    btr rax, 49
    mov qword [rsi + 136], rax
    cmp ecx, 19
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 144]
    btr rax, 10
    btr rax, 35
    btr rax, 60
    mov qword [rsi + 144], rax
    cmp ecx, 20
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 152]
    btr rax, 21
    btr rax, 46
    mov qword [rsi + 152], rax
    cmp ecx, 21
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 160]
    btr rax, 7
    btr rax, 32
    btr rax, 57
    mov qword [rsi + 160], rax
    cmp ecx, 22
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 168]
    btr rax, 18
    btr rax, 43
    mov qword [rsi + 168], rax
    cmp ecx, 23
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 176]
    btr rax, 4
    btr rax, 29
    btr rax, 54
    mov qword [rsi + 176], rax
    cmp ecx, 24
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 184]
    btr rax, 15
    btr rax, 40
    mov qword [rsi + 184], rax
    cmp ecx, 25
    jb run_sieve_dense_restore_025
    mov rax, qword [rsi + 192]
    btr rax, 1
    btr rax, 26
    btr rax, 51
    mov qword [rsi + 192], rax
run_sieve_dense_restore_025:
    bts qword [r14], 12
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_027:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 27
    jb run_sieve_dense_tail_027
align 16
run_sieve_dense_loop_027:
    mov rax, qword [rsi]
    btr rax, 13
    btr rax, 40
    mov qword [rsi], rax
    mov rax, qword [rsi + 8]
    btr rax, 3
    btr rax, 30
    btr rax, 57
    mov qword [rsi + 8], rax
    mov rax, qword [rsi + 16]
    btr rax, 20
    btr rax, 47
    mov qword [rsi + 16], rax
    mov rax, qword [rsi + 24]
    btr rax, 10
    btr rax, 37
    mov qword [rsi + 24], rax
    mov rax, qword [rsi + 32]
    btr rax, 0
    btr rax, 27
    btr rax, 54
    mov qword [rsi + 32], rax
    mov rax, qword [rsi + 40]
    btr rax, 17
    btr rax, 44
    mov qword [rsi + 40], rax
    mov rax, qword [rsi + 48]
    btr rax, 7
    btr rax, 34
    btr rax, 61
    mov qword [rsi + 48], rax
    mov rax, qword [rsi + 56]
    btr rax, 24
    btr rax, 51
    mov qword [rsi + 56], rax
    mov rax, qword [rsi + 64]
    btr rax, 14
    btr rax, 41
    mov qword [rsi + 64], rax
    mov rax, qword [rsi + 72]
    btr rax, 4
    btr rax, 31
    btr rax, 58
    mov qword [rsi + 72], rax
    mov rax, qword [rsi + 80]
    btr rax, 21
    btr rax, 48
    mov qword [rsi + 80], rax
    mov rax, qword [rsi + 88]
    btr rax, 11
    btr rax, 38
    mov qword [rsi + 88], rax
    mov rax, qword [rsi + 96]
    btr rax, 1
    btr rax, 28
    btr rax, 55
    mov qword [rsi + 96], rax
    mov rax, qword [rsi + 104]
    btr rax, 18
    btr rax, 45
    mov qword [rsi + 104], rax
    mov rax, qword [rsi + 112]
    btr rax, 8
    btr rax, 35
    btr rax, 62
    mov qword [rsi + 112], rax
    mov rax, qword [rsi + 120]
    btr rax, 25
    btr rax, 52
    mov qword [rsi + 120], rax
    mov rax, qword [rsi + 128]
    btr rax, 15
    btr rax, 42
    mov qword [rsi + 128], rax
    mov rax, qword [rsi + 136]
    btr rax, 5
    btr rax, 32
    btr rax, 59
    mov qword [rsi + 136], rax
    mov rax, qword [rsi + 144]
    btr rax, 22
    btr rax, 49
    mov qword [rsi + 144], rax
    mov rax, qword [rsi + 152]
    btr rax, 12
    btr rax, 39
    mov qword [rsi + 152], rax
    mov rax, qword [rsi + 160]
    btr rax, 2
    btr rax, 29
    btr rax, 56
    mov qword [rsi + 160], rax
    mov rax, qword [rsi + 168]
    btr rax, 19
    btr rax, 46
    mov qword [rsi + 168], rax
    mov rax, qword [rsi + 176]
    btr rax, 9
    btr rax, 36
    btr rax, 63
    mov qword [rsi + 176], rax
    mov rax, qword [rsi + 184]
    btr rax, 26
    btr rax, 53
    mov qword [rsi + 184], rax
    mov rax, qword [rsi + 192]
    btr rax, 16
    btr rax, 43
    mov qword [rsi + 192], rax
    mov rax, qword [rsi + 200]
    btr rax, 6
    btr rax, 33
    btr rax, 60
    mov qword [rsi + 200], rax
    mov rax, qword [rsi + 208]
    btr rax, 23
    btr rax, 50
    mov qword [rsi + 208], rax
    add rsi, 216
    sub ecx, 27
    cmp ecx, 27
    jae run_sieve_dense_loop_027
run_sieve_dense_tail_027:
    test ecx, ecx
    jz run_sieve_dense_restore_027
    cmp ecx, 1
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi]
    btr rax, 13
    btr rax, 40
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 8]
    btr rax, 3
    btr rax, 30
    btr rax, 57
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 16]
    btr rax, 20
    btr rax, 47
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 24]
    btr rax, 10
    btr rax, 37
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 32]
    btr rax, 0
    btr rax, 27
    btr rax, 54
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 40]
    btr rax, 17
    btr rax, 44
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 48]
    btr rax, 7
    btr rax, 34
    btr rax, 61
    mov qword [rsi + 48], rax
    cmp ecx, 8
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 56]
    btr rax, 24
    btr rax, 51
    mov qword [rsi + 56], rax
    cmp ecx, 9
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 64]
    btr rax, 14
    btr rax, 41
    mov qword [rsi + 64], rax
    cmp ecx, 10
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 72]
    btr rax, 4
    btr rax, 31
    btr rax, 58
    mov qword [rsi + 72], rax
    cmp ecx, 11
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 80]
    btr rax, 21
    btr rax, 48
    mov qword [rsi + 80], rax
    cmp ecx, 12
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 88]
    btr rax, 11
    btr rax, 38
    mov qword [rsi + 88], rax
    cmp ecx, 13
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 96]
    btr rax, 1
    btr rax, 28
    btr rax, 55
    mov qword [rsi + 96], rax
    cmp ecx, 14
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 104]
    btr rax, 18
    btr rax, 45
    mov qword [rsi + 104], rax
    cmp ecx, 15
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 112]
    btr rax, 8
    btr rax, 35
    btr rax, 62
    mov qword [rsi + 112], rax
    cmp ecx, 16
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 120]
    btr rax, 25
    btr rax, 52
    mov qword [rsi + 120], rax
    cmp ecx, 17
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 128]
    btr rax, 15
    btr rax, 42
    mov qword [rsi + 128], rax
    cmp ecx, 18
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 136]
    btr rax, 5
    btr rax, 32
    btr rax, 59
    mov qword [rsi + 136], rax
    cmp ecx, 19
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 144]
    btr rax, 22
    btr rax, 49
    mov qword [rsi + 144], rax
    cmp ecx, 20
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 152]
    btr rax, 12
    btr rax, 39
    mov qword [rsi + 152], rax
    cmp ecx, 21
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 160]
    btr rax, 2
    btr rax, 29
    btr rax, 56
    mov qword [rsi + 160], rax
    cmp ecx, 22
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 168]
    btr rax, 19
    btr rax, 46
    mov qword [rsi + 168], rax
    cmp ecx, 23
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 176]
    btr rax, 9
    btr rax, 36
    btr rax, 63
    mov qword [rsi + 176], rax
    cmp ecx, 24
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 184]
    btr rax, 26
    btr rax, 53
    mov qword [rsi + 184], rax
    cmp ecx, 25
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 192]
    btr rax, 16
    btr rax, 43
    mov qword [rsi + 192], rax
    cmp ecx, 26
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 200]
    btr rax, 6
    btr rax, 33
    btr rax, 60
    mov qword [rsi + 200], rax
    cmp ecx, 27
    jb run_sieve_dense_restore_027
    mov rax, qword [rsi + 208]
    btr rax, 23
    btr rax, 50
    mov qword [rsi + 208], rax
run_sieve_dense_restore_027:
    bts qword [r14], 13
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_029:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 29
    jb run_sieve_dense_tail_029
align 16
run_sieve_dense_loop_029:
    mov rax, qword [rsi]
    btr rax, 14
    btr rax, 43
    mov qword [rsi], rax
    mov rax, qword [rsi + 8]
    btr rax, 8
    btr rax, 37
    mov qword [rsi + 8], rax
    mov rax, qword [rsi + 16]
    btr rax, 2
    btr rax, 31
    btr rax, 60
    mov qword [rsi + 16], rax
    mov rax, qword [rsi + 24]
    btr rax, 25
    btr rax, 54
    mov qword [rsi + 24], rax
    mov rax, qword [rsi + 32]
    btr rax, 19
    btr rax, 48
    mov qword [rsi + 32], rax
    mov rax, qword [rsi + 40]
    btr rax, 13
    btr rax, 42
    mov qword [rsi + 40], rax
    mov rax, qword [rsi + 48]
    btr rax, 7
    btr rax, 36
    mov qword [rsi + 48], rax
    mov rax, qword [rsi + 56]
    btr rax, 1
    btr rax, 30
    btr rax, 59
    mov qword [rsi + 56], rax
    mov rax, qword [rsi + 64]
    btr rax, 24
    btr rax, 53
    mov qword [rsi + 64], rax
    mov rax, qword [rsi + 72]
    btr rax, 18
    btr rax, 47
    mov qword [rsi + 72], rax
    mov rax, qword [rsi + 80]
    btr rax, 12
    btr rax, 41
    mov qword [rsi + 80], rax
    mov rax, qword [rsi + 88]
    btr rax, 6
    btr rax, 35
    mov qword [rsi + 88], rax
    mov rax, qword [rsi + 96]
    btr rax, 0
    btr rax, 29
    btr rax, 58
    mov qword [rsi + 96], rax
    mov rax, qword [rsi + 104]
    btr rax, 23
    btr rax, 52
    mov qword [rsi + 104], rax
    mov rax, qword [rsi + 112]
    btr rax, 17
    btr rax, 46
    mov qword [rsi + 112], rax
    mov rax, qword [rsi + 120]
    btr rax, 11
    btr rax, 40
    mov qword [rsi + 120], rax
    mov rax, qword [rsi + 128]
    btr rax, 5
    btr rax, 34
    btr rax, 63
    mov qword [rsi + 128], rax
    mov rax, qword [rsi + 136]
    btr rax, 28
    btr rax, 57
    mov qword [rsi + 136], rax
    mov rax, qword [rsi + 144]
    btr rax, 22
    btr rax, 51
    mov qword [rsi + 144], rax
    mov rax, qword [rsi + 152]
    btr rax, 16
    btr rax, 45
    mov qword [rsi + 152], rax
    mov rax, qword [rsi + 160]
    btr rax, 10
    btr rax, 39
    mov qword [rsi + 160], rax
    mov rax, qword [rsi + 168]
    btr rax, 4
    btr rax, 33
    btr rax, 62
    mov qword [rsi + 168], rax
    mov rax, qword [rsi + 176]
    btr rax, 27
    btr rax, 56
    mov qword [rsi + 176], rax
    mov rax, qword [rsi + 184]
    btr rax, 21
    btr rax, 50
    mov qword [rsi + 184], rax
    mov rax, qword [rsi + 192]
    btr rax, 15
    btr rax, 44
    mov qword [rsi + 192], rax
    mov rax, qword [rsi + 200]
    btr rax, 9
    btr rax, 38
    mov qword [rsi + 200], rax
    mov rax, qword [rsi + 208]
    btr rax, 3
    btr rax, 32
    btr rax, 61
    mov qword [rsi + 208], rax
    mov rax, qword [rsi + 216]
    btr rax, 26
    btr rax, 55
    mov qword [rsi + 216], rax
    mov rax, qword [rsi + 224]
    btr rax, 20
    btr rax, 49
    mov qword [rsi + 224], rax
    add rsi, 232
    sub ecx, 29
    cmp ecx, 29
    jae run_sieve_dense_loop_029
run_sieve_dense_tail_029:
    test ecx, ecx
    jz run_sieve_dense_restore_029
    cmp ecx, 1
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi]
    btr rax, 14
    btr rax, 43
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 8]
    btr rax, 8
    btr rax, 37
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 16]
    btr rax, 2
    btr rax, 31
    btr rax, 60
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 24]
    btr rax, 25
    btr rax, 54
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 32]
    btr rax, 19
    btr rax, 48
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 40]
    btr rax, 13
    btr rax, 42
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 48]
    btr rax, 7
    btr rax, 36
    mov qword [rsi + 48], rax
    cmp ecx, 8
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 56]
    btr rax, 1
    btr rax, 30
    btr rax, 59
    mov qword [rsi + 56], rax
    cmp ecx, 9
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 64]
    btr rax, 24
    btr rax, 53
    mov qword [rsi + 64], rax
    cmp ecx, 10
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 72]
    btr rax, 18
    btr rax, 47
    mov qword [rsi + 72], rax
    cmp ecx, 11
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 80]
    btr rax, 12
    btr rax, 41
    mov qword [rsi + 80], rax
    cmp ecx, 12
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 88]
    btr rax, 6
    btr rax, 35
    mov qword [rsi + 88], rax
    cmp ecx, 13
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 96]
    btr rax, 0
    btr rax, 29
    btr rax, 58
    mov qword [rsi + 96], rax
    cmp ecx, 14
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 104]
    btr rax, 23
    btr rax, 52
    mov qword [rsi + 104], rax
    cmp ecx, 15
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 112]
    btr rax, 17
    btr rax, 46
    mov qword [rsi + 112], rax
    cmp ecx, 16
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 120]
    btr rax, 11
    btr rax, 40
    mov qword [rsi + 120], rax
    cmp ecx, 17
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 128]
    btr rax, 5
    btr rax, 34
    btr rax, 63
    mov qword [rsi + 128], rax
    cmp ecx, 18
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 136]
    btr rax, 28
    btr rax, 57
    mov qword [rsi + 136], rax
    cmp ecx, 19
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 144]
    btr rax, 22
    btr rax, 51
    mov qword [rsi + 144], rax
    cmp ecx, 20
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 152]
    btr rax, 16
    btr rax, 45
    mov qword [rsi + 152], rax
    cmp ecx, 21
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 160]
    btr rax, 10
    btr rax, 39
    mov qword [rsi + 160], rax
    cmp ecx, 22
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 168]
    btr rax, 4
    btr rax, 33
    btr rax, 62
    mov qword [rsi + 168], rax
    cmp ecx, 23
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 176]
    btr rax, 27
    btr rax, 56
    mov qword [rsi + 176], rax
    cmp ecx, 24
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 184]
    btr rax, 21
    btr rax, 50
    mov qword [rsi + 184], rax
    cmp ecx, 25
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 192]
    btr rax, 15
    btr rax, 44
    mov qword [rsi + 192], rax
    cmp ecx, 26
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 200]
    btr rax, 9
    btr rax, 38
    mov qword [rsi + 200], rax
    cmp ecx, 27
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 208]
    btr rax, 3
    btr rax, 32
    btr rax, 61
    mov qword [rsi + 208], rax
    cmp ecx, 28
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 216]
    btr rax, 26
    btr rax, 55
    mov qword [rsi + 216], rax
    cmp ecx, 29
    jb run_sieve_dense_restore_029
    mov rax, qword [rsi + 224]
    btr rax, 20
    btr rax, 49
    mov qword [rsi + 224], rax
run_sieve_dense_restore_029:
    bts qword [r14], 14
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_031:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 31
    jb run_sieve_dense_tail_031
align 16
run_sieve_dense_loop_031:
    mov rax, qword [rsi]
    btr rax, 15
    btr rax, 46
    mov qword [rsi], rax
    mov rax, qword [rsi + 8]
    btr rax, 13
    btr rax, 44
    mov qword [rsi + 8], rax
    mov rax, qword [rsi + 16]
    btr rax, 11
    btr rax, 42
    mov qword [rsi + 16], rax
    mov rax, qword [rsi + 24]
    btr rax, 9
    btr rax, 40
    mov qword [rsi + 24], rax
    mov rax, qword [rsi + 32]
    btr rax, 7
    btr rax, 38
    mov qword [rsi + 32], rax
    mov rax, qword [rsi + 40]
    btr rax, 5
    btr rax, 36
    mov qword [rsi + 40], rax
    mov rax, qword [rsi + 48]
    btr rax, 3
    btr rax, 34
    mov qword [rsi + 48], rax
    mov rax, qword [rsi + 56]
    btr rax, 1
    btr rax, 32
    btr rax, 63
    mov qword [rsi + 56], rax
    mov rax, qword [rsi + 64]
    btr rax, 30
    btr rax, 61
    mov qword [rsi + 64], rax
    mov rax, qword [rsi + 72]
    btr rax, 28
    btr rax, 59
    mov qword [rsi + 72], rax
    mov rax, qword [rsi + 80]
    btr rax, 26
    btr rax, 57
    mov qword [rsi + 80], rax
    mov rax, qword [rsi + 88]
    btr rax, 24
    btr rax, 55
    mov qword [rsi + 88], rax
    mov rax, qword [rsi + 96]
    btr rax, 22
    btr rax, 53
    mov qword [rsi + 96], rax
    mov rax, qword [rsi + 104]
    btr rax, 20
    btr rax, 51
    mov qword [rsi + 104], rax
    mov rax, qword [rsi + 112]
    btr rax, 18
    btr rax, 49
    mov qword [rsi + 112], rax
    mov rax, qword [rsi + 120]
    btr rax, 16
    btr rax, 47
    mov qword [rsi + 120], rax
    mov rax, qword [rsi + 128]
    btr rax, 14
    btr rax, 45
    mov qword [rsi + 128], rax
    mov rax, qword [rsi + 136]
    btr rax, 12
    btr rax, 43
    mov qword [rsi + 136], rax
    mov rax, qword [rsi + 144]
    btr rax, 10
    btr rax, 41
    mov qword [rsi + 144], rax
    mov rax, qword [rsi + 152]
    btr rax, 8
    btr rax, 39
    mov qword [rsi + 152], rax
    mov rax, qword [rsi + 160]
    btr rax, 6
    btr rax, 37
    mov qword [rsi + 160], rax
    mov rax, qword [rsi + 168]
    btr rax, 4
    btr rax, 35
    mov qword [rsi + 168], rax
    mov rax, qword [rsi + 176]
    btr rax, 2
    btr rax, 33
    mov qword [rsi + 176], rax
    mov rax, qword [rsi + 184]
    btr rax, 0
    btr rax, 31
    btr rax, 62
    mov qword [rsi + 184], rax
    mov rax, qword [rsi + 192]
    btr rax, 29
    btr rax, 60
    mov qword [rsi + 192], rax
    mov rax, qword [rsi + 200]
    btr rax, 27
    btr rax, 58
    mov qword [rsi + 200], rax
    mov rax, qword [rsi + 208]
    btr rax, 25
    btr rax, 56
    mov qword [rsi + 208], rax
    mov rax, qword [rsi + 216]
    btr rax, 23
    btr rax, 54
    mov qword [rsi + 216], rax
    mov rax, qword [rsi + 224]
    btr rax, 21
    btr rax, 52
    mov qword [rsi + 224], rax
    mov rax, qword [rsi + 232]
    btr rax, 19
    btr rax, 50
    mov qword [rsi + 232], rax
    mov rax, qword [rsi + 240]
    btr rax, 17
    btr rax, 48
    mov qword [rsi + 240], rax
    add rsi, 248
    sub ecx, 31
    cmp ecx, 31
    jae run_sieve_dense_loop_031
run_sieve_dense_tail_031:
    test ecx, ecx
    jz run_sieve_dense_restore_031
    cmp ecx, 1
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi]
    btr rax, 15
    btr rax, 46
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 8]
    btr rax, 13
    btr rax, 44
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 16]
    btr rax, 11
    btr rax, 42
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 24]
    btr rax, 9
    btr rax, 40
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 32]
    btr rax, 7
    btr rax, 38
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 40]
    btr rax, 5
    btr rax, 36
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 48]
    btr rax, 3
    btr rax, 34
    mov qword [rsi + 48], rax
    cmp ecx, 8
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 56]
    btr rax, 1
    btr rax, 32
    btr rax, 63
    mov qword [rsi + 56], rax
    cmp ecx, 9
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 64]
    btr rax, 30
    btr rax, 61
    mov qword [rsi + 64], rax
    cmp ecx, 10
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 72]
    btr rax, 28
    btr rax, 59
    mov qword [rsi + 72], rax
    cmp ecx, 11
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 80]
    btr rax, 26
    btr rax, 57
    mov qword [rsi + 80], rax
    cmp ecx, 12
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 88]
    btr rax, 24
    btr rax, 55
    mov qword [rsi + 88], rax
    cmp ecx, 13
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 96]
    btr rax, 22
    btr rax, 53
    mov qword [rsi + 96], rax
    cmp ecx, 14
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 104]
    btr rax, 20
    btr rax, 51
    mov qword [rsi + 104], rax
    cmp ecx, 15
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 112]
    btr rax, 18
    btr rax, 49
    mov qword [rsi + 112], rax
    cmp ecx, 16
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 120]
    btr rax, 16
    btr rax, 47
    mov qword [rsi + 120], rax
    cmp ecx, 17
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 128]
    btr rax, 14
    btr rax, 45
    mov qword [rsi + 128], rax
    cmp ecx, 18
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 136]
    btr rax, 12
    btr rax, 43
    mov qword [rsi + 136], rax
    cmp ecx, 19
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 144]
    btr rax, 10
    btr rax, 41
    mov qword [rsi + 144], rax
    cmp ecx, 20
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 152]
    btr rax, 8
    btr rax, 39
    mov qword [rsi + 152], rax
    cmp ecx, 21
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 160]
    btr rax, 6
    btr rax, 37
    mov qword [rsi + 160], rax
    cmp ecx, 22
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 168]
    btr rax, 4
    btr rax, 35
    mov qword [rsi + 168], rax
    cmp ecx, 23
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 176]
    btr rax, 2
    btr rax, 33
    mov qword [rsi + 176], rax
    cmp ecx, 24
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 184]
    btr rax, 0
    btr rax, 31
    btr rax, 62
    mov qword [rsi + 184], rax
    cmp ecx, 25
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 192]
    btr rax, 29
    btr rax, 60
    mov qword [rsi + 192], rax
    cmp ecx, 26
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 200]
    btr rax, 27
    btr rax, 58
    mov qword [rsi + 200], rax
    cmp ecx, 27
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 208]
    btr rax, 25
    btr rax, 56
    mov qword [rsi + 208], rax
    cmp ecx, 28
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 216]
    btr rax, 23
    btr rax, 54
    mov qword [rsi + 216], rax
    cmp ecx, 29
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 224]
    btr rax, 21
    btr rax, 52
    mov qword [rsi + 224], rax
    cmp ecx, 30
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 232]
    btr rax, 19
    btr rax, 50
    mov qword [rsi + 232], rax
    cmp ecx, 31
    jb run_sieve_dense_restore_031
    mov rax, qword [rsi + 240]
    btr rax, 17
    btr rax, 48
    mov qword [rsi + 240], rax
run_sieve_dense_restore_031:
    bts qword [r14], 15
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_033:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 33
    jb run_sieve_dense_tail_033
align 16
run_sieve_dense_loop_033:
    mov rax, qword [rsi]
    btr rax, 16
    btr rax, 49
    mov qword [rsi], rax
    mov rax, qword [rsi + 8]
    btr rax, 18
    btr rax, 51
    mov qword [rsi + 8], rax
    mov rax, qword [rsi + 16]
    btr rax, 20
    btr rax, 53
    mov qword [rsi + 16], rax
    mov rax, qword [rsi + 24]
    btr rax, 22
    btr rax, 55
    mov qword [rsi + 24], rax
    mov rax, qword [rsi + 32]
    btr rax, 24
    btr rax, 57
    mov qword [rsi + 32], rax
    mov rax, qword [rsi + 40]
    btr rax, 26
    btr rax, 59
    mov qword [rsi + 40], rax
    mov rax, qword [rsi + 48]
    btr rax, 28
    btr rax, 61
    mov qword [rsi + 48], rax
    mov rax, qword [rsi + 56]
    btr rax, 30
    btr rax, 63
    mov qword [rsi + 56], rax
    btr qword [rsi + 64], 32
    mov rax, qword [rsi + 72]
    btr rax, 1
    btr rax, 34
    mov qword [rsi + 72], rax
    mov rax, qword [rsi + 80]
    btr rax, 3
    btr rax, 36
    mov qword [rsi + 80], rax
    mov rax, qword [rsi + 88]
    btr rax, 5
    btr rax, 38
    mov qword [rsi + 88], rax
    mov rax, qword [rsi + 96]
    btr rax, 7
    btr rax, 40
    mov qword [rsi + 96], rax
    mov rax, qword [rsi + 104]
    btr rax, 9
    btr rax, 42
    mov qword [rsi + 104], rax
    mov rax, qword [rsi + 112]
    btr rax, 11
    btr rax, 44
    mov qword [rsi + 112], rax
    mov rax, qword [rsi + 120]
    btr rax, 13
    btr rax, 46
    mov qword [rsi + 120], rax
    mov rax, qword [rsi + 128]
    btr rax, 15
    btr rax, 48
    mov qword [rsi + 128], rax
    mov rax, qword [rsi + 136]
    btr rax, 17
    btr rax, 50
    mov qword [rsi + 136], rax
    mov rax, qword [rsi + 144]
    btr rax, 19
    btr rax, 52
    mov qword [rsi + 144], rax
    mov rax, qword [rsi + 152]
    btr rax, 21
    btr rax, 54
    mov qword [rsi + 152], rax
    mov rax, qword [rsi + 160]
    btr rax, 23
    btr rax, 56
    mov qword [rsi + 160], rax
    mov rax, qword [rsi + 168]
    btr rax, 25
    btr rax, 58
    mov qword [rsi + 168], rax
    mov rax, qword [rsi + 176]
    btr rax, 27
    btr rax, 60
    mov qword [rsi + 176], rax
    mov rax, qword [rsi + 184]
    btr rax, 29
    btr rax, 62
    mov qword [rsi + 184], rax
    btr qword [rsi + 192], 31
    mov rax, qword [rsi + 200]
    btr rax, 0
    btr rax, 33
    mov qword [rsi + 200], rax
    mov rax, qword [rsi + 208]
    btr rax, 2
    btr rax, 35
    mov qword [rsi + 208], rax
    mov rax, qword [rsi + 216]
    btr rax, 4
    btr rax, 37
    mov qword [rsi + 216], rax
    mov rax, qword [rsi + 224]
    btr rax, 6
    btr rax, 39
    mov qword [rsi + 224], rax
    mov rax, qword [rsi + 232]
    btr rax, 8
    btr rax, 41
    mov qword [rsi + 232], rax
    mov rax, qword [rsi + 240]
    btr rax, 10
    btr rax, 43
    mov qword [rsi + 240], rax
    mov rax, qword [rsi + 248]
    btr rax, 12
    btr rax, 45
    mov qword [rsi + 248], rax
    mov rax, qword [rsi + 256]
    btr rax, 14
    btr rax, 47
    mov qword [rsi + 256], rax
    add rsi, 264
    sub ecx, 33
    cmp ecx, 33
    jae run_sieve_dense_loop_033
run_sieve_dense_tail_033:
    test ecx, ecx
    jz run_sieve_dense_restore_033
    cmp ecx, 1
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi]
    btr rax, 16
    btr rax, 49
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 8]
    btr rax, 18
    btr rax, 51
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 16]
    btr rax, 20
    btr rax, 53
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 24]
    btr rax, 22
    btr rax, 55
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 32]
    btr rax, 24
    btr rax, 57
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 40]
    btr rax, 26
    btr rax, 59
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 48]
    btr rax, 28
    btr rax, 61
    mov qword [rsi + 48], rax
    cmp ecx, 8
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 56]
    btr rax, 30
    btr rax, 63
    mov qword [rsi + 56], rax
    cmp ecx, 9
    jb run_sieve_dense_restore_033
    btr qword [rsi + 64], 32
    cmp ecx, 10
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 72]
    btr rax, 1
    btr rax, 34
    mov qword [rsi + 72], rax
    cmp ecx, 11
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 80]
    btr rax, 3
    btr rax, 36
    mov qword [rsi + 80], rax
    cmp ecx, 12
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 88]
    btr rax, 5
    btr rax, 38
    mov qword [rsi + 88], rax
    cmp ecx, 13
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 96]
    btr rax, 7
    btr rax, 40
    mov qword [rsi + 96], rax
    cmp ecx, 14
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 104]
    btr rax, 9
    btr rax, 42
    mov qword [rsi + 104], rax
    cmp ecx, 15
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 112]
    btr rax, 11
    btr rax, 44
    mov qword [rsi + 112], rax
    cmp ecx, 16
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 120]
    btr rax, 13
    btr rax, 46
    mov qword [rsi + 120], rax
    cmp ecx, 17
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 128]
    btr rax, 15
    btr rax, 48
    mov qword [rsi + 128], rax
    cmp ecx, 18
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 136]
    btr rax, 17
    btr rax, 50
    mov qword [rsi + 136], rax
    cmp ecx, 19
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 144]
    btr rax, 19
    btr rax, 52
    mov qword [rsi + 144], rax
    cmp ecx, 20
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 152]
    btr rax, 21
    btr rax, 54
    mov qword [rsi + 152], rax
    cmp ecx, 21
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 160]
    btr rax, 23
    btr rax, 56
    mov qword [rsi + 160], rax
    cmp ecx, 22
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 168]
    btr rax, 25
    btr rax, 58
    mov qword [rsi + 168], rax
    cmp ecx, 23
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 176]
    btr rax, 27
    btr rax, 60
    mov qword [rsi + 176], rax
    cmp ecx, 24
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 184]
    btr rax, 29
    btr rax, 62
    mov qword [rsi + 184], rax
    cmp ecx, 25
    jb run_sieve_dense_restore_033
    btr qword [rsi + 192], 31
    cmp ecx, 26
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 200]
    btr rax, 0
    btr rax, 33
    mov qword [rsi + 200], rax
    cmp ecx, 27
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 208]
    btr rax, 2
    btr rax, 35
    mov qword [rsi + 208], rax
    cmp ecx, 28
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 216]
    btr rax, 4
    btr rax, 37
    mov qword [rsi + 216], rax
    cmp ecx, 29
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 224]
    btr rax, 6
    btr rax, 39
    mov qword [rsi + 224], rax
    cmp ecx, 30
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 232]
    btr rax, 8
    btr rax, 41
    mov qword [rsi + 232], rax
    cmp ecx, 31
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 240]
    btr rax, 10
    btr rax, 43
    mov qword [rsi + 240], rax
    cmp ecx, 32
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 248]
    btr rax, 12
    btr rax, 45
    mov qword [rsi + 248], rax
    cmp ecx, 33
    jb run_sieve_dense_restore_033
    mov rax, qword [rsi + 256]
    btr rax, 14
    btr rax, 47
    mov qword [rsi + 256], rax
run_sieve_dense_restore_033:
    bts qword [r14], 16
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_035:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 35
    jb run_sieve_dense_tail_035
align 16
run_sieve_dense_loop_035:
    mov rax, qword [rsi]
    btr rax, 17
    btr rax, 52
    mov qword [rsi], rax
    mov rax, qword [rsi + 8]
    btr rax, 23
    btr rax, 58
    mov qword [rsi + 8], rax
    btr qword [rsi + 16], 29
    mov rax, qword [rsi + 24]
    btr rax, 0
    btr rax, 35
    mov qword [rsi + 24], rax
    mov rax, qword [rsi + 32]
    btr rax, 6
    btr rax, 41
    mov qword [rsi + 32], rax
    mov rax, qword [rsi + 40]
    btr rax, 12
    btr rax, 47
    mov qword [rsi + 40], rax
    mov rax, qword [rsi + 48]
    btr rax, 18
    btr rax, 53
    mov qword [rsi + 48], rax
    mov rax, qword [rsi + 56]
    btr rax, 24
    btr rax, 59
    mov qword [rsi + 56], rax
    btr qword [rsi + 64], 30
    mov rax, qword [rsi + 72]
    btr rax, 1
    btr rax, 36
    mov qword [rsi + 72], rax
    mov rax, qword [rsi + 80]
    btr rax, 7
    btr rax, 42
    mov qword [rsi + 80], rax
    mov rax, qword [rsi + 88]
    btr rax, 13
    btr rax, 48
    mov qword [rsi + 88], rax
    mov rax, qword [rsi + 96]
    btr rax, 19
    btr rax, 54
    mov qword [rsi + 96], rax
    mov rax, qword [rsi + 104]
    btr rax, 25
    btr rax, 60
    mov qword [rsi + 104], rax
    btr qword [rsi + 112], 31
    mov rax, qword [rsi + 120]
    btr rax, 2
    btr rax, 37
    mov qword [rsi + 120], rax
    mov rax, qword [rsi + 128]
    btr rax, 8
    btr rax, 43
    mov qword [rsi + 128], rax
    mov rax, qword [rsi + 136]
    btr rax, 14
    btr rax, 49
    mov qword [rsi + 136], rax
    mov rax, qword [rsi + 144]
    btr rax, 20
    btr rax, 55
    mov qword [rsi + 144], rax
    mov rax, qword [rsi + 152]
    btr rax, 26
    btr rax, 61
    mov qword [rsi + 152], rax
    btr qword [rsi + 160], 32
    mov rax, qword [rsi + 168]
    btr rax, 3
    btr rax, 38
    mov qword [rsi + 168], rax
    mov rax, qword [rsi + 176]
    btr rax, 9
    btr rax, 44
    mov qword [rsi + 176], rax
    mov rax, qword [rsi + 184]
    btr rax, 15
    btr rax, 50
    mov qword [rsi + 184], rax
    mov rax, qword [rsi + 192]
    btr rax, 21
    btr rax, 56
    mov qword [rsi + 192], rax
    mov rax, qword [rsi + 200]
    btr rax, 27
    btr rax, 62
    mov qword [rsi + 200], rax
    btr qword [rsi + 208], 33
    mov rax, qword [rsi + 216]
    btr rax, 4
    btr rax, 39
    mov qword [rsi + 216], rax
    mov rax, qword [rsi + 224]
    btr rax, 10
    btr rax, 45
    mov qword [rsi + 224], rax
    mov rax, qword [rsi + 232]
    btr rax, 16
    btr rax, 51
    mov qword [rsi + 232], rax
    mov rax, qword [rsi + 240]
    btr rax, 22
    btr rax, 57
    mov qword [rsi + 240], rax
    mov rax, qword [rsi + 248]
    btr rax, 28
    btr rax, 63
    mov qword [rsi + 248], rax
    btr qword [rsi + 256], 34
    mov rax, qword [rsi + 264]
    btr rax, 5
    btr rax, 40
    mov qword [rsi + 264], rax
    mov rax, qword [rsi + 272]
    btr rax, 11
    btr rax, 46
    mov qword [rsi + 272], rax
    add rsi, 280
    sub ecx, 35
    cmp ecx, 35
    jae run_sieve_dense_loop_035
run_sieve_dense_tail_035:
    test ecx, ecx
    jz run_sieve_dense_restore_035
    cmp ecx, 1
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi]
    btr rax, 17
    btr rax, 52
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 8]
    btr rax, 23
    btr rax, 58
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_035
    btr qword [rsi + 16], 29
    cmp ecx, 4
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 24]
    btr rax, 0
    btr rax, 35
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 32]
    btr rax, 6
    btr rax, 41
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 40]
    btr rax, 12
    btr rax, 47
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 48]
    btr rax, 18
    btr rax, 53
    mov qword [rsi + 48], rax
    cmp ecx, 8
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 56]
    btr rax, 24
    btr rax, 59
    mov qword [rsi + 56], rax
    cmp ecx, 9
    jb run_sieve_dense_restore_035
    btr qword [rsi + 64], 30
    cmp ecx, 10
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 72]
    btr rax, 1
    btr rax, 36
    mov qword [rsi + 72], rax
    cmp ecx, 11
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 80]
    btr rax, 7
    btr rax, 42
    mov qword [rsi + 80], rax
    cmp ecx, 12
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 88]
    btr rax, 13
    btr rax, 48
    mov qword [rsi + 88], rax
    cmp ecx, 13
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 96]
    btr rax, 19
    btr rax, 54
    mov qword [rsi + 96], rax
    cmp ecx, 14
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 104]
    btr rax, 25
    btr rax, 60
    mov qword [rsi + 104], rax
    cmp ecx, 15
    jb run_sieve_dense_restore_035
    btr qword [rsi + 112], 31
    cmp ecx, 16
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 120]
    btr rax, 2
    btr rax, 37
    mov qword [rsi + 120], rax
    cmp ecx, 17
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 128]
    btr rax, 8
    btr rax, 43
    mov qword [rsi + 128], rax
    cmp ecx, 18
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 136]
    btr rax, 14
    btr rax, 49
    mov qword [rsi + 136], rax
    cmp ecx, 19
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 144]
    btr rax, 20
    btr rax, 55
    mov qword [rsi + 144], rax
    cmp ecx, 20
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 152]
    btr rax, 26
    btr rax, 61
    mov qword [rsi + 152], rax
    cmp ecx, 21
    jb run_sieve_dense_restore_035
    btr qword [rsi + 160], 32
    cmp ecx, 22
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 168]
    btr rax, 3
    btr rax, 38
    mov qword [rsi + 168], rax
    cmp ecx, 23
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 176]
    btr rax, 9
    btr rax, 44
    mov qword [rsi + 176], rax
    cmp ecx, 24
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 184]
    btr rax, 15
    btr rax, 50
    mov qword [rsi + 184], rax
    cmp ecx, 25
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 192]
    btr rax, 21
    btr rax, 56
    mov qword [rsi + 192], rax
    cmp ecx, 26
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 200]
    btr rax, 27
    btr rax, 62
    mov qword [rsi + 200], rax
    cmp ecx, 27
    jb run_sieve_dense_restore_035
    btr qword [rsi + 208], 33
    cmp ecx, 28
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 216]
    btr rax, 4
    btr rax, 39
    mov qword [rsi + 216], rax
    cmp ecx, 29
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 224]
    btr rax, 10
    btr rax, 45
    mov qword [rsi + 224], rax
    cmp ecx, 30
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 232]
    btr rax, 16
    btr rax, 51
    mov qword [rsi + 232], rax
    cmp ecx, 31
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 240]
    btr rax, 22
    btr rax, 57
    mov qword [rsi + 240], rax
    cmp ecx, 32
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 248]
    btr rax, 28
    btr rax, 63
    mov qword [rsi + 248], rax
    cmp ecx, 33
    jb run_sieve_dense_restore_035
    btr qword [rsi + 256], 34
    cmp ecx, 34
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 264]
    btr rax, 5
    btr rax, 40
    mov qword [rsi + 264], rax
    cmp ecx, 35
    jb run_sieve_dense_restore_035
    mov rax, qword [rsi + 272]
    btr rax, 11
    btr rax, 46
    mov qword [rsi + 272], rax
run_sieve_dense_restore_035:
    bts qword [r14], 17
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_037:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 37
    jb run_sieve_dense_tail_037
align 16
run_sieve_dense_loop_037:
    mov rax, qword [rsi]
    btr rax, 18
    btr rax, 55
    mov qword [rsi], rax
    btr qword [rsi + 8], 28
    mov rax, qword [rsi + 16]
    btr rax, 1
    btr rax, 38
    mov qword [rsi + 16], rax
    mov rax, qword [rsi + 24]
    btr rax, 11
    btr rax, 48
    mov qword [rsi + 24], rax
    mov rax, qword [rsi + 32]
    btr rax, 21
    btr rax, 58
    mov qword [rsi + 32], rax
    btr qword [rsi + 40], 31
    mov rax, qword [rsi + 48]
    btr rax, 4
    btr rax, 41
    mov qword [rsi + 48], rax
    mov rax, qword [rsi + 56]
    btr rax, 14
    btr rax, 51
    mov qword [rsi + 56], rax
    mov rax, qword [rsi + 64]
    btr rax, 24
    btr rax, 61
    mov qword [rsi + 64], rax
    btr qword [rsi + 72], 34
    mov rax, qword [rsi + 80]
    btr rax, 7
    btr rax, 44
    mov qword [rsi + 80], rax
    mov rax, qword [rsi + 88]
    btr rax, 17
    btr rax, 54
    mov qword [rsi + 88], rax
    btr qword [rsi + 96], 27
    mov rax, qword [rsi + 104]
    btr rax, 0
    btr rax, 37
    mov qword [rsi + 104], rax
    mov rax, qword [rsi + 112]
    btr rax, 10
    btr rax, 47
    mov qword [rsi + 112], rax
    mov rax, qword [rsi + 120]
    btr rax, 20
    btr rax, 57
    mov qword [rsi + 120], rax
    btr qword [rsi + 128], 30
    mov rax, qword [rsi + 136]
    btr rax, 3
    btr rax, 40
    mov qword [rsi + 136], rax
    mov rax, qword [rsi + 144]
    btr rax, 13
    btr rax, 50
    mov qword [rsi + 144], rax
    mov rax, qword [rsi + 152]
    btr rax, 23
    btr rax, 60
    mov qword [rsi + 152], rax
    btr qword [rsi + 160], 33
    mov rax, qword [rsi + 168]
    btr rax, 6
    btr rax, 43
    mov qword [rsi + 168], rax
    mov rax, qword [rsi + 176]
    btr rax, 16
    btr rax, 53
    mov qword [rsi + 176], rax
    mov rax, qword [rsi + 184]
    btr rax, 26
    btr rax, 63
    mov qword [rsi + 184], rax
    btr qword [rsi + 192], 36
    mov rax, qword [rsi + 200]
    btr rax, 9
    btr rax, 46
    mov qword [rsi + 200], rax
    mov rax, qword [rsi + 208]
    btr rax, 19
    btr rax, 56
    mov qword [rsi + 208], rax
    btr qword [rsi + 216], 29
    mov rax, qword [rsi + 224]
    btr rax, 2
    btr rax, 39
    mov qword [rsi + 224], rax
    mov rax, qword [rsi + 232]
    btr rax, 12
    btr rax, 49
    mov qword [rsi + 232], rax
    mov rax, qword [rsi + 240]
    btr rax, 22
    btr rax, 59
    mov qword [rsi + 240], rax
    btr qword [rsi + 248], 32
    mov rax, qword [rsi + 256]
    btr rax, 5
    btr rax, 42
    mov qword [rsi + 256], rax
    mov rax, qword [rsi + 264]
    btr rax, 15
    btr rax, 52
    mov qword [rsi + 264], rax
    mov rax, qword [rsi + 272]
    btr rax, 25
    btr rax, 62
    mov qword [rsi + 272], rax
    btr qword [rsi + 280], 35
    mov rax, qword [rsi + 288]
    btr rax, 8
    btr rax, 45
    mov qword [rsi + 288], rax
    add rsi, 296
    sub ecx, 37
    cmp ecx, 37
    jae run_sieve_dense_loop_037
run_sieve_dense_tail_037:
    test ecx, ecx
    jz run_sieve_dense_restore_037
    cmp ecx, 1
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi]
    btr rax, 18
    btr rax, 55
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_037
    btr qword [rsi + 8], 28
    cmp ecx, 3
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 16]
    btr rax, 1
    btr rax, 38
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 24]
    btr rax, 11
    btr rax, 48
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 32]
    btr rax, 21
    btr rax, 58
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_037
    btr qword [rsi + 40], 31
    cmp ecx, 7
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 48]
    btr rax, 4
    btr rax, 41
    mov qword [rsi + 48], rax
    cmp ecx, 8
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 56]
    btr rax, 14
    btr rax, 51
    mov qword [rsi + 56], rax
    cmp ecx, 9
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 64]
    btr rax, 24
    btr rax, 61
    mov qword [rsi + 64], rax
    cmp ecx, 10
    jb run_sieve_dense_restore_037
    btr qword [rsi + 72], 34
    cmp ecx, 11
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 80]
    btr rax, 7
    btr rax, 44
    mov qword [rsi + 80], rax
    cmp ecx, 12
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 88]
    btr rax, 17
    btr rax, 54
    mov qword [rsi + 88], rax
    cmp ecx, 13
    jb run_sieve_dense_restore_037
    btr qword [rsi + 96], 27
    cmp ecx, 14
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 104]
    btr rax, 0
    btr rax, 37
    mov qword [rsi + 104], rax
    cmp ecx, 15
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 112]
    btr rax, 10
    btr rax, 47
    mov qword [rsi + 112], rax
    cmp ecx, 16
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 120]
    btr rax, 20
    btr rax, 57
    mov qword [rsi + 120], rax
    cmp ecx, 17
    jb run_sieve_dense_restore_037
    btr qword [rsi + 128], 30
    cmp ecx, 18
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 136]
    btr rax, 3
    btr rax, 40
    mov qword [rsi + 136], rax
    cmp ecx, 19
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 144]
    btr rax, 13
    btr rax, 50
    mov qword [rsi + 144], rax
    cmp ecx, 20
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 152]
    btr rax, 23
    btr rax, 60
    mov qword [rsi + 152], rax
    cmp ecx, 21
    jb run_sieve_dense_restore_037
    btr qword [rsi + 160], 33
    cmp ecx, 22
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 168]
    btr rax, 6
    btr rax, 43
    mov qword [rsi + 168], rax
    cmp ecx, 23
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 176]
    btr rax, 16
    btr rax, 53
    mov qword [rsi + 176], rax
    cmp ecx, 24
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 184]
    btr rax, 26
    btr rax, 63
    mov qword [rsi + 184], rax
    cmp ecx, 25
    jb run_sieve_dense_restore_037
    btr qword [rsi + 192], 36
    cmp ecx, 26
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 200]
    btr rax, 9
    btr rax, 46
    mov qword [rsi + 200], rax
    cmp ecx, 27
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 208]
    btr rax, 19
    btr rax, 56
    mov qword [rsi + 208], rax
    cmp ecx, 28
    jb run_sieve_dense_restore_037
    btr qword [rsi + 216], 29
    cmp ecx, 29
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 224]
    btr rax, 2
    btr rax, 39
    mov qword [rsi + 224], rax
    cmp ecx, 30
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 232]
    btr rax, 12
    btr rax, 49
    mov qword [rsi + 232], rax
    cmp ecx, 31
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 240]
    btr rax, 22
    btr rax, 59
    mov qword [rsi + 240], rax
    cmp ecx, 32
    jb run_sieve_dense_restore_037
    btr qword [rsi + 248], 32
    cmp ecx, 33
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 256]
    btr rax, 5
    btr rax, 42
    mov qword [rsi + 256], rax
    cmp ecx, 34
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 264]
    btr rax, 15
    btr rax, 52
    mov qword [rsi + 264], rax
    cmp ecx, 35
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 272]
    btr rax, 25
    btr rax, 62
    mov qword [rsi + 272], rax
    cmp ecx, 36
    jb run_sieve_dense_restore_037
    btr qword [rsi + 280], 35
    cmp ecx, 37
    jb run_sieve_dense_restore_037
    mov rax, qword [rsi + 288]
    btr rax, 8
    btr rax, 45
    mov qword [rsi + 288], rax
run_sieve_dense_restore_037:
    bts qword [r14], 18
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_039:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 39
    jb run_sieve_dense_tail_039
align 16
run_sieve_dense_loop_039:
    mov rax, qword [rsi]
    btr rax, 19
    btr rax, 58
    mov qword [rsi], rax
    btr qword [rsi + 8], 33
    mov rax, qword [rsi + 16]
    btr rax, 8
    btr rax, 47
    mov qword [rsi + 16], rax
    mov rax, qword [rsi + 24]
    btr rax, 22
    btr rax, 61
    mov qword [rsi + 24], rax
    btr qword [rsi + 32], 36
    mov rax, qword [rsi + 40]
    btr rax, 11
    btr rax, 50
    mov qword [rsi + 40], rax
    btr qword [rsi + 48], 25
    mov rax, qword [rsi + 56]
    btr rax, 0
    btr rax, 39
    mov qword [rsi + 56], rax
    mov rax, qword [rsi + 64]
    btr rax, 14
    btr rax, 53
    mov qword [rsi + 64], rax
    btr qword [rsi + 72], 28
    mov rax, qword [rsi + 80]
    btr rax, 3
    btr rax, 42
    mov qword [rsi + 80], rax
    mov rax, qword [rsi + 88]
    btr rax, 17
    btr rax, 56
    mov qword [rsi + 88], rax
    btr qword [rsi + 96], 31
    mov rax, qword [rsi + 104]
    btr rax, 6
    btr rax, 45
    mov qword [rsi + 104], rax
    mov rax, qword [rsi + 112]
    btr rax, 20
    btr rax, 59
    mov qword [rsi + 112], rax
    btr qword [rsi + 120], 34
    mov rax, qword [rsi + 128]
    btr rax, 9
    btr rax, 48
    mov qword [rsi + 128], rax
    mov rax, qword [rsi + 136]
    btr rax, 23
    btr rax, 62
    mov qword [rsi + 136], rax
    btr qword [rsi + 144], 37
    mov rax, qword [rsi + 152]
    btr rax, 12
    btr rax, 51
    mov qword [rsi + 152], rax
    btr qword [rsi + 160], 26
    mov rax, qword [rsi + 168]
    btr rax, 1
    btr rax, 40
    mov qword [rsi + 168], rax
    mov rax, qword [rsi + 176]
    btr rax, 15
    btr rax, 54
    mov qword [rsi + 176], rax
    btr qword [rsi + 184], 29
    mov rax, qword [rsi + 192]
    btr rax, 4
    btr rax, 43
    mov qword [rsi + 192], rax
    mov rax, qword [rsi + 200]
    btr rax, 18
    btr rax, 57
    mov qword [rsi + 200], rax
    btr qword [rsi + 208], 32
    mov rax, qword [rsi + 216]
    btr rax, 7
    btr rax, 46
    mov qword [rsi + 216], rax
    mov rax, qword [rsi + 224]
    btr rax, 21
    btr rax, 60
    mov qword [rsi + 224], rax
    btr qword [rsi + 232], 35
    mov rax, qword [rsi + 240]
    btr rax, 10
    btr rax, 49
    mov qword [rsi + 240], rax
    mov rax, qword [rsi + 248]
    btr rax, 24
    btr rax, 63
    mov qword [rsi + 248], rax
    btr qword [rsi + 256], 38
    mov rax, qword [rsi + 264]
    btr rax, 13
    btr rax, 52
    mov qword [rsi + 264], rax
    btr qword [rsi + 272], 27
    mov rax, qword [rsi + 280]
    btr rax, 2
    btr rax, 41
    mov qword [rsi + 280], rax
    mov rax, qword [rsi + 288]
    btr rax, 16
    btr rax, 55
    mov qword [rsi + 288], rax
    btr qword [rsi + 296], 30
    mov rax, qword [rsi + 304]
    btr rax, 5
    btr rax, 44
    mov qword [rsi + 304], rax
    add rsi, 312
    sub ecx, 39
    cmp ecx, 39
    jae run_sieve_dense_loop_039
run_sieve_dense_tail_039:
    test ecx, ecx
    jz run_sieve_dense_restore_039
    cmp ecx, 1
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi]
    btr rax, 19
    btr rax, 58
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_039
    btr qword [rsi + 8], 33
    cmp ecx, 3
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 16]
    btr rax, 8
    btr rax, 47
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 24]
    btr rax, 22
    btr rax, 61
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_039
    btr qword [rsi + 32], 36
    cmp ecx, 6
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 40]
    btr rax, 11
    btr rax, 50
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_039
    btr qword [rsi + 48], 25
    cmp ecx, 8
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 56]
    btr rax, 0
    btr rax, 39
    mov qword [rsi + 56], rax
    cmp ecx, 9
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 64]
    btr rax, 14
    btr rax, 53
    mov qword [rsi + 64], rax
    cmp ecx, 10
    jb run_sieve_dense_restore_039
    btr qword [rsi + 72], 28
    cmp ecx, 11
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 80]
    btr rax, 3
    btr rax, 42
    mov qword [rsi + 80], rax
    cmp ecx, 12
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 88]
    btr rax, 17
    btr rax, 56
    mov qword [rsi + 88], rax
    cmp ecx, 13
    jb run_sieve_dense_restore_039
    btr qword [rsi + 96], 31
    cmp ecx, 14
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 104]
    btr rax, 6
    btr rax, 45
    mov qword [rsi + 104], rax
    cmp ecx, 15
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 112]
    btr rax, 20
    btr rax, 59
    mov qword [rsi + 112], rax
    cmp ecx, 16
    jb run_sieve_dense_restore_039
    btr qword [rsi + 120], 34
    cmp ecx, 17
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 128]
    btr rax, 9
    btr rax, 48
    mov qword [rsi + 128], rax
    cmp ecx, 18
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 136]
    btr rax, 23
    btr rax, 62
    mov qword [rsi + 136], rax
    cmp ecx, 19
    jb run_sieve_dense_restore_039
    btr qword [rsi + 144], 37
    cmp ecx, 20
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 152]
    btr rax, 12
    btr rax, 51
    mov qword [rsi + 152], rax
    cmp ecx, 21
    jb run_sieve_dense_restore_039
    btr qword [rsi + 160], 26
    cmp ecx, 22
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 168]
    btr rax, 1
    btr rax, 40
    mov qword [rsi + 168], rax
    cmp ecx, 23
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 176]
    btr rax, 15
    btr rax, 54
    mov qword [rsi + 176], rax
    cmp ecx, 24
    jb run_sieve_dense_restore_039
    btr qword [rsi + 184], 29
    cmp ecx, 25
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 192]
    btr rax, 4
    btr rax, 43
    mov qword [rsi + 192], rax
    cmp ecx, 26
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 200]
    btr rax, 18
    btr rax, 57
    mov qword [rsi + 200], rax
    cmp ecx, 27
    jb run_sieve_dense_restore_039
    btr qword [rsi + 208], 32
    cmp ecx, 28
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 216]
    btr rax, 7
    btr rax, 46
    mov qword [rsi + 216], rax
    cmp ecx, 29
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 224]
    btr rax, 21
    btr rax, 60
    mov qword [rsi + 224], rax
    cmp ecx, 30
    jb run_sieve_dense_restore_039
    btr qword [rsi + 232], 35
    cmp ecx, 31
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 240]
    btr rax, 10
    btr rax, 49
    mov qword [rsi + 240], rax
    cmp ecx, 32
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 248]
    btr rax, 24
    btr rax, 63
    mov qword [rsi + 248], rax
    cmp ecx, 33
    jb run_sieve_dense_restore_039
    btr qword [rsi + 256], 38
    cmp ecx, 34
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 264]
    btr rax, 13
    btr rax, 52
    mov qword [rsi + 264], rax
    cmp ecx, 35
    jb run_sieve_dense_restore_039
    btr qword [rsi + 272], 27
    cmp ecx, 36
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 280]
    btr rax, 2
    btr rax, 41
    mov qword [rsi + 280], rax
    cmp ecx, 37
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 288]
    btr rax, 16
    btr rax, 55
    mov qword [rsi + 288], rax
    cmp ecx, 38
    jb run_sieve_dense_restore_039
    btr qword [rsi + 296], 30
    cmp ecx, 39
    jb run_sieve_dense_restore_039
    mov rax, qword [rsi + 304]
    btr rax, 5
    btr rax, 44
    mov qword [rsi + 304], rax
run_sieve_dense_restore_039:
    bts qword [r14], 19
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_041:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 41
    jb run_sieve_dense_tail_041
align 16
run_sieve_dense_loop_041:
    mov rax, qword [rsi]
    btr rax, 20
    btr rax, 61
    mov qword [rsi], rax
    btr qword [rsi + 8], 38
    mov rax, qword [rsi + 16]
    btr rax, 15
    btr rax, 56
    mov qword [rsi + 16], rax
    btr qword [rsi + 24], 33
    mov rax, qword [rsi + 32]
    btr rax, 10
    btr rax, 51
    mov qword [rsi + 32], rax
    btr qword [rsi + 40], 28
    mov rax, qword [rsi + 48]
    btr rax, 5
    btr rax, 46
    mov qword [rsi + 48], rax
    btr qword [rsi + 56], 23
    mov rax, qword [rsi + 64]
    btr rax, 0
    btr rax, 41
    mov qword [rsi + 64], rax
    mov rax, qword [rsi + 72]
    btr rax, 18
    btr rax, 59
    mov qword [rsi + 72], rax
    btr qword [rsi + 80], 36
    mov rax, qword [rsi + 88]
    btr rax, 13
    btr rax, 54
    mov qword [rsi + 88], rax
    btr qword [rsi + 96], 31
    mov rax, qword [rsi + 104]
    btr rax, 8
    btr rax, 49
    mov qword [rsi + 104], rax
    btr qword [rsi + 112], 26
    mov rax, qword [rsi + 120]
    btr rax, 3
    btr rax, 44
    mov qword [rsi + 120], rax
    mov rax, qword [rsi + 128]
    btr rax, 21
    btr rax, 62
    mov qword [rsi + 128], rax
    btr qword [rsi + 136], 39
    mov rax, qword [rsi + 144]
    btr rax, 16
    btr rax, 57
    mov qword [rsi + 144], rax
    btr qword [rsi + 152], 34
    mov rax, qword [rsi + 160]
    btr rax, 11
    btr rax, 52
    mov qword [rsi + 160], rax
    btr qword [rsi + 168], 29
    mov rax, qword [rsi + 176]
    btr rax, 6
    btr rax, 47
    mov qword [rsi + 176], rax
    btr qword [rsi + 184], 24
    mov rax, qword [rsi + 192]
    btr rax, 1
    btr rax, 42
    mov qword [rsi + 192], rax
    mov rax, qword [rsi + 200]
    btr rax, 19
    btr rax, 60
    mov qword [rsi + 200], rax
    btr qword [rsi + 208], 37
    mov rax, qword [rsi + 216]
    btr rax, 14
    btr rax, 55
    mov qword [rsi + 216], rax
    btr qword [rsi + 224], 32
    mov rax, qword [rsi + 232]
    btr rax, 9
    btr rax, 50
    mov qword [rsi + 232], rax
    btr qword [rsi + 240], 27
    mov rax, qword [rsi + 248]
    btr rax, 4
    btr rax, 45
    mov qword [rsi + 248], rax
    mov rax, qword [rsi + 256]
    btr rax, 22
    btr rax, 63
    mov qword [rsi + 256], rax
    btr qword [rsi + 264], 40
    mov rax, qword [rsi + 272]
    btr rax, 17
    btr rax, 58
    mov qword [rsi + 272], rax
    btr qword [rsi + 280], 35
    mov rax, qword [rsi + 288]
    btr rax, 12
    btr rax, 53
    mov qword [rsi + 288], rax
    btr qword [rsi + 296], 30
    mov rax, qword [rsi + 304]
    btr rax, 7
    btr rax, 48
    mov qword [rsi + 304], rax
    btr qword [rsi + 312], 25
    mov rax, qword [rsi + 320]
    btr rax, 2
    btr rax, 43
    mov qword [rsi + 320], rax
    add rsi, 328
    sub ecx, 41
    cmp ecx, 41
    jae run_sieve_dense_loop_041
run_sieve_dense_tail_041:
    test ecx, ecx
    jz run_sieve_dense_restore_041
    cmp ecx, 1
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi]
    btr rax, 20
    btr rax, 61
    mov qword [rsi], rax
    cmp ecx, 2
    jb run_sieve_dense_restore_041
    btr qword [rsi + 8], 38
    cmp ecx, 3
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 16]
    btr rax, 15
    btr rax, 56
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_041
    btr qword [rsi + 24], 33
    cmp ecx, 5
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 32]
    btr rax, 10
    btr rax, 51
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_041
    btr qword [rsi + 40], 28
    cmp ecx, 7
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 48]
    btr rax, 5
    btr rax, 46
    mov qword [rsi + 48], rax
    cmp ecx, 8
    jb run_sieve_dense_restore_041
    btr qword [rsi + 56], 23
    cmp ecx, 9
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 64]
    btr rax, 0
    btr rax, 41
    mov qword [rsi + 64], rax
    cmp ecx, 10
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 72]
    btr rax, 18
    btr rax, 59
    mov qword [rsi + 72], rax
    cmp ecx, 11
    jb run_sieve_dense_restore_041
    btr qword [rsi + 80], 36
    cmp ecx, 12
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 88]
    btr rax, 13
    btr rax, 54
    mov qword [rsi + 88], rax
    cmp ecx, 13
    jb run_sieve_dense_restore_041
    btr qword [rsi + 96], 31
    cmp ecx, 14
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 104]
    btr rax, 8
    btr rax, 49
    mov qword [rsi + 104], rax
    cmp ecx, 15
    jb run_sieve_dense_restore_041
    btr qword [rsi + 112], 26
    cmp ecx, 16
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 120]
    btr rax, 3
    btr rax, 44
    mov qword [rsi + 120], rax
    cmp ecx, 17
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 128]
    btr rax, 21
    btr rax, 62
    mov qword [rsi + 128], rax
    cmp ecx, 18
    jb run_sieve_dense_restore_041
    btr qword [rsi + 136], 39
    cmp ecx, 19
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 144]
    btr rax, 16
    btr rax, 57
    mov qword [rsi + 144], rax
    cmp ecx, 20
    jb run_sieve_dense_restore_041
    btr qword [rsi + 152], 34
    cmp ecx, 21
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 160]
    btr rax, 11
    btr rax, 52
    mov qword [rsi + 160], rax
    cmp ecx, 22
    jb run_sieve_dense_restore_041
    btr qword [rsi + 168], 29
    cmp ecx, 23
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 176]
    btr rax, 6
    btr rax, 47
    mov qword [rsi + 176], rax
    cmp ecx, 24
    jb run_sieve_dense_restore_041
    btr qword [rsi + 184], 24
    cmp ecx, 25
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 192]
    btr rax, 1
    btr rax, 42
    mov qword [rsi + 192], rax
    cmp ecx, 26
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 200]
    btr rax, 19
    btr rax, 60
    mov qword [rsi + 200], rax
    cmp ecx, 27
    jb run_sieve_dense_restore_041
    btr qword [rsi + 208], 37
    cmp ecx, 28
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 216]
    btr rax, 14
    btr rax, 55
    mov qword [rsi + 216], rax
    cmp ecx, 29
    jb run_sieve_dense_restore_041
    btr qword [rsi + 224], 32
    cmp ecx, 30
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 232]
    btr rax, 9
    btr rax, 50
    mov qword [rsi + 232], rax
    cmp ecx, 31
    jb run_sieve_dense_restore_041
    btr qword [rsi + 240], 27
    cmp ecx, 32
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 248]
    btr rax, 4
    btr rax, 45
    mov qword [rsi + 248], rax
    cmp ecx, 33
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 256]
    btr rax, 22
    btr rax, 63
    mov qword [rsi + 256], rax
    cmp ecx, 34
    jb run_sieve_dense_restore_041
    btr qword [rsi + 264], 40
    cmp ecx, 35
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 272]
    btr rax, 17
    btr rax, 58
    mov qword [rsi + 272], rax
    cmp ecx, 36
    jb run_sieve_dense_restore_041
    btr qword [rsi + 280], 35
    cmp ecx, 37
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 288]
    btr rax, 12
    btr rax, 53
    mov qword [rsi + 288], rax
    cmp ecx, 38
    jb run_sieve_dense_restore_041
    btr qword [rsi + 296], 30
    cmp ecx, 39
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 304]
    btr rax, 7
    btr rax, 48
    mov qword [rsi + 304], rax
    cmp ecx, 40
    jb run_sieve_dense_restore_041
    btr qword [rsi + 312], 25
    cmp ecx, 41
    jb run_sieve_dense_restore_041
    mov rax, qword [rsi + 320]
    btr rax, 2
    btr rax, 43
    mov qword [rsi + 320], rax
run_sieve_dense_restore_041:
    bts qword [r14], 20
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_043:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 43
    jb run_sieve_dense_tail_043
align 16
run_sieve_dense_loop_043:
    btr qword [rsi], 21
    mov rax, qword [rsi + 8]
    btr rax, 0
    btr rax, 43
    mov qword [rsi + 8], rax
    btr qword [rsi + 16], 22
    mov rax, qword [rsi + 24]
    btr rax, 1
    btr rax, 44
    mov qword [rsi + 24], rax
    btr qword [rsi + 32], 23
    mov rax, qword [rsi + 40]
    btr rax, 2
    btr rax, 45
    mov qword [rsi + 40], rax
    btr qword [rsi + 48], 24
    mov rax, qword [rsi + 56]
    btr rax, 3
    btr rax, 46
    mov qword [rsi + 56], rax
    btr qword [rsi + 64], 25
    mov rax, qword [rsi + 72]
    btr rax, 4
    btr rax, 47
    mov qword [rsi + 72], rax
    btr qword [rsi + 80], 26
    mov rax, qword [rsi + 88]
    btr rax, 5
    btr rax, 48
    mov qword [rsi + 88], rax
    btr qword [rsi + 96], 27
    mov rax, qword [rsi + 104]
    btr rax, 6
    btr rax, 49
    mov qword [rsi + 104], rax
    btr qword [rsi + 112], 28
    mov rax, qword [rsi + 120]
    btr rax, 7
    btr rax, 50
    mov qword [rsi + 120], rax
    btr qword [rsi + 128], 29
    mov rax, qword [rsi + 136]
    btr rax, 8
    btr rax, 51
    mov qword [rsi + 136], rax
    btr qword [rsi + 144], 30
    mov rax, qword [rsi + 152]
    btr rax, 9
    btr rax, 52
    mov qword [rsi + 152], rax
    btr qword [rsi + 160], 31
    mov rax, qword [rsi + 168]
    btr rax, 10
    btr rax, 53
    mov qword [rsi + 168], rax
    btr qword [rsi + 176], 32
    mov rax, qword [rsi + 184]
    btr rax, 11
    btr rax, 54
    mov qword [rsi + 184], rax
    btr qword [rsi + 192], 33
    mov rax, qword [rsi + 200]
    btr rax, 12
    btr rax, 55
    mov qword [rsi + 200], rax
    btr qword [rsi + 208], 34
    mov rax, qword [rsi + 216]
    btr rax, 13
    btr rax, 56
    mov qword [rsi + 216], rax
    btr qword [rsi + 224], 35
    mov rax, qword [rsi + 232]
    btr rax, 14
    btr rax, 57
    mov qword [rsi + 232], rax
    btr qword [rsi + 240], 36
    mov rax, qword [rsi + 248]
    btr rax, 15
    btr rax, 58
    mov qword [rsi + 248], rax
    btr qword [rsi + 256], 37
    mov rax, qword [rsi + 264]
    btr rax, 16
    btr rax, 59
    mov qword [rsi + 264], rax
    btr qword [rsi + 272], 38
    mov rax, qword [rsi + 280]
    btr rax, 17
    btr rax, 60
    mov qword [rsi + 280], rax
    btr qword [rsi + 288], 39
    mov rax, qword [rsi + 296]
    btr rax, 18
    btr rax, 61
    mov qword [rsi + 296], rax
    btr qword [rsi + 304], 40
    mov rax, qword [rsi + 312]
    btr rax, 19
    btr rax, 62
    mov qword [rsi + 312], rax
    btr qword [rsi + 320], 41
    mov rax, qword [rsi + 328]
    btr rax, 20
    btr rax, 63
    mov qword [rsi + 328], rax
    btr qword [rsi + 336], 42
    add rsi, 344
    sub ecx, 43
    cmp ecx, 43
    jae run_sieve_dense_loop_043
run_sieve_dense_tail_043:
    test ecx, ecx
    jz run_sieve_dense_restore_043
    cmp ecx, 1
    jb run_sieve_dense_restore_043
    btr qword [rsi], 21
    cmp ecx, 2
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 8]
    btr rax, 0
    btr rax, 43
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_043
    btr qword [rsi + 16], 22
    cmp ecx, 4
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 24]
    btr rax, 1
    btr rax, 44
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_043
    btr qword [rsi + 32], 23
    cmp ecx, 6
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 40]
    btr rax, 2
    btr rax, 45
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_043
    btr qword [rsi + 48], 24
    cmp ecx, 8
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 56]
    btr rax, 3
    btr rax, 46
    mov qword [rsi + 56], rax
    cmp ecx, 9
    jb run_sieve_dense_restore_043
    btr qword [rsi + 64], 25
    cmp ecx, 10
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 72]
    btr rax, 4
    btr rax, 47
    mov qword [rsi + 72], rax
    cmp ecx, 11
    jb run_sieve_dense_restore_043
    btr qword [rsi + 80], 26
    cmp ecx, 12
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 88]
    btr rax, 5
    btr rax, 48
    mov qword [rsi + 88], rax
    cmp ecx, 13
    jb run_sieve_dense_restore_043
    btr qword [rsi + 96], 27
    cmp ecx, 14
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 104]
    btr rax, 6
    btr rax, 49
    mov qword [rsi + 104], rax
    cmp ecx, 15
    jb run_sieve_dense_restore_043
    btr qword [rsi + 112], 28
    cmp ecx, 16
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 120]
    btr rax, 7
    btr rax, 50
    mov qword [rsi + 120], rax
    cmp ecx, 17
    jb run_sieve_dense_restore_043
    btr qword [rsi + 128], 29
    cmp ecx, 18
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 136]
    btr rax, 8
    btr rax, 51
    mov qword [rsi + 136], rax
    cmp ecx, 19
    jb run_sieve_dense_restore_043
    btr qword [rsi + 144], 30
    cmp ecx, 20
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 152]
    btr rax, 9
    btr rax, 52
    mov qword [rsi + 152], rax
    cmp ecx, 21
    jb run_sieve_dense_restore_043
    btr qword [rsi + 160], 31
    cmp ecx, 22
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 168]
    btr rax, 10
    btr rax, 53
    mov qword [rsi + 168], rax
    cmp ecx, 23
    jb run_sieve_dense_restore_043
    btr qword [rsi + 176], 32
    cmp ecx, 24
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 184]
    btr rax, 11
    btr rax, 54
    mov qword [rsi + 184], rax
    cmp ecx, 25
    jb run_sieve_dense_restore_043
    btr qword [rsi + 192], 33
    cmp ecx, 26
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 200]
    btr rax, 12
    btr rax, 55
    mov qword [rsi + 200], rax
    cmp ecx, 27
    jb run_sieve_dense_restore_043
    btr qword [rsi + 208], 34
    cmp ecx, 28
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 216]
    btr rax, 13
    btr rax, 56
    mov qword [rsi + 216], rax
    cmp ecx, 29
    jb run_sieve_dense_restore_043
    btr qword [rsi + 224], 35
    cmp ecx, 30
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 232]
    btr rax, 14
    btr rax, 57
    mov qword [rsi + 232], rax
    cmp ecx, 31
    jb run_sieve_dense_restore_043
    btr qword [rsi + 240], 36
    cmp ecx, 32
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 248]
    btr rax, 15
    btr rax, 58
    mov qword [rsi + 248], rax
    cmp ecx, 33
    jb run_sieve_dense_restore_043
    btr qword [rsi + 256], 37
    cmp ecx, 34
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 264]
    btr rax, 16
    btr rax, 59
    mov qword [rsi + 264], rax
    cmp ecx, 35
    jb run_sieve_dense_restore_043
    btr qword [rsi + 272], 38
    cmp ecx, 36
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 280]
    btr rax, 17
    btr rax, 60
    mov qword [rsi + 280], rax
    cmp ecx, 37
    jb run_sieve_dense_restore_043
    btr qword [rsi + 288], 39
    cmp ecx, 38
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 296]
    btr rax, 18
    btr rax, 61
    mov qword [rsi + 296], rax
    cmp ecx, 39
    jb run_sieve_dense_restore_043
    btr qword [rsi + 304], 40
    cmp ecx, 40
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 312]
    btr rax, 19
    btr rax, 62
    mov qword [rsi + 312], rax
    cmp ecx, 41
    jb run_sieve_dense_restore_043
    btr qword [rsi + 320], 41
    cmp ecx, 42
    jb run_sieve_dense_restore_043
    mov rax, qword [rsi + 328]
    btr rax, 20
    btr rax, 63
    mov qword [rsi + 328], rax
    cmp ecx, 43
    jb run_sieve_dense_restore_043
    btr qword [rsi + 336], 42
run_sieve_dense_restore_043:
    bts qword [r14], 21
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_045:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 45
    jb run_sieve_dense_tail_045
align 16
run_sieve_dense_loop_045:
    btr qword [rsi], 22
    mov rax, qword [rsi + 8]
    btr rax, 3
    btr rax, 48
    mov qword [rsi + 8], rax
    btr qword [rsi + 16], 29
    mov rax, qword [rsi + 24]
    btr rax, 10
    btr rax, 55
    mov qword [rsi + 24], rax
    btr qword [rsi + 32], 36
    mov rax, qword [rsi + 40]
    btr rax, 17
    btr rax, 62
    mov qword [rsi + 40], rax
    btr qword [rsi + 48], 43
    btr qword [rsi + 56], 24
    mov rax, qword [rsi + 64]
    btr rax, 5
    btr rax, 50
    mov qword [rsi + 64], rax
    btr qword [rsi + 72], 31
    mov rax, qword [rsi + 80]
    btr rax, 12
    btr rax, 57
    mov qword [rsi + 80], rax
    btr qword [rsi + 88], 38
    btr qword [rsi + 96], 19
    mov rax, qword [rsi + 104]
    btr rax, 0
    btr rax, 45
    mov qword [rsi + 104], rax
    btr qword [rsi + 112], 26
    mov rax, qword [rsi + 120]
    btr rax, 7
    btr rax, 52
    mov qword [rsi + 120], rax
    btr qword [rsi + 128], 33
    mov rax, qword [rsi + 136]
    btr rax, 14
    btr rax, 59
    mov qword [rsi + 136], rax
    btr qword [rsi + 144], 40
    btr qword [rsi + 152], 21
    mov rax, qword [rsi + 160]
    btr rax, 2
    btr rax, 47
    mov qword [rsi + 160], rax
    btr qword [rsi + 168], 28
    mov rax, qword [rsi + 176]
    btr rax, 9
    btr rax, 54
    mov qword [rsi + 176], rax
    btr qword [rsi + 184], 35
    mov rax, qword [rsi + 192]
    btr rax, 16
    btr rax, 61
    mov qword [rsi + 192], rax
    btr qword [rsi + 200], 42
    btr qword [rsi + 208], 23
    mov rax, qword [rsi + 216]
    btr rax, 4
    btr rax, 49
    mov qword [rsi + 216], rax
    btr qword [rsi + 224], 30
    mov rax, qword [rsi + 232]
    btr rax, 11
    btr rax, 56
    mov qword [rsi + 232], rax
    btr qword [rsi + 240], 37
    mov rax, qword [rsi + 248]
    btr rax, 18
    btr rax, 63
    mov qword [rsi + 248], rax
    btr qword [rsi + 256], 44
    btr qword [rsi + 264], 25
    mov rax, qword [rsi + 272]
    btr rax, 6
    btr rax, 51
    mov qword [rsi + 272], rax
    btr qword [rsi + 280], 32
    mov rax, qword [rsi + 288]
    btr rax, 13
    btr rax, 58
    mov qword [rsi + 288], rax
    btr qword [rsi + 296], 39
    btr qword [rsi + 304], 20
    mov rax, qword [rsi + 312]
    btr rax, 1
    btr rax, 46
    mov qword [rsi + 312], rax
    btr qword [rsi + 320], 27
    mov rax, qword [rsi + 328]
    btr rax, 8
    btr rax, 53
    mov qword [rsi + 328], rax
    btr qword [rsi + 336], 34
    mov rax, qword [rsi + 344]
    btr rax, 15
    btr rax, 60
    mov qword [rsi + 344], rax
    btr qword [rsi + 352], 41
    add rsi, 360
    sub ecx, 45
    cmp ecx, 45
    jae run_sieve_dense_loop_045
run_sieve_dense_tail_045:
    test ecx, ecx
    jz run_sieve_dense_restore_045
    cmp ecx, 1
    jb run_sieve_dense_restore_045
    btr qword [rsi], 22
    cmp ecx, 2
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 8]
    btr rax, 3
    btr rax, 48
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_045
    btr qword [rsi + 16], 29
    cmp ecx, 4
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 24]
    btr rax, 10
    btr rax, 55
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_045
    btr qword [rsi + 32], 36
    cmp ecx, 6
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 40]
    btr rax, 17
    btr rax, 62
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_045
    btr qword [rsi + 48], 43
    cmp ecx, 8
    jb run_sieve_dense_restore_045
    btr qword [rsi + 56], 24
    cmp ecx, 9
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 64]
    btr rax, 5
    btr rax, 50
    mov qword [rsi + 64], rax
    cmp ecx, 10
    jb run_sieve_dense_restore_045
    btr qword [rsi + 72], 31
    cmp ecx, 11
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 80]
    btr rax, 12
    btr rax, 57
    mov qword [rsi + 80], rax
    cmp ecx, 12
    jb run_sieve_dense_restore_045
    btr qword [rsi + 88], 38
    cmp ecx, 13
    jb run_sieve_dense_restore_045
    btr qword [rsi + 96], 19
    cmp ecx, 14
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 104]
    btr rax, 0
    btr rax, 45
    mov qword [rsi + 104], rax
    cmp ecx, 15
    jb run_sieve_dense_restore_045
    btr qword [rsi + 112], 26
    cmp ecx, 16
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 120]
    btr rax, 7
    btr rax, 52
    mov qword [rsi + 120], rax
    cmp ecx, 17
    jb run_sieve_dense_restore_045
    btr qword [rsi + 128], 33
    cmp ecx, 18
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 136]
    btr rax, 14
    btr rax, 59
    mov qword [rsi + 136], rax
    cmp ecx, 19
    jb run_sieve_dense_restore_045
    btr qword [rsi + 144], 40
    cmp ecx, 20
    jb run_sieve_dense_restore_045
    btr qword [rsi + 152], 21
    cmp ecx, 21
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 160]
    btr rax, 2
    btr rax, 47
    mov qword [rsi + 160], rax
    cmp ecx, 22
    jb run_sieve_dense_restore_045
    btr qword [rsi + 168], 28
    cmp ecx, 23
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 176]
    btr rax, 9
    btr rax, 54
    mov qword [rsi + 176], rax
    cmp ecx, 24
    jb run_sieve_dense_restore_045
    btr qword [rsi + 184], 35
    cmp ecx, 25
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 192]
    btr rax, 16
    btr rax, 61
    mov qword [rsi + 192], rax
    cmp ecx, 26
    jb run_sieve_dense_restore_045
    btr qword [rsi + 200], 42
    cmp ecx, 27
    jb run_sieve_dense_restore_045
    btr qword [rsi + 208], 23
    cmp ecx, 28
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 216]
    btr rax, 4
    btr rax, 49
    mov qword [rsi + 216], rax
    cmp ecx, 29
    jb run_sieve_dense_restore_045
    btr qword [rsi + 224], 30
    cmp ecx, 30
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 232]
    btr rax, 11
    btr rax, 56
    mov qword [rsi + 232], rax
    cmp ecx, 31
    jb run_sieve_dense_restore_045
    btr qword [rsi + 240], 37
    cmp ecx, 32
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 248]
    btr rax, 18
    btr rax, 63
    mov qword [rsi + 248], rax
    cmp ecx, 33
    jb run_sieve_dense_restore_045
    btr qword [rsi + 256], 44
    cmp ecx, 34
    jb run_sieve_dense_restore_045
    btr qword [rsi + 264], 25
    cmp ecx, 35
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 272]
    btr rax, 6
    btr rax, 51
    mov qword [rsi + 272], rax
    cmp ecx, 36
    jb run_sieve_dense_restore_045
    btr qword [rsi + 280], 32
    cmp ecx, 37
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 288]
    btr rax, 13
    btr rax, 58
    mov qword [rsi + 288], rax
    cmp ecx, 38
    jb run_sieve_dense_restore_045
    btr qword [rsi + 296], 39
    cmp ecx, 39
    jb run_sieve_dense_restore_045
    btr qword [rsi + 304], 20
    cmp ecx, 40
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 312]
    btr rax, 1
    btr rax, 46
    mov qword [rsi + 312], rax
    cmp ecx, 41
    jb run_sieve_dense_restore_045
    btr qword [rsi + 320], 27
    cmp ecx, 42
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 328]
    btr rax, 8
    btr rax, 53
    mov qword [rsi + 328], rax
    cmp ecx, 43
    jb run_sieve_dense_restore_045
    btr qword [rsi + 336], 34
    cmp ecx, 44
    jb run_sieve_dense_restore_045
    mov rax, qword [rsi + 344]
    btr rax, 15
    btr rax, 60
    mov qword [rsi + 344], rax
    cmp ecx, 45
    jb run_sieve_dense_restore_045
    btr qword [rsi + 352], 41
run_sieve_dense_restore_045:
    bts qword [r14], 22
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_047:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 47
    jb run_sieve_dense_tail_047
align 16
run_sieve_dense_loop_047:
    btr qword [rsi], 23
    mov rax, qword [rsi + 8]
    btr rax, 6
    btr rax, 53
    mov qword [rsi + 8], rax
    btr qword [rsi + 16], 36
    btr qword [rsi + 24], 19
    mov rax, qword [rsi + 32]
    btr rax, 2
    btr rax, 49
    mov qword [rsi + 32], rax
    btr qword [rsi + 40], 32
    mov rax, qword [rsi + 48]
    btr rax, 15
    btr rax, 62
    mov qword [rsi + 48], rax
    btr qword [rsi + 56], 45
    btr qword [rsi + 64], 28
    mov rax, qword [rsi + 72]
    btr rax, 11
    btr rax, 58
    mov qword [rsi + 72], rax
    btr qword [rsi + 80], 41
    btr qword [rsi + 88], 24
    mov rax, qword [rsi + 96]
    btr rax, 7
    btr rax, 54
    mov qword [rsi + 96], rax
    btr qword [rsi + 104], 37
    btr qword [rsi + 112], 20
    mov rax, qword [rsi + 120]
    btr rax, 3
    btr rax, 50
    mov qword [rsi + 120], rax
    btr qword [rsi + 128], 33
    mov rax, qword [rsi + 136]
    btr rax, 16
    btr rax, 63
    mov qword [rsi + 136], rax
    btr qword [rsi + 144], 46
    btr qword [rsi + 152], 29
    mov rax, qword [rsi + 160]
    btr rax, 12
    btr rax, 59
    mov qword [rsi + 160], rax
    btr qword [rsi + 168], 42
    btr qword [rsi + 176], 25
    mov rax, qword [rsi + 184]
    btr rax, 8
    btr rax, 55
    mov qword [rsi + 184], rax
    btr qword [rsi + 192], 38
    btr qword [rsi + 200], 21
    mov rax, qword [rsi + 208]
    btr rax, 4
    btr rax, 51
    mov qword [rsi + 208], rax
    btr qword [rsi + 216], 34
    btr qword [rsi + 224], 17
    mov rax, qword [rsi + 232]
    btr rax, 0
    btr rax, 47
    mov qword [rsi + 232], rax
    btr qword [rsi + 240], 30
    mov rax, qword [rsi + 248]
    btr rax, 13
    btr rax, 60
    mov qword [rsi + 248], rax
    btr qword [rsi + 256], 43
    btr qword [rsi + 264], 26
    mov rax, qword [rsi + 272]
    btr rax, 9
    btr rax, 56
    mov qword [rsi + 272], rax
    btr qword [rsi + 280], 39
    btr qword [rsi + 288], 22
    mov rax, qword [rsi + 296]
    btr rax, 5
    btr rax, 52
    mov qword [rsi + 296], rax
    btr qword [rsi + 304], 35
    btr qword [rsi + 312], 18
    mov rax, qword [rsi + 320]
    btr rax, 1
    btr rax, 48
    mov qword [rsi + 320], rax
    btr qword [rsi + 328], 31
    mov rax, qword [rsi + 336]
    btr rax, 14
    btr rax, 61
    mov qword [rsi + 336], rax
    btr qword [rsi + 344], 44
    btr qword [rsi + 352], 27
    mov rax, qword [rsi + 360]
    btr rax, 10
    btr rax, 57
    mov qword [rsi + 360], rax
    btr qword [rsi + 368], 40
    add rsi, 376
    sub ecx, 47
    cmp ecx, 47
    jae run_sieve_dense_loop_047
run_sieve_dense_tail_047:
    test ecx, ecx
    jz run_sieve_dense_restore_047
    cmp ecx, 1
    jb run_sieve_dense_restore_047
    btr qword [rsi], 23
    cmp ecx, 2
    jb run_sieve_dense_restore_047
    mov rax, qword [rsi + 8]
    btr rax, 6
    btr rax, 53
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_047
    btr qword [rsi + 16], 36
    cmp ecx, 4
    jb run_sieve_dense_restore_047
    btr qword [rsi + 24], 19
    cmp ecx, 5
    jb run_sieve_dense_restore_047
    mov rax, qword [rsi + 32]
    btr rax, 2
    btr rax, 49
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_047
    btr qword [rsi + 40], 32
    cmp ecx, 7
    jb run_sieve_dense_restore_047
    mov rax, qword [rsi + 48]
    btr rax, 15
    btr rax, 62
    mov qword [rsi + 48], rax
    cmp ecx, 8
    jb run_sieve_dense_restore_047
    btr qword [rsi + 56], 45
    cmp ecx, 9
    jb run_sieve_dense_restore_047
    btr qword [rsi + 64], 28
    cmp ecx, 10
    jb run_sieve_dense_restore_047
    mov rax, qword [rsi + 72]
    btr rax, 11
    btr rax, 58
    mov qword [rsi + 72], rax
    cmp ecx, 11
    jb run_sieve_dense_restore_047
    btr qword [rsi + 80], 41
    cmp ecx, 12
    jb run_sieve_dense_restore_047
    btr qword [rsi + 88], 24
    cmp ecx, 13
    jb run_sieve_dense_restore_047
    mov rax, qword [rsi + 96]
    btr rax, 7
    btr rax, 54
    mov qword [rsi + 96], rax
    cmp ecx, 14
    jb run_sieve_dense_restore_047
    btr qword [rsi + 104], 37
    cmp ecx, 15
    jb run_sieve_dense_restore_047
    btr qword [rsi + 112], 20
    cmp ecx, 16
    jb run_sieve_dense_restore_047
    mov rax, qword [rsi + 120]
    btr rax, 3
    btr rax, 50
    mov qword [rsi + 120], rax
    cmp ecx, 17
    jb run_sieve_dense_restore_047
    btr qword [rsi + 128], 33
    cmp ecx, 18
    jb run_sieve_dense_restore_047
    mov rax, qword [rsi + 136]
    btr rax, 16
    btr rax, 63
    mov qword [rsi + 136], rax
    cmp ecx, 19
    jb run_sieve_dense_restore_047
    btr qword [rsi + 144], 46
    cmp ecx, 20
    jb run_sieve_dense_restore_047
    btr qword [rsi + 152], 29
    cmp ecx, 21
    jb run_sieve_dense_restore_047
    mov rax, qword [rsi + 160]
    btr rax, 12
    btr rax, 59
    mov qword [rsi + 160], rax
    cmp ecx, 22
    jb run_sieve_dense_restore_047
    btr qword [rsi + 168], 42
    cmp ecx, 23
    jb run_sieve_dense_restore_047
    btr qword [rsi + 176], 25
    cmp ecx, 24
    jb run_sieve_dense_restore_047
    mov rax, qword [rsi + 184]
    btr rax, 8
    btr rax, 55
    mov qword [rsi + 184], rax
    cmp ecx, 25
    jb run_sieve_dense_restore_047
    btr qword [rsi + 192], 38
    cmp ecx, 26
    jb run_sieve_dense_restore_047
    btr qword [rsi + 200], 21
    cmp ecx, 27
    jb run_sieve_dense_restore_047
    mov rax, qword [rsi + 208]
    btr rax, 4
    btr rax, 51
    mov qword [rsi + 208], rax
    cmp ecx, 28
    jb run_sieve_dense_restore_047
    btr qword [rsi + 216], 34
    cmp ecx, 29
    jb run_sieve_dense_restore_047
    btr qword [rsi + 224], 17
    cmp ecx, 30
    jb run_sieve_dense_restore_047
    mov rax, qword [rsi + 232]
    btr rax, 0
    btr rax, 47
    mov qword [rsi + 232], rax
    cmp ecx, 31
    jb run_sieve_dense_restore_047
    btr qword [rsi + 240], 30
    cmp ecx, 32
    jb run_sieve_dense_restore_047
    mov rax, qword [rsi + 248]
    btr rax, 13
    btr rax, 60
    mov qword [rsi + 248], rax
    cmp ecx, 33
    jb run_sieve_dense_restore_047
    btr qword [rsi + 256], 43
    cmp ecx, 34
    jb run_sieve_dense_restore_047
    btr qword [rsi + 264], 26
    cmp ecx, 35
    jb run_sieve_dense_restore_047
    mov rax, qword [rsi + 272]
    btr rax, 9
    btr rax, 56
    mov qword [rsi + 272], rax
    cmp ecx, 36
    jb run_sieve_dense_restore_047
    btr qword [rsi + 280], 39
    cmp ecx, 37
    jb run_sieve_dense_restore_047
    btr qword [rsi + 288], 22
    cmp ecx, 38
    jb run_sieve_dense_restore_047
    mov rax, qword [rsi + 296]
    btr rax, 5
    btr rax, 52
    mov qword [rsi + 296], rax
    cmp ecx, 39
    jb run_sieve_dense_restore_047
    btr qword [rsi + 304], 35
    cmp ecx, 40
    jb run_sieve_dense_restore_047
    btr qword [rsi + 312], 18
    cmp ecx, 41
    jb run_sieve_dense_restore_047
    mov rax, qword [rsi + 320]
    btr rax, 1
    btr rax, 48
    mov qword [rsi + 320], rax
    cmp ecx, 42
    jb run_sieve_dense_restore_047
    btr qword [rsi + 328], 31
    cmp ecx, 43
    jb run_sieve_dense_restore_047
    mov rax, qword [rsi + 336]
    btr rax, 14
    btr rax, 61
    mov qword [rsi + 336], rax
    cmp ecx, 44
    jb run_sieve_dense_restore_047
    btr qword [rsi + 344], 44
    cmp ecx, 45
    jb run_sieve_dense_restore_047
    btr qword [rsi + 352], 27
    cmp ecx, 46
    jb run_sieve_dense_restore_047
    mov rax, qword [rsi + 360]
    btr rax, 10
    btr rax, 57
    mov qword [rsi + 360], rax
    cmp ecx, 47
    jb run_sieve_dense_restore_047
    btr qword [rsi + 368], 40
run_sieve_dense_restore_047:
    bts qword [r14], 23
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_049:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 49
    jb run_sieve_dense_tail_049
align 16
run_sieve_dense_loop_049:
    btr qword [rsi], 24
    mov rax, qword [rsi + 8]
    btr rax, 9
    btr rax, 58
    mov qword [rsi + 8], rax
    btr qword [rsi + 16], 43
    btr qword [rsi + 24], 28
    mov rax, qword [rsi + 32]
    btr rax, 13
    btr rax, 62
    mov qword [rsi + 32], rax
    btr qword [rsi + 40], 47
    btr qword [rsi + 48], 32
    btr qword [rsi + 56], 17
    mov rax, qword [rsi + 64]
    btr rax, 2
    btr rax, 51
    mov qword [rsi + 64], rax
    btr qword [rsi + 72], 36
    btr qword [rsi + 80], 21
    mov rax, qword [rsi + 88]
    btr rax, 6
    btr rax, 55
    mov qword [rsi + 88], rax
    btr qword [rsi + 96], 40
    btr qword [rsi + 104], 25
    mov rax, qword [rsi + 112]
    btr rax, 10
    btr rax, 59
    mov qword [rsi + 112], rax
    btr qword [rsi + 120], 44
    btr qword [rsi + 128], 29
    mov rax, qword [rsi + 136]
    btr rax, 14
    btr rax, 63
    mov qword [rsi + 136], rax
    btr qword [rsi + 144], 48
    btr qword [rsi + 152], 33
    btr qword [rsi + 160], 18
    mov rax, qword [rsi + 168]
    btr rax, 3
    btr rax, 52
    mov qword [rsi + 168], rax
    btr qword [rsi + 176], 37
    btr qword [rsi + 184], 22
    mov rax, qword [rsi + 192]
    btr rax, 7
    btr rax, 56
    mov qword [rsi + 192], rax
    btr qword [rsi + 200], 41
    btr qword [rsi + 208], 26
    mov rax, qword [rsi + 216]
    btr rax, 11
    btr rax, 60
    mov qword [rsi + 216], rax
    btr qword [rsi + 224], 45
    btr qword [rsi + 232], 30
    btr qword [rsi + 240], 15
    mov rax, qword [rsi + 248]
    btr rax, 0
    btr rax, 49
    mov qword [rsi + 248], rax
    btr qword [rsi + 256], 34
    btr qword [rsi + 264], 19
    mov rax, qword [rsi + 272]
    btr rax, 4
    btr rax, 53
    mov qword [rsi + 272], rax
    btr qword [rsi + 280], 38
    btr qword [rsi + 288], 23
    mov rax, qword [rsi + 296]
    btr rax, 8
    btr rax, 57
    mov qword [rsi + 296], rax
    btr qword [rsi + 304], 42
    btr qword [rsi + 312], 27
    mov rax, qword [rsi + 320]
    btr rax, 12
    btr rax, 61
    mov qword [rsi + 320], rax
    btr qword [rsi + 328], 46
    btr qword [rsi + 336], 31
    btr qword [rsi + 344], 16
    mov rax, qword [rsi + 352]
    btr rax, 1
    btr rax, 50
    mov qword [rsi + 352], rax
    btr qword [rsi + 360], 35
    btr qword [rsi + 368], 20
    mov rax, qword [rsi + 376]
    btr rax, 5
    btr rax, 54
    mov qword [rsi + 376], rax
    btr qword [rsi + 384], 39
    add rsi, 392
    sub ecx, 49
    cmp ecx, 49
    jae run_sieve_dense_loop_049
run_sieve_dense_tail_049:
    test ecx, ecx
    jz run_sieve_dense_restore_049
    cmp ecx, 1
    jb run_sieve_dense_restore_049
    btr qword [rsi], 24
    cmp ecx, 2
    jb run_sieve_dense_restore_049
    mov rax, qword [rsi + 8]
    btr rax, 9
    btr rax, 58
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_049
    btr qword [rsi + 16], 43
    cmp ecx, 4
    jb run_sieve_dense_restore_049
    btr qword [rsi + 24], 28
    cmp ecx, 5
    jb run_sieve_dense_restore_049
    mov rax, qword [rsi + 32]
    btr rax, 13
    btr rax, 62
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_049
    btr qword [rsi + 40], 47
    cmp ecx, 7
    jb run_sieve_dense_restore_049
    btr qword [rsi + 48], 32
    cmp ecx, 8
    jb run_sieve_dense_restore_049
    btr qword [rsi + 56], 17
    cmp ecx, 9
    jb run_sieve_dense_restore_049
    mov rax, qword [rsi + 64]
    btr rax, 2
    btr rax, 51
    mov qword [rsi + 64], rax
    cmp ecx, 10
    jb run_sieve_dense_restore_049
    btr qword [rsi + 72], 36
    cmp ecx, 11
    jb run_sieve_dense_restore_049
    btr qword [rsi + 80], 21
    cmp ecx, 12
    jb run_sieve_dense_restore_049
    mov rax, qword [rsi + 88]
    btr rax, 6
    btr rax, 55
    mov qword [rsi + 88], rax
    cmp ecx, 13
    jb run_sieve_dense_restore_049
    btr qword [rsi + 96], 40
    cmp ecx, 14
    jb run_sieve_dense_restore_049
    btr qword [rsi + 104], 25
    cmp ecx, 15
    jb run_sieve_dense_restore_049
    mov rax, qword [rsi + 112]
    btr rax, 10
    btr rax, 59
    mov qword [rsi + 112], rax
    cmp ecx, 16
    jb run_sieve_dense_restore_049
    btr qword [rsi + 120], 44
    cmp ecx, 17
    jb run_sieve_dense_restore_049
    btr qword [rsi + 128], 29
    cmp ecx, 18
    jb run_sieve_dense_restore_049
    mov rax, qword [rsi + 136]
    btr rax, 14
    btr rax, 63
    mov qword [rsi + 136], rax
    cmp ecx, 19
    jb run_sieve_dense_restore_049
    btr qword [rsi + 144], 48
    cmp ecx, 20
    jb run_sieve_dense_restore_049
    btr qword [rsi + 152], 33
    cmp ecx, 21
    jb run_sieve_dense_restore_049
    btr qword [rsi + 160], 18
    cmp ecx, 22
    jb run_sieve_dense_restore_049
    mov rax, qword [rsi + 168]
    btr rax, 3
    btr rax, 52
    mov qword [rsi + 168], rax
    cmp ecx, 23
    jb run_sieve_dense_restore_049
    btr qword [rsi + 176], 37
    cmp ecx, 24
    jb run_sieve_dense_restore_049
    btr qword [rsi + 184], 22
    cmp ecx, 25
    jb run_sieve_dense_restore_049
    mov rax, qword [rsi + 192]
    btr rax, 7
    btr rax, 56
    mov qword [rsi + 192], rax
    cmp ecx, 26
    jb run_sieve_dense_restore_049
    btr qword [rsi + 200], 41
    cmp ecx, 27
    jb run_sieve_dense_restore_049
    btr qword [rsi + 208], 26
    cmp ecx, 28
    jb run_sieve_dense_restore_049
    mov rax, qword [rsi + 216]
    btr rax, 11
    btr rax, 60
    mov qword [rsi + 216], rax
    cmp ecx, 29
    jb run_sieve_dense_restore_049
    btr qword [rsi + 224], 45
    cmp ecx, 30
    jb run_sieve_dense_restore_049
    btr qword [rsi + 232], 30
    cmp ecx, 31
    jb run_sieve_dense_restore_049
    btr qword [rsi + 240], 15
    cmp ecx, 32
    jb run_sieve_dense_restore_049
    mov rax, qword [rsi + 248]
    btr rax, 0
    btr rax, 49
    mov qword [rsi + 248], rax
    cmp ecx, 33
    jb run_sieve_dense_restore_049
    btr qword [rsi + 256], 34
    cmp ecx, 34
    jb run_sieve_dense_restore_049
    btr qword [rsi + 264], 19
    cmp ecx, 35
    jb run_sieve_dense_restore_049
    mov rax, qword [rsi + 272]
    btr rax, 4
    btr rax, 53
    mov qword [rsi + 272], rax
    cmp ecx, 36
    jb run_sieve_dense_restore_049
    btr qword [rsi + 280], 38
    cmp ecx, 37
    jb run_sieve_dense_restore_049
    btr qword [rsi + 288], 23
    cmp ecx, 38
    jb run_sieve_dense_restore_049
    mov rax, qword [rsi + 296]
    btr rax, 8
    btr rax, 57
    mov qword [rsi + 296], rax
    cmp ecx, 39
    jb run_sieve_dense_restore_049
    btr qword [rsi + 304], 42
    cmp ecx, 40
    jb run_sieve_dense_restore_049
    btr qword [rsi + 312], 27
    cmp ecx, 41
    jb run_sieve_dense_restore_049
    mov rax, qword [rsi + 320]
    btr rax, 12
    btr rax, 61
    mov qword [rsi + 320], rax
    cmp ecx, 42
    jb run_sieve_dense_restore_049
    btr qword [rsi + 328], 46
    cmp ecx, 43
    jb run_sieve_dense_restore_049
    btr qword [rsi + 336], 31
    cmp ecx, 44
    jb run_sieve_dense_restore_049
    btr qword [rsi + 344], 16
    cmp ecx, 45
    jb run_sieve_dense_restore_049
    mov rax, qword [rsi + 352]
    btr rax, 1
    btr rax, 50
    mov qword [rsi + 352], rax
    cmp ecx, 46
    jb run_sieve_dense_restore_049
    btr qword [rsi + 360], 35
    cmp ecx, 47
    jb run_sieve_dense_restore_049
    btr qword [rsi + 368], 20
    cmp ecx, 48
    jb run_sieve_dense_restore_049
    mov rax, qword [rsi + 376]
    btr rax, 5
    btr rax, 54
    mov qword [rsi + 376], rax
    cmp ecx, 49
    jb run_sieve_dense_restore_049
    btr qword [rsi + 384], 39
run_sieve_dense_restore_049:
    bts qword [r14], 24
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_051:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 51
    jb run_sieve_dense_tail_051
align 16
run_sieve_dense_loop_051:
    btr qword [rsi], 25
    mov rax, qword [rsi + 8]
    btr rax, 12
    btr rax, 63
    mov qword [rsi + 8], rax
    btr qword [rsi + 16], 50
    btr qword [rsi + 24], 37
    btr qword [rsi + 32], 24
    mov rax, qword [rsi + 40]
    btr rax, 11
    btr rax, 62
    mov qword [rsi + 40], rax
    btr qword [rsi + 48], 49
    btr qword [rsi + 56], 36
    btr qword [rsi + 64], 23
    mov rax, qword [rsi + 72]
    btr rax, 10
    btr rax, 61
    mov qword [rsi + 72], rax
    btr qword [rsi + 80], 48
    btr qword [rsi + 88], 35
    btr qword [rsi + 96], 22
    mov rax, qword [rsi + 104]
    btr rax, 9
    btr rax, 60
    mov qword [rsi + 104], rax
    btr qword [rsi + 112], 47
    btr qword [rsi + 120], 34
    btr qword [rsi + 128], 21
    mov rax, qword [rsi + 136]
    btr rax, 8
    btr rax, 59
    mov qword [rsi + 136], rax
    btr qword [rsi + 144], 46
    btr qword [rsi + 152], 33
    btr qword [rsi + 160], 20
    mov rax, qword [rsi + 168]
    btr rax, 7
    btr rax, 58
    mov qword [rsi + 168], rax
    btr qword [rsi + 176], 45
    btr qword [rsi + 184], 32
    btr qword [rsi + 192], 19
    mov rax, qword [rsi + 200]
    btr rax, 6
    btr rax, 57
    mov qword [rsi + 200], rax
    btr qword [rsi + 208], 44
    btr qword [rsi + 216], 31
    btr qword [rsi + 224], 18
    mov rax, qword [rsi + 232]
    btr rax, 5
    btr rax, 56
    mov qword [rsi + 232], rax
    btr qword [rsi + 240], 43
    btr qword [rsi + 248], 30
    btr qword [rsi + 256], 17
    mov rax, qword [rsi + 264]
    btr rax, 4
    btr rax, 55
    mov qword [rsi + 264], rax
    btr qword [rsi + 272], 42
    btr qword [rsi + 280], 29
    btr qword [rsi + 288], 16
    mov rax, qword [rsi + 296]
    btr rax, 3
    btr rax, 54
    mov qword [rsi + 296], rax
    btr qword [rsi + 304], 41
    btr qword [rsi + 312], 28
    btr qword [rsi + 320], 15
    mov rax, qword [rsi + 328]
    btr rax, 2
    btr rax, 53
    mov qword [rsi + 328], rax
    btr qword [rsi + 336], 40
    btr qword [rsi + 344], 27
    btr qword [rsi + 352], 14
    mov rax, qword [rsi + 360]
    btr rax, 1
    btr rax, 52
    mov qword [rsi + 360], rax
    btr qword [rsi + 368], 39
    btr qword [rsi + 376], 26
    btr qword [rsi + 384], 13
    mov rax, qword [rsi + 392]
    btr rax, 0
    btr rax, 51
    mov qword [rsi + 392], rax
    btr qword [rsi + 400], 38
    add rsi, 408
    sub ecx, 51
    cmp ecx, 51
    jae run_sieve_dense_loop_051
run_sieve_dense_tail_051:
    test ecx, ecx
    jz run_sieve_dense_restore_051
    cmp ecx, 1
    jb run_sieve_dense_restore_051
    btr qword [rsi], 25
    cmp ecx, 2
    jb run_sieve_dense_restore_051
    mov rax, qword [rsi + 8]
    btr rax, 12
    btr rax, 63
    mov qword [rsi + 8], rax
    cmp ecx, 3
    jb run_sieve_dense_restore_051
    btr qword [rsi + 16], 50
    cmp ecx, 4
    jb run_sieve_dense_restore_051
    btr qword [rsi + 24], 37
    cmp ecx, 5
    jb run_sieve_dense_restore_051
    btr qword [rsi + 32], 24
    cmp ecx, 6
    jb run_sieve_dense_restore_051
    mov rax, qword [rsi + 40]
    btr rax, 11
    btr rax, 62
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_051
    btr qword [rsi + 48], 49
    cmp ecx, 8
    jb run_sieve_dense_restore_051
    btr qword [rsi + 56], 36
    cmp ecx, 9
    jb run_sieve_dense_restore_051
    btr qword [rsi + 64], 23
    cmp ecx, 10
    jb run_sieve_dense_restore_051
    mov rax, qword [rsi + 72]
    btr rax, 10
    btr rax, 61
    mov qword [rsi + 72], rax
    cmp ecx, 11
    jb run_sieve_dense_restore_051
    btr qword [rsi + 80], 48
    cmp ecx, 12
    jb run_sieve_dense_restore_051
    btr qword [rsi + 88], 35
    cmp ecx, 13
    jb run_sieve_dense_restore_051
    btr qword [rsi + 96], 22
    cmp ecx, 14
    jb run_sieve_dense_restore_051
    mov rax, qword [rsi + 104]
    btr rax, 9
    btr rax, 60
    mov qword [rsi + 104], rax
    cmp ecx, 15
    jb run_sieve_dense_restore_051
    btr qword [rsi + 112], 47
    cmp ecx, 16
    jb run_sieve_dense_restore_051
    btr qword [rsi + 120], 34
    cmp ecx, 17
    jb run_sieve_dense_restore_051
    btr qword [rsi + 128], 21
    cmp ecx, 18
    jb run_sieve_dense_restore_051
    mov rax, qword [rsi + 136]
    btr rax, 8
    btr rax, 59
    mov qword [rsi + 136], rax
    cmp ecx, 19
    jb run_sieve_dense_restore_051
    btr qword [rsi + 144], 46
    cmp ecx, 20
    jb run_sieve_dense_restore_051
    btr qword [rsi + 152], 33
    cmp ecx, 21
    jb run_sieve_dense_restore_051
    btr qword [rsi + 160], 20
    cmp ecx, 22
    jb run_sieve_dense_restore_051
    mov rax, qword [rsi + 168]
    btr rax, 7
    btr rax, 58
    mov qword [rsi + 168], rax
    cmp ecx, 23
    jb run_sieve_dense_restore_051
    btr qword [rsi + 176], 45
    cmp ecx, 24
    jb run_sieve_dense_restore_051
    btr qword [rsi + 184], 32
    cmp ecx, 25
    jb run_sieve_dense_restore_051
    btr qword [rsi + 192], 19
    cmp ecx, 26
    jb run_sieve_dense_restore_051
    mov rax, qword [rsi + 200]
    btr rax, 6
    btr rax, 57
    mov qword [rsi + 200], rax
    cmp ecx, 27
    jb run_sieve_dense_restore_051
    btr qword [rsi + 208], 44
    cmp ecx, 28
    jb run_sieve_dense_restore_051
    btr qword [rsi + 216], 31
    cmp ecx, 29
    jb run_sieve_dense_restore_051
    btr qword [rsi + 224], 18
    cmp ecx, 30
    jb run_sieve_dense_restore_051
    mov rax, qword [rsi + 232]
    btr rax, 5
    btr rax, 56
    mov qword [rsi + 232], rax
    cmp ecx, 31
    jb run_sieve_dense_restore_051
    btr qword [rsi + 240], 43
    cmp ecx, 32
    jb run_sieve_dense_restore_051
    btr qword [rsi + 248], 30
    cmp ecx, 33
    jb run_sieve_dense_restore_051
    btr qword [rsi + 256], 17
    cmp ecx, 34
    jb run_sieve_dense_restore_051
    mov rax, qword [rsi + 264]
    btr rax, 4
    btr rax, 55
    mov qword [rsi + 264], rax
    cmp ecx, 35
    jb run_sieve_dense_restore_051
    btr qword [rsi + 272], 42
    cmp ecx, 36
    jb run_sieve_dense_restore_051
    btr qword [rsi + 280], 29
    cmp ecx, 37
    jb run_sieve_dense_restore_051
    btr qword [rsi + 288], 16
    cmp ecx, 38
    jb run_sieve_dense_restore_051
    mov rax, qword [rsi + 296]
    btr rax, 3
    btr rax, 54
    mov qword [rsi + 296], rax
    cmp ecx, 39
    jb run_sieve_dense_restore_051
    btr qword [rsi + 304], 41
    cmp ecx, 40
    jb run_sieve_dense_restore_051
    btr qword [rsi + 312], 28
    cmp ecx, 41
    jb run_sieve_dense_restore_051
    btr qword [rsi + 320], 15
    cmp ecx, 42
    jb run_sieve_dense_restore_051
    mov rax, qword [rsi + 328]
    btr rax, 2
    btr rax, 53
    mov qword [rsi + 328], rax
    cmp ecx, 43
    jb run_sieve_dense_restore_051
    btr qword [rsi + 336], 40
    cmp ecx, 44
    jb run_sieve_dense_restore_051
    btr qword [rsi + 344], 27
    cmp ecx, 45
    jb run_sieve_dense_restore_051
    btr qword [rsi + 352], 14
    cmp ecx, 46
    jb run_sieve_dense_restore_051
    mov rax, qword [rsi + 360]
    btr rax, 1
    btr rax, 52
    mov qword [rsi + 360], rax
    cmp ecx, 47
    jb run_sieve_dense_restore_051
    btr qword [rsi + 368], 39
    cmp ecx, 48
    jb run_sieve_dense_restore_051
    btr qword [rsi + 376], 26
    cmp ecx, 49
    jb run_sieve_dense_restore_051
    btr qword [rsi + 384], 13
    cmp ecx, 50
    jb run_sieve_dense_restore_051
    mov rax, qword [rsi + 392]
    btr rax, 0
    btr rax, 51
    mov qword [rsi + 392], rax
    cmp ecx, 51
    jb run_sieve_dense_restore_051
    btr qword [rsi + 400], 38
run_sieve_dense_restore_051:
    bts qword [r14], 25
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_053:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 53
    jb run_sieve_dense_tail_053
align 16
run_sieve_dense_loop_053:
    btr qword [rsi], 26
    btr qword [rsi + 8], 15
    mov rax, qword [rsi + 16]
    btr rax, 4
    btr rax, 57
    mov qword [rsi + 16], rax
    btr qword [rsi + 24], 46
    btr qword [rsi + 32], 35
    btr qword [rsi + 40], 24
    btr qword [rsi + 48], 13
    mov rax, qword [rsi + 56]
    btr rax, 2
    btr rax, 55
    mov qword [rsi + 56], rax
    btr qword [rsi + 64], 44
    btr qword [rsi + 72], 33
    btr qword [rsi + 80], 22
    btr qword [rsi + 88], 11
    mov rax, qword [rsi + 96]
    btr rax, 0
    btr rax, 53
    mov qword [rsi + 96], rax
    btr qword [rsi + 104], 42
    btr qword [rsi + 112], 31
    btr qword [rsi + 120], 20
    mov rax, qword [rsi + 128]
    btr rax, 9
    btr rax, 62
    mov qword [rsi + 128], rax
    btr qword [rsi + 136], 51
    btr qword [rsi + 144], 40
    btr qword [rsi + 152], 29
    btr qword [rsi + 160], 18
    mov rax, qword [rsi + 168]
    btr rax, 7
    btr rax, 60
    mov qword [rsi + 168], rax
    btr qword [rsi + 176], 49
    btr qword [rsi + 184], 38
    btr qword [rsi + 192], 27
    btr qword [rsi + 200], 16
    mov rax, qword [rsi + 208]
    btr rax, 5
    btr rax, 58
    mov qword [rsi + 208], rax
    btr qword [rsi + 216], 47
    btr qword [rsi + 224], 36
    btr qword [rsi + 232], 25
    btr qword [rsi + 240], 14
    mov rax, qword [rsi + 248]
    btr rax, 3
    btr rax, 56
    mov qword [rsi + 248], rax
    btr qword [rsi + 256], 45
    btr qword [rsi + 264], 34
    btr qword [rsi + 272], 23
    btr qword [rsi + 280], 12
    mov rax, qword [rsi + 288]
    btr rax, 1
    btr rax, 54
    mov qword [rsi + 288], rax
    btr qword [rsi + 296], 43
    btr qword [rsi + 304], 32
    btr qword [rsi + 312], 21
    mov rax, qword [rsi + 320]
    btr rax, 10
    btr rax, 63
    mov qword [rsi + 320], rax
    btr qword [rsi + 328], 52
    btr qword [rsi + 336], 41
    btr qword [rsi + 344], 30
    btr qword [rsi + 352], 19
    mov rax, qword [rsi + 360]
    btr rax, 8
    btr rax, 61
    mov qword [rsi + 360], rax
    btr qword [rsi + 368], 50
    btr qword [rsi + 376], 39
    btr qword [rsi + 384], 28
    btr qword [rsi + 392], 17
    mov rax, qword [rsi + 400]
    btr rax, 6
    btr rax, 59
    mov qword [rsi + 400], rax
    btr qword [rsi + 408], 48
    btr qword [rsi + 416], 37
    add rsi, 424
    sub ecx, 53
    cmp ecx, 53
    jae run_sieve_dense_loop_053
run_sieve_dense_tail_053:
    test ecx, ecx
    jz run_sieve_dense_restore_053
    cmp ecx, 1
    jb run_sieve_dense_restore_053
    btr qword [rsi], 26
    cmp ecx, 2
    jb run_sieve_dense_restore_053
    btr qword [rsi + 8], 15
    cmp ecx, 3
    jb run_sieve_dense_restore_053
    mov rax, qword [rsi + 16]
    btr rax, 4
    btr rax, 57
    mov qword [rsi + 16], rax
    cmp ecx, 4
    jb run_sieve_dense_restore_053
    btr qword [rsi + 24], 46
    cmp ecx, 5
    jb run_sieve_dense_restore_053
    btr qword [rsi + 32], 35
    cmp ecx, 6
    jb run_sieve_dense_restore_053
    btr qword [rsi + 40], 24
    cmp ecx, 7
    jb run_sieve_dense_restore_053
    btr qword [rsi + 48], 13
    cmp ecx, 8
    jb run_sieve_dense_restore_053
    mov rax, qword [rsi + 56]
    btr rax, 2
    btr rax, 55
    mov qword [rsi + 56], rax
    cmp ecx, 9
    jb run_sieve_dense_restore_053
    btr qword [rsi + 64], 44
    cmp ecx, 10
    jb run_sieve_dense_restore_053
    btr qword [rsi + 72], 33
    cmp ecx, 11
    jb run_sieve_dense_restore_053
    btr qword [rsi + 80], 22
    cmp ecx, 12
    jb run_sieve_dense_restore_053
    btr qword [rsi + 88], 11
    cmp ecx, 13
    jb run_sieve_dense_restore_053
    mov rax, qword [rsi + 96]
    btr rax, 0
    btr rax, 53
    mov qword [rsi + 96], rax
    cmp ecx, 14
    jb run_sieve_dense_restore_053
    btr qword [rsi + 104], 42
    cmp ecx, 15
    jb run_sieve_dense_restore_053
    btr qword [rsi + 112], 31
    cmp ecx, 16
    jb run_sieve_dense_restore_053
    btr qword [rsi + 120], 20
    cmp ecx, 17
    jb run_sieve_dense_restore_053
    mov rax, qword [rsi + 128]
    btr rax, 9
    btr rax, 62
    mov qword [rsi + 128], rax
    cmp ecx, 18
    jb run_sieve_dense_restore_053
    btr qword [rsi + 136], 51
    cmp ecx, 19
    jb run_sieve_dense_restore_053
    btr qword [rsi + 144], 40
    cmp ecx, 20
    jb run_sieve_dense_restore_053
    btr qword [rsi + 152], 29
    cmp ecx, 21
    jb run_sieve_dense_restore_053
    btr qword [rsi + 160], 18
    cmp ecx, 22
    jb run_sieve_dense_restore_053
    mov rax, qword [rsi + 168]
    btr rax, 7
    btr rax, 60
    mov qword [rsi + 168], rax
    cmp ecx, 23
    jb run_sieve_dense_restore_053
    btr qword [rsi + 176], 49
    cmp ecx, 24
    jb run_sieve_dense_restore_053
    btr qword [rsi + 184], 38
    cmp ecx, 25
    jb run_sieve_dense_restore_053
    btr qword [rsi + 192], 27
    cmp ecx, 26
    jb run_sieve_dense_restore_053
    btr qword [rsi + 200], 16
    cmp ecx, 27
    jb run_sieve_dense_restore_053
    mov rax, qword [rsi + 208]
    btr rax, 5
    btr rax, 58
    mov qword [rsi + 208], rax
    cmp ecx, 28
    jb run_sieve_dense_restore_053
    btr qword [rsi + 216], 47
    cmp ecx, 29
    jb run_sieve_dense_restore_053
    btr qword [rsi + 224], 36
    cmp ecx, 30
    jb run_sieve_dense_restore_053
    btr qword [rsi + 232], 25
    cmp ecx, 31
    jb run_sieve_dense_restore_053
    btr qword [rsi + 240], 14
    cmp ecx, 32
    jb run_sieve_dense_restore_053
    mov rax, qword [rsi + 248]
    btr rax, 3
    btr rax, 56
    mov qword [rsi + 248], rax
    cmp ecx, 33
    jb run_sieve_dense_restore_053
    btr qword [rsi + 256], 45
    cmp ecx, 34
    jb run_sieve_dense_restore_053
    btr qword [rsi + 264], 34
    cmp ecx, 35
    jb run_sieve_dense_restore_053
    btr qword [rsi + 272], 23
    cmp ecx, 36
    jb run_sieve_dense_restore_053
    btr qword [rsi + 280], 12
    cmp ecx, 37
    jb run_sieve_dense_restore_053
    mov rax, qword [rsi + 288]
    btr rax, 1
    btr rax, 54
    mov qword [rsi + 288], rax
    cmp ecx, 38
    jb run_sieve_dense_restore_053
    btr qword [rsi + 296], 43
    cmp ecx, 39
    jb run_sieve_dense_restore_053
    btr qword [rsi + 304], 32
    cmp ecx, 40
    jb run_sieve_dense_restore_053
    btr qword [rsi + 312], 21
    cmp ecx, 41
    jb run_sieve_dense_restore_053
    mov rax, qword [rsi + 320]
    btr rax, 10
    btr rax, 63
    mov qword [rsi + 320], rax
    cmp ecx, 42
    jb run_sieve_dense_restore_053
    btr qword [rsi + 328], 52
    cmp ecx, 43
    jb run_sieve_dense_restore_053
    btr qword [rsi + 336], 41
    cmp ecx, 44
    jb run_sieve_dense_restore_053
    btr qword [rsi + 344], 30
    cmp ecx, 45
    jb run_sieve_dense_restore_053
    btr qword [rsi + 352], 19
    cmp ecx, 46
    jb run_sieve_dense_restore_053
    mov rax, qword [rsi + 360]
    btr rax, 8
    btr rax, 61
    mov qword [rsi + 360], rax
    cmp ecx, 47
    jb run_sieve_dense_restore_053
    btr qword [rsi + 368], 50
    cmp ecx, 48
    jb run_sieve_dense_restore_053
    btr qword [rsi + 376], 39
    cmp ecx, 49
    jb run_sieve_dense_restore_053
    btr qword [rsi + 384], 28
    cmp ecx, 50
    jb run_sieve_dense_restore_053
    btr qword [rsi + 392], 17
    cmp ecx, 51
    jb run_sieve_dense_restore_053
    mov rax, qword [rsi + 400]
    btr rax, 6
    btr rax, 59
    mov qword [rsi + 400], rax
    cmp ecx, 52
    jb run_sieve_dense_restore_053
    btr qword [rsi + 408], 48
    cmp ecx, 53
    jb run_sieve_dense_restore_053
    btr qword [rsi + 416], 37
run_sieve_dense_restore_053:
    bts qword [r14], 26
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_055:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 55
    jb run_sieve_dense_tail_055
align 16
run_sieve_dense_loop_055:
    btr qword [rsi], 27
    btr qword [rsi + 8], 18
    btr qword [rsi + 16], 9
    mov rax, qword [rsi + 24]
    btr rax, 0
    btr rax, 55
    mov qword [rsi + 24], rax
    btr qword [rsi + 32], 46
    btr qword [rsi + 40], 37
    btr qword [rsi + 48], 28
    btr qword [rsi + 56], 19
    btr qword [rsi + 64], 10
    mov rax, qword [rsi + 72]
    btr rax, 1
    btr rax, 56
    mov qword [rsi + 72], rax
    btr qword [rsi + 80], 47
    btr qword [rsi + 88], 38
    btr qword [rsi + 96], 29
    btr qword [rsi + 104], 20
    btr qword [rsi + 112], 11
    mov rax, qword [rsi + 120]
    btr rax, 2
    btr rax, 57
    mov qword [rsi + 120], rax
    btr qword [rsi + 128], 48
    btr qword [rsi + 136], 39
    btr qword [rsi + 144], 30
    btr qword [rsi + 152], 21
    btr qword [rsi + 160], 12
    mov rax, qword [rsi + 168]
    btr rax, 3
    btr rax, 58
    mov qword [rsi + 168], rax
    btr qword [rsi + 176], 49
    btr qword [rsi + 184], 40
    btr qword [rsi + 192], 31
    btr qword [rsi + 200], 22
    btr qword [rsi + 208], 13
    mov rax, qword [rsi + 216]
    btr rax, 4
    btr rax, 59
    mov qword [rsi + 216], rax
    btr qword [rsi + 224], 50
    btr qword [rsi + 232], 41
    btr qword [rsi + 240], 32
    btr qword [rsi + 248], 23
    btr qword [rsi + 256], 14
    mov rax, qword [rsi + 264]
    btr rax, 5
    btr rax, 60
    mov qword [rsi + 264], rax
    btr qword [rsi + 272], 51
    btr qword [rsi + 280], 42
    btr qword [rsi + 288], 33
    btr qword [rsi + 296], 24
    btr qword [rsi + 304], 15
    mov rax, qword [rsi + 312]
    btr rax, 6
    btr rax, 61
    mov qword [rsi + 312], rax
    btr qword [rsi + 320], 52
    btr qword [rsi + 328], 43
    btr qword [rsi + 336], 34
    btr qword [rsi + 344], 25
    btr qword [rsi + 352], 16
    mov rax, qword [rsi + 360]
    btr rax, 7
    btr rax, 62
    mov qword [rsi + 360], rax
    btr qword [rsi + 368], 53
    btr qword [rsi + 376], 44
    btr qword [rsi + 384], 35
    btr qword [rsi + 392], 26
    btr qword [rsi + 400], 17
    mov rax, qword [rsi + 408]
    btr rax, 8
    btr rax, 63
    mov qword [rsi + 408], rax
    btr qword [rsi + 416], 54
    btr qword [rsi + 424], 45
    btr qword [rsi + 432], 36
    add rsi, 440
    sub ecx, 55
    cmp ecx, 55
    jae run_sieve_dense_loop_055
run_sieve_dense_tail_055:
    test ecx, ecx
    jz run_sieve_dense_restore_055
    cmp ecx, 1
    jb run_sieve_dense_restore_055
    btr qword [rsi], 27
    cmp ecx, 2
    jb run_sieve_dense_restore_055
    btr qword [rsi + 8], 18
    cmp ecx, 3
    jb run_sieve_dense_restore_055
    btr qword [rsi + 16], 9
    cmp ecx, 4
    jb run_sieve_dense_restore_055
    mov rax, qword [rsi + 24]
    btr rax, 0
    btr rax, 55
    mov qword [rsi + 24], rax
    cmp ecx, 5
    jb run_sieve_dense_restore_055
    btr qword [rsi + 32], 46
    cmp ecx, 6
    jb run_sieve_dense_restore_055
    btr qword [rsi + 40], 37
    cmp ecx, 7
    jb run_sieve_dense_restore_055
    btr qword [rsi + 48], 28
    cmp ecx, 8
    jb run_sieve_dense_restore_055
    btr qword [rsi + 56], 19
    cmp ecx, 9
    jb run_sieve_dense_restore_055
    btr qword [rsi + 64], 10
    cmp ecx, 10
    jb run_sieve_dense_restore_055
    mov rax, qword [rsi + 72]
    btr rax, 1
    btr rax, 56
    mov qword [rsi + 72], rax
    cmp ecx, 11
    jb run_sieve_dense_restore_055
    btr qword [rsi + 80], 47
    cmp ecx, 12
    jb run_sieve_dense_restore_055
    btr qword [rsi + 88], 38
    cmp ecx, 13
    jb run_sieve_dense_restore_055
    btr qword [rsi + 96], 29
    cmp ecx, 14
    jb run_sieve_dense_restore_055
    btr qword [rsi + 104], 20
    cmp ecx, 15
    jb run_sieve_dense_restore_055
    btr qword [rsi + 112], 11
    cmp ecx, 16
    jb run_sieve_dense_restore_055
    mov rax, qword [rsi + 120]
    btr rax, 2
    btr rax, 57
    mov qword [rsi + 120], rax
    cmp ecx, 17
    jb run_sieve_dense_restore_055
    btr qword [rsi + 128], 48
    cmp ecx, 18
    jb run_sieve_dense_restore_055
    btr qword [rsi + 136], 39
    cmp ecx, 19
    jb run_sieve_dense_restore_055
    btr qword [rsi + 144], 30
    cmp ecx, 20
    jb run_sieve_dense_restore_055
    btr qword [rsi + 152], 21
    cmp ecx, 21
    jb run_sieve_dense_restore_055
    btr qword [rsi + 160], 12
    cmp ecx, 22
    jb run_sieve_dense_restore_055
    mov rax, qword [rsi + 168]
    btr rax, 3
    btr rax, 58
    mov qword [rsi + 168], rax
    cmp ecx, 23
    jb run_sieve_dense_restore_055
    btr qword [rsi + 176], 49
    cmp ecx, 24
    jb run_sieve_dense_restore_055
    btr qword [rsi + 184], 40
    cmp ecx, 25
    jb run_sieve_dense_restore_055
    btr qword [rsi + 192], 31
    cmp ecx, 26
    jb run_sieve_dense_restore_055
    btr qword [rsi + 200], 22
    cmp ecx, 27
    jb run_sieve_dense_restore_055
    btr qword [rsi + 208], 13
    cmp ecx, 28
    jb run_sieve_dense_restore_055
    mov rax, qword [rsi + 216]
    btr rax, 4
    btr rax, 59
    mov qword [rsi + 216], rax
    cmp ecx, 29
    jb run_sieve_dense_restore_055
    btr qword [rsi + 224], 50
    cmp ecx, 30
    jb run_sieve_dense_restore_055
    btr qword [rsi + 232], 41
    cmp ecx, 31
    jb run_sieve_dense_restore_055
    btr qword [rsi + 240], 32
    cmp ecx, 32
    jb run_sieve_dense_restore_055
    btr qword [rsi + 248], 23
    cmp ecx, 33
    jb run_sieve_dense_restore_055
    btr qword [rsi + 256], 14
    cmp ecx, 34
    jb run_sieve_dense_restore_055
    mov rax, qword [rsi + 264]
    btr rax, 5
    btr rax, 60
    mov qword [rsi + 264], rax
    cmp ecx, 35
    jb run_sieve_dense_restore_055
    btr qword [rsi + 272], 51
    cmp ecx, 36
    jb run_sieve_dense_restore_055
    btr qword [rsi + 280], 42
    cmp ecx, 37
    jb run_sieve_dense_restore_055
    btr qword [rsi + 288], 33
    cmp ecx, 38
    jb run_sieve_dense_restore_055
    btr qword [rsi + 296], 24
    cmp ecx, 39
    jb run_sieve_dense_restore_055
    btr qword [rsi + 304], 15
    cmp ecx, 40
    jb run_sieve_dense_restore_055
    mov rax, qword [rsi + 312]
    btr rax, 6
    btr rax, 61
    mov qword [rsi + 312], rax
    cmp ecx, 41
    jb run_sieve_dense_restore_055
    btr qword [rsi + 320], 52
    cmp ecx, 42
    jb run_sieve_dense_restore_055
    btr qword [rsi + 328], 43
    cmp ecx, 43
    jb run_sieve_dense_restore_055
    btr qword [rsi + 336], 34
    cmp ecx, 44
    jb run_sieve_dense_restore_055
    btr qword [rsi + 344], 25
    cmp ecx, 45
    jb run_sieve_dense_restore_055
    btr qword [rsi + 352], 16
    cmp ecx, 46
    jb run_sieve_dense_restore_055
    mov rax, qword [rsi + 360]
    btr rax, 7
    btr rax, 62
    mov qword [rsi + 360], rax
    cmp ecx, 47
    jb run_sieve_dense_restore_055
    btr qword [rsi + 368], 53
    cmp ecx, 48
    jb run_sieve_dense_restore_055
    btr qword [rsi + 376], 44
    cmp ecx, 49
    jb run_sieve_dense_restore_055
    btr qword [rsi + 384], 35
    cmp ecx, 50
    jb run_sieve_dense_restore_055
    btr qword [rsi + 392], 26
    cmp ecx, 51
    jb run_sieve_dense_restore_055
    btr qword [rsi + 400], 17
    cmp ecx, 52
    jb run_sieve_dense_restore_055
    mov rax, qword [rsi + 408]
    btr rax, 8
    btr rax, 63
    mov qword [rsi + 408], rax
    cmp ecx, 53
    jb run_sieve_dense_restore_055
    btr qword [rsi + 416], 54
    cmp ecx, 54
    jb run_sieve_dense_restore_055
    btr qword [rsi + 424], 45
    cmp ecx, 55
    jb run_sieve_dense_restore_055
    btr qword [rsi + 432], 36
run_sieve_dense_restore_055:
    bts qword [r14], 27
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_057:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 57
    jb run_sieve_dense_tail_057
align 16
run_sieve_dense_loop_057:
    btr qword [rsi], 28
    btr qword [rsi + 8], 21
    btr qword [rsi + 16], 14
    btr qword [rsi + 24], 7
    mov rax, qword [rsi + 32]
    btr rax, 0
    btr rax, 57
    mov qword [rsi + 32], rax
    btr qword [rsi + 40], 50
    btr qword [rsi + 48], 43
    btr qword [rsi + 56], 36
    btr qword [rsi + 64], 29
    btr qword [rsi + 72], 22
    btr qword [rsi + 80], 15
    btr qword [rsi + 88], 8
    mov rax, qword [rsi + 96]
    btr rax, 1
    btr rax, 58
    mov qword [rsi + 96], rax
    btr qword [rsi + 104], 51
    btr qword [rsi + 112], 44
    btr qword [rsi + 120], 37
    btr qword [rsi + 128], 30
    btr qword [rsi + 136], 23
    btr qword [rsi + 144], 16
    btr qword [rsi + 152], 9
    mov rax, qword [rsi + 160]
    btr rax, 2
    btr rax, 59
    mov qword [rsi + 160], rax
    btr qword [rsi + 168], 52
    btr qword [rsi + 176], 45
    btr qword [rsi + 184], 38
    btr qword [rsi + 192], 31
    btr qword [rsi + 200], 24
    btr qword [rsi + 208], 17
    btr qword [rsi + 216], 10
    mov rax, qword [rsi + 224]
    btr rax, 3
    btr rax, 60
    mov qword [rsi + 224], rax
    btr qword [rsi + 232], 53
    btr qword [rsi + 240], 46
    btr qword [rsi + 248], 39
    btr qword [rsi + 256], 32
    btr qword [rsi + 264], 25
    btr qword [rsi + 272], 18
    btr qword [rsi + 280], 11
    mov rax, qword [rsi + 288]
    btr rax, 4
    btr rax, 61
    mov qword [rsi + 288], rax
    btr qword [rsi + 296], 54
    btr qword [rsi + 304], 47
    btr qword [rsi + 312], 40
    btr qword [rsi + 320], 33
    btr qword [rsi + 328], 26
    btr qword [rsi + 336], 19
    btr qword [rsi + 344], 12
    mov rax, qword [rsi + 352]
    btr rax, 5
    btr rax, 62
    mov qword [rsi + 352], rax
    btr qword [rsi + 360], 55
    btr qword [rsi + 368], 48
    btr qword [rsi + 376], 41
    btr qword [rsi + 384], 34
    btr qword [rsi + 392], 27
    btr qword [rsi + 400], 20
    btr qword [rsi + 408], 13
    mov rax, qword [rsi + 416]
    btr rax, 6
    btr rax, 63
    mov qword [rsi + 416], rax
    btr qword [rsi + 424], 56
    btr qword [rsi + 432], 49
    btr qword [rsi + 440], 42
    btr qword [rsi + 448], 35
    add rsi, 456
    sub ecx, 57
    cmp ecx, 57
    jae run_sieve_dense_loop_057
run_sieve_dense_tail_057:
    test ecx, ecx
    jz run_sieve_dense_restore_057
    cmp ecx, 1
    jb run_sieve_dense_restore_057
    btr qword [rsi], 28
    cmp ecx, 2
    jb run_sieve_dense_restore_057
    btr qword [rsi + 8], 21
    cmp ecx, 3
    jb run_sieve_dense_restore_057
    btr qword [rsi + 16], 14
    cmp ecx, 4
    jb run_sieve_dense_restore_057
    btr qword [rsi + 24], 7
    cmp ecx, 5
    jb run_sieve_dense_restore_057
    mov rax, qword [rsi + 32]
    btr rax, 0
    btr rax, 57
    mov qword [rsi + 32], rax
    cmp ecx, 6
    jb run_sieve_dense_restore_057
    btr qword [rsi + 40], 50
    cmp ecx, 7
    jb run_sieve_dense_restore_057
    btr qword [rsi + 48], 43
    cmp ecx, 8
    jb run_sieve_dense_restore_057
    btr qword [rsi + 56], 36
    cmp ecx, 9
    jb run_sieve_dense_restore_057
    btr qword [rsi + 64], 29
    cmp ecx, 10
    jb run_sieve_dense_restore_057
    btr qword [rsi + 72], 22
    cmp ecx, 11
    jb run_sieve_dense_restore_057
    btr qword [rsi + 80], 15
    cmp ecx, 12
    jb run_sieve_dense_restore_057
    btr qword [rsi + 88], 8
    cmp ecx, 13
    jb run_sieve_dense_restore_057
    mov rax, qword [rsi + 96]
    btr rax, 1
    btr rax, 58
    mov qword [rsi + 96], rax
    cmp ecx, 14
    jb run_sieve_dense_restore_057
    btr qword [rsi + 104], 51
    cmp ecx, 15
    jb run_sieve_dense_restore_057
    btr qword [rsi + 112], 44
    cmp ecx, 16
    jb run_sieve_dense_restore_057
    btr qword [rsi + 120], 37
    cmp ecx, 17
    jb run_sieve_dense_restore_057
    btr qword [rsi + 128], 30
    cmp ecx, 18
    jb run_sieve_dense_restore_057
    btr qword [rsi + 136], 23
    cmp ecx, 19
    jb run_sieve_dense_restore_057
    btr qword [rsi + 144], 16
    cmp ecx, 20
    jb run_sieve_dense_restore_057
    btr qword [rsi + 152], 9
    cmp ecx, 21
    jb run_sieve_dense_restore_057
    mov rax, qword [rsi + 160]
    btr rax, 2
    btr rax, 59
    mov qword [rsi + 160], rax
    cmp ecx, 22
    jb run_sieve_dense_restore_057
    btr qword [rsi + 168], 52
    cmp ecx, 23
    jb run_sieve_dense_restore_057
    btr qword [rsi + 176], 45
    cmp ecx, 24
    jb run_sieve_dense_restore_057
    btr qword [rsi + 184], 38
    cmp ecx, 25
    jb run_sieve_dense_restore_057
    btr qword [rsi + 192], 31
    cmp ecx, 26
    jb run_sieve_dense_restore_057
    btr qword [rsi + 200], 24
    cmp ecx, 27
    jb run_sieve_dense_restore_057
    btr qword [rsi + 208], 17
    cmp ecx, 28
    jb run_sieve_dense_restore_057
    btr qword [rsi + 216], 10
    cmp ecx, 29
    jb run_sieve_dense_restore_057
    mov rax, qword [rsi + 224]
    btr rax, 3
    btr rax, 60
    mov qword [rsi + 224], rax
    cmp ecx, 30
    jb run_sieve_dense_restore_057
    btr qword [rsi + 232], 53
    cmp ecx, 31
    jb run_sieve_dense_restore_057
    btr qword [rsi + 240], 46
    cmp ecx, 32
    jb run_sieve_dense_restore_057
    btr qword [rsi + 248], 39
    cmp ecx, 33
    jb run_sieve_dense_restore_057
    btr qword [rsi + 256], 32
    cmp ecx, 34
    jb run_sieve_dense_restore_057
    btr qword [rsi + 264], 25
    cmp ecx, 35
    jb run_sieve_dense_restore_057
    btr qword [rsi + 272], 18
    cmp ecx, 36
    jb run_sieve_dense_restore_057
    btr qword [rsi + 280], 11
    cmp ecx, 37
    jb run_sieve_dense_restore_057
    mov rax, qword [rsi + 288]
    btr rax, 4
    btr rax, 61
    mov qword [rsi + 288], rax
    cmp ecx, 38
    jb run_sieve_dense_restore_057
    btr qword [rsi + 296], 54
    cmp ecx, 39
    jb run_sieve_dense_restore_057
    btr qword [rsi + 304], 47
    cmp ecx, 40
    jb run_sieve_dense_restore_057
    btr qword [rsi + 312], 40
    cmp ecx, 41
    jb run_sieve_dense_restore_057
    btr qword [rsi + 320], 33
    cmp ecx, 42
    jb run_sieve_dense_restore_057
    btr qword [rsi + 328], 26
    cmp ecx, 43
    jb run_sieve_dense_restore_057
    btr qword [rsi + 336], 19
    cmp ecx, 44
    jb run_sieve_dense_restore_057
    btr qword [rsi + 344], 12
    cmp ecx, 45
    jb run_sieve_dense_restore_057
    mov rax, qword [rsi + 352]
    btr rax, 5
    btr rax, 62
    mov qword [rsi + 352], rax
    cmp ecx, 46
    jb run_sieve_dense_restore_057
    btr qword [rsi + 360], 55
    cmp ecx, 47
    jb run_sieve_dense_restore_057
    btr qword [rsi + 368], 48
    cmp ecx, 48
    jb run_sieve_dense_restore_057
    btr qword [rsi + 376], 41
    cmp ecx, 49
    jb run_sieve_dense_restore_057
    btr qword [rsi + 384], 34
    cmp ecx, 50
    jb run_sieve_dense_restore_057
    btr qword [rsi + 392], 27
    cmp ecx, 51
    jb run_sieve_dense_restore_057
    btr qword [rsi + 400], 20
    cmp ecx, 52
    jb run_sieve_dense_restore_057
    btr qword [rsi + 408], 13
    cmp ecx, 53
    jb run_sieve_dense_restore_057
    mov rax, qword [rsi + 416]
    btr rax, 6
    btr rax, 63
    mov qword [rsi + 416], rax
    cmp ecx, 54
    jb run_sieve_dense_restore_057
    btr qword [rsi + 424], 56
    cmp ecx, 55
    jb run_sieve_dense_restore_057
    btr qword [rsi + 432], 49
    cmp ecx, 56
    jb run_sieve_dense_restore_057
    btr qword [rsi + 440], 42
    cmp ecx, 57
    jb run_sieve_dense_restore_057
    btr qword [rsi + 448], 35
run_sieve_dense_restore_057:
    bts qword [r14], 28
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_059:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 59
    jb run_sieve_dense_tail_059
align 16
run_sieve_dense_loop_059:
    btr qword [rsi], 29
    btr qword [rsi + 8], 24
    btr qword [rsi + 16], 19
    btr qword [rsi + 24], 14
    btr qword [rsi + 32], 9
    mov rax, qword [rsi + 40]
    btr rax, 4
    btr rax, 63
    mov qword [rsi + 40], rax
    btr qword [rsi + 48], 58
    btr qword [rsi + 56], 53
    btr qword [rsi + 64], 48
    btr qword [rsi + 72], 43
    btr qword [rsi + 80], 38
    btr qword [rsi + 88], 33
    btr qword [rsi + 96], 28
    btr qword [rsi + 104], 23
    btr qword [rsi + 112], 18
    btr qword [rsi + 120], 13
    btr qword [rsi + 128], 8
    mov rax, qword [rsi + 136]
    btr rax, 3
    btr rax, 62
    mov qword [rsi + 136], rax
    btr qword [rsi + 144], 57
    btr qword [rsi + 152], 52
    btr qword [rsi + 160], 47
    btr qword [rsi + 168], 42
    btr qword [rsi + 176], 37
    btr qword [rsi + 184], 32
    btr qword [rsi + 192], 27
    btr qword [rsi + 200], 22
    btr qword [rsi + 208], 17
    btr qword [rsi + 216], 12
    btr qword [rsi + 224], 7
    mov rax, qword [rsi + 232]
    btr rax, 2
    btr rax, 61
    mov qword [rsi + 232], rax
    btr qword [rsi + 240], 56
    btr qword [rsi + 248], 51
    btr qword [rsi + 256], 46
    btr qword [rsi + 264], 41
    btr qword [rsi + 272], 36
    btr qword [rsi + 280], 31
    btr qword [rsi + 288], 26
    btr qword [rsi + 296], 21
    btr qword [rsi + 304], 16
    btr qword [rsi + 312], 11
    btr qword [rsi + 320], 6
    mov rax, qword [rsi + 328]
    btr rax, 1
    btr rax, 60
    mov qword [rsi + 328], rax
    btr qword [rsi + 336], 55
    btr qword [rsi + 344], 50
    btr qword [rsi + 352], 45
    btr qword [rsi + 360], 40
    btr qword [rsi + 368], 35
    btr qword [rsi + 376], 30
    btr qword [rsi + 384], 25
    btr qword [rsi + 392], 20
    btr qword [rsi + 400], 15
    btr qword [rsi + 408], 10
    btr qword [rsi + 416], 5
    mov rax, qword [rsi + 424]
    btr rax, 0
    btr rax, 59
    mov qword [rsi + 424], rax
    btr qword [rsi + 432], 54
    btr qword [rsi + 440], 49
    btr qword [rsi + 448], 44
    btr qword [rsi + 456], 39
    btr qword [rsi + 464], 34
    add rsi, 472
    sub ecx, 59
    cmp ecx, 59
    jae run_sieve_dense_loop_059
run_sieve_dense_tail_059:
    test ecx, ecx
    jz run_sieve_dense_restore_059
    cmp ecx, 1
    jb run_sieve_dense_restore_059
    btr qword [rsi], 29
    cmp ecx, 2
    jb run_sieve_dense_restore_059
    btr qword [rsi + 8], 24
    cmp ecx, 3
    jb run_sieve_dense_restore_059
    btr qword [rsi + 16], 19
    cmp ecx, 4
    jb run_sieve_dense_restore_059
    btr qword [rsi + 24], 14
    cmp ecx, 5
    jb run_sieve_dense_restore_059
    btr qword [rsi + 32], 9
    cmp ecx, 6
    jb run_sieve_dense_restore_059
    mov rax, qword [rsi + 40]
    btr rax, 4
    btr rax, 63
    mov qword [rsi + 40], rax
    cmp ecx, 7
    jb run_sieve_dense_restore_059
    btr qword [rsi + 48], 58
    cmp ecx, 8
    jb run_sieve_dense_restore_059
    btr qword [rsi + 56], 53
    cmp ecx, 9
    jb run_sieve_dense_restore_059
    btr qword [rsi + 64], 48
    cmp ecx, 10
    jb run_sieve_dense_restore_059
    btr qword [rsi + 72], 43
    cmp ecx, 11
    jb run_sieve_dense_restore_059
    btr qword [rsi + 80], 38
    cmp ecx, 12
    jb run_sieve_dense_restore_059
    btr qword [rsi + 88], 33
    cmp ecx, 13
    jb run_sieve_dense_restore_059
    btr qword [rsi + 96], 28
    cmp ecx, 14
    jb run_sieve_dense_restore_059
    btr qword [rsi + 104], 23
    cmp ecx, 15
    jb run_sieve_dense_restore_059
    btr qword [rsi + 112], 18
    cmp ecx, 16
    jb run_sieve_dense_restore_059
    btr qword [rsi + 120], 13
    cmp ecx, 17
    jb run_sieve_dense_restore_059
    btr qword [rsi + 128], 8
    cmp ecx, 18
    jb run_sieve_dense_restore_059
    mov rax, qword [rsi + 136]
    btr rax, 3
    btr rax, 62
    mov qword [rsi + 136], rax
    cmp ecx, 19
    jb run_sieve_dense_restore_059
    btr qword [rsi + 144], 57
    cmp ecx, 20
    jb run_sieve_dense_restore_059
    btr qword [rsi + 152], 52
    cmp ecx, 21
    jb run_sieve_dense_restore_059
    btr qword [rsi + 160], 47
    cmp ecx, 22
    jb run_sieve_dense_restore_059
    btr qword [rsi + 168], 42
    cmp ecx, 23
    jb run_sieve_dense_restore_059
    btr qword [rsi + 176], 37
    cmp ecx, 24
    jb run_sieve_dense_restore_059
    btr qword [rsi + 184], 32
    cmp ecx, 25
    jb run_sieve_dense_restore_059
    btr qword [rsi + 192], 27
    cmp ecx, 26
    jb run_sieve_dense_restore_059
    btr qword [rsi + 200], 22
    cmp ecx, 27
    jb run_sieve_dense_restore_059
    btr qword [rsi + 208], 17
    cmp ecx, 28
    jb run_sieve_dense_restore_059
    btr qword [rsi + 216], 12
    cmp ecx, 29
    jb run_sieve_dense_restore_059
    btr qword [rsi + 224], 7
    cmp ecx, 30
    jb run_sieve_dense_restore_059
    mov rax, qword [rsi + 232]
    btr rax, 2
    btr rax, 61
    mov qword [rsi + 232], rax
    cmp ecx, 31
    jb run_sieve_dense_restore_059
    btr qword [rsi + 240], 56
    cmp ecx, 32
    jb run_sieve_dense_restore_059
    btr qword [rsi + 248], 51
    cmp ecx, 33
    jb run_sieve_dense_restore_059
    btr qword [rsi + 256], 46
    cmp ecx, 34
    jb run_sieve_dense_restore_059
    btr qword [rsi + 264], 41
    cmp ecx, 35
    jb run_sieve_dense_restore_059
    btr qword [rsi + 272], 36
    cmp ecx, 36
    jb run_sieve_dense_restore_059
    btr qword [rsi + 280], 31
    cmp ecx, 37
    jb run_sieve_dense_restore_059
    btr qword [rsi + 288], 26
    cmp ecx, 38
    jb run_sieve_dense_restore_059
    btr qword [rsi + 296], 21
    cmp ecx, 39
    jb run_sieve_dense_restore_059
    btr qword [rsi + 304], 16
    cmp ecx, 40
    jb run_sieve_dense_restore_059
    btr qword [rsi + 312], 11
    cmp ecx, 41
    jb run_sieve_dense_restore_059
    btr qword [rsi + 320], 6
    cmp ecx, 42
    jb run_sieve_dense_restore_059
    mov rax, qword [rsi + 328]
    btr rax, 1
    btr rax, 60
    mov qword [rsi + 328], rax
    cmp ecx, 43
    jb run_sieve_dense_restore_059
    btr qword [rsi + 336], 55
    cmp ecx, 44
    jb run_sieve_dense_restore_059
    btr qword [rsi + 344], 50
    cmp ecx, 45
    jb run_sieve_dense_restore_059
    btr qword [rsi + 352], 45
    cmp ecx, 46
    jb run_sieve_dense_restore_059
    btr qword [rsi + 360], 40
    cmp ecx, 47
    jb run_sieve_dense_restore_059
    btr qword [rsi + 368], 35
    cmp ecx, 48
    jb run_sieve_dense_restore_059
    btr qword [rsi + 376], 30
    cmp ecx, 49
    jb run_sieve_dense_restore_059
    btr qword [rsi + 384], 25
    cmp ecx, 50
    jb run_sieve_dense_restore_059
    btr qword [rsi + 392], 20
    cmp ecx, 51
    jb run_sieve_dense_restore_059
    btr qword [rsi + 400], 15
    cmp ecx, 52
    jb run_sieve_dense_restore_059
    btr qword [rsi + 408], 10
    cmp ecx, 53
    jb run_sieve_dense_restore_059
    btr qword [rsi + 416], 5
    cmp ecx, 54
    jb run_sieve_dense_restore_059
    mov rax, qword [rsi + 424]
    btr rax, 0
    btr rax, 59
    mov qword [rsi + 424], rax
    cmp ecx, 55
    jb run_sieve_dense_restore_059
    btr qword [rsi + 432], 54
    cmp ecx, 56
    jb run_sieve_dense_restore_059
    btr qword [rsi + 440], 49
    cmp ecx, 57
    jb run_sieve_dense_restore_059
    btr qword [rsi + 448], 44
    cmp ecx, 58
    jb run_sieve_dense_restore_059
    btr qword [rsi + 456], 39
    cmp ecx, 59
    jb run_sieve_dense_restore_059
    btr qword [rsi + 464], 34
run_sieve_dense_restore_059:
    bts qword [r14], 29
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_061:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 61
    jb run_sieve_dense_tail_061
align 16
run_sieve_dense_loop_061:
    btr qword [rsi], 30
    btr qword [rsi + 8], 27
    btr qword [rsi + 16], 24
    btr qword [rsi + 24], 21
    btr qword [rsi + 32], 18
    btr qword [rsi + 40], 15
    btr qword [rsi + 48], 12
    btr qword [rsi + 56], 9
    btr qword [rsi + 64], 6
    btr qword [rsi + 72], 3
    mov rax, qword [rsi + 80]
    btr rax, 0
    btr rax, 61
    mov qword [rsi + 80], rax
    btr qword [rsi + 88], 58
    btr qword [rsi + 96], 55
    btr qword [rsi + 104], 52
    btr qword [rsi + 112], 49
    btr qword [rsi + 120], 46
    btr qword [rsi + 128], 43
    btr qword [rsi + 136], 40
    btr qword [rsi + 144], 37
    btr qword [rsi + 152], 34
    btr qword [rsi + 160], 31
    btr qword [rsi + 168], 28
    btr qword [rsi + 176], 25
    btr qword [rsi + 184], 22
    btr qword [rsi + 192], 19
    btr qword [rsi + 200], 16
    btr qword [rsi + 208], 13
    btr qword [rsi + 216], 10
    btr qword [rsi + 224], 7
    btr qword [rsi + 232], 4
    mov rax, qword [rsi + 240]
    btr rax, 1
    btr rax, 62
    mov qword [rsi + 240], rax
    btr qword [rsi + 248], 59
    btr qword [rsi + 256], 56
    btr qword [rsi + 264], 53
    btr qword [rsi + 272], 50
    btr qword [rsi + 280], 47
    btr qword [rsi + 288], 44
    btr qword [rsi + 296], 41
    btr qword [rsi + 304], 38
    btr qword [rsi + 312], 35
    btr qword [rsi + 320], 32
    btr qword [rsi + 328], 29
    btr qword [rsi + 336], 26
    btr qword [rsi + 344], 23
    btr qword [rsi + 352], 20
    btr qword [rsi + 360], 17
    btr qword [rsi + 368], 14
    btr qword [rsi + 376], 11
    btr qword [rsi + 384], 8
    btr qword [rsi + 392], 5
    mov rax, qword [rsi + 400]
    btr rax, 2
    btr rax, 63
    mov qword [rsi + 400], rax
    btr qword [rsi + 408], 60
    btr qword [rsi + 416], 57
    btr qword [rsi + 424], 54
    btr qword [rsi + 432], 51
    btr qword [rsi + 440], 48
    btr qword [rsi + 448], 45
    btr qword [rsi + 456], 42
    btr qword [rsi + 464], 39
    btr qword [rsi + 472], 36
    btr qword [rsi + 480], 33
    add rsi, 488
    sub ecx, 61
    cmp ecx, 61
    jae run_sieve_dense_loop_061
run_sieve_dense_tail_061:
    test ecx, ecx
    jz run_sieve_dense_restore_061
    cmp ecx, 1
    jb run_sieve_dense_restore_061
    btr qword [rsi], 30
    cmp ecx, 2
    jb run_sieve_dense_restore_061
    btr qword [rsi + 8], 27
    cmp ecx, 3
    jb run_sieve_dense_restore_061
    btr qword [rsi + 16], 24
    cmp ecx, 4
    jb run_sieve_dense_restore_061
    btr qword [rsi + 24], 21
    cmp ecx, 5
    jb run_sieve_dense_restore_061
    btr qword [rsi + 32], 18
    cmp ecx, 6
    jb run_sieve_dense_restore_061
    btr qword [rsi + 40], 15
    cmp ecx, 7
    jb run_sieve_dense_restore_061
    btr qword [rsi + 48], 12
    cmp ecx, 8
    jb run_sieve_dense_restore_061
    btr qword [rsi + 56], 9
    cmp ecx, 9
    jb run_sieve_dense_restore_061
    btr qword [rsi + 64], 6
    cmp ecx, 10
    jb run_sieve_dense_restore_061
    btr qword [rsi + 72], 3
    cmp ecx, 11
    jb run_sieve_dense_restore_061
    mov rax, qword [rsi + 80]
    btr rax, 0
    btr rax, 61
    mov qword [rsi + 80], rax
    cmp ecx, 12
    jb run_sieve_dense_restore_061
    btr qword [rsi + 88], 58
    cmp ecx, 13
    jb run_sieve_dense_restore_061
    btr qword [rsi + 96], 55
    cmp ecx, 14
    jb run_sieve_dense_restore_061
    btr qword [rsi + 104], 52
    cmp ecx, 15
    jb run_sieve_dense_restore_061
    btr qword [rsi + 112], 49
    cmp ecx, 16
    jb run_sieve_dense_restore_061
    btr qword [rsi + 120], 46
    cmp ecx, 17
    jb run_sieve_dense_restore_061
    btr qword [rsi + 128], 43
    cmp ecx, 18
    jb run_sieve_dense_restore_061
    btr qword [rsi + 136], 40
    cmp ecx, 19
    jb run_sieve_dense_restore_061
    btr qword [rsi + 144], 37
    cmp ecx, 20
    jb run_sieve_dense_restore_061
    btr qword [rsi + 152], 34
    cmp ecx, 21
    jb run_sieve_dense_restore_061
    btr qword [rsi + 160], 31
    cmp ecx, 22
    jb run_sieve_dense_restore_061
    btr qword [rsi + 168], 28
    cmp ecx, 23
    jb run_sieve_dense_restore_061
    btr qword [rsi + 176], 25
    cmp ecx, 24
    jb run_sieve_dense_restore_061
    btr qword [rsi + 184], 22
    cmp ecx, 25
    jb run_sieve_dense_restore_061
    btr qword [rsi + 192], 19
    cmp ecx, 26
    jb run_sieve_dense_restore_061
    btr qword [rsi + 200], 16
    cmp ecx, 27
    jb run_sieve_dense_restore_061
    btr qword [rsi + 208], 13
    cmp ecx, 28
    jb run_sieve_dense_restore_061
    btr qword [rsi + 216], 10
    cmp ecx, 29
    jb run_sieve_dense_restore_061
    btr qword [rsi + 224], 7
    cmp ecx, 30
    jb run_sieve_dense_restore_061
    btr qword [rsi + 232], 4
    cmp ecx, 31
    jb run_sieve_dense_restore_061
    mov rax, qword [rsi + 240]
    btr rax, 1
    btr rax, 62
    mov qword [rsi + 240], rax
    cmp ecx, 32
    jb run_sieve_dense_restore_061
    btr qword [rsi + 248], 59
    cmp ecx, 33
    jb run_sieve_dense_restore_061
    btr qword [rsi + 256], 56
    cmp ecx, 34
    jb run_sieve_dense_restore_061
    btr qword [rsi + 264], 53
    cmp ecx, 35
    jb run_sieve_dense_restore_061
    btr qword [rsi + 272], 50
    cmp ecx, 36
    jb run_sieve_dense_restore_061
    btr qword [rsi + 280], 47
    cmp ecx, 37
    jb run_sieve_dense_restore_061
    btr qword [rsi + 288], 44
    cmp ecx, 38
    jb run_sieve_dense_restore_061
    btr qword [rsi + 296], 41
    cmp ecx, 39
    jb run_sieve_dense_restore_061
    btr qword [rsi + 304], 38
    cmp ecx, 40
    jb run_sieve_dense_restore_061
    btr qword [rsi + 312], 35
    cmp ecx, 41
    jb run_sieve_dense_restore_061
    btr qword [rsi + 320], 32
    cmp ecx, 42
    jb run_sieve_dense_restore_061
    btr qword [rsi + 328], 29
    cmp ecx, 43
    jb run_sieve_dense_restore_061
    btr qword [rsi + 336], 26
    cmp ecx, 44
    jb run_sieve_dense_restore_061
    btr qword [rsi + 344], 23
    cmp ecx, 45
    jb run_sieve_dense_restore_061
    btr qword [rsi + 352], 20
    cmp ecx, 46
    jb run_sieve_dense_restore_061
    btr qword [rsi + 360], 17
    cmp ecx, 47
    jb run_sieve_dense_restore_061
    btr qword [rsi + 368], 14
    cmp ecx, 48
    jb run_sieve_dense_restore_061
    btr qword [rsi + 376], 11
    cmp ecx, 49
    jb run_sieve_dense_restore_061
    btr qword [rsi + 384], 8
    cmp ecx, 50
    jb run_sieve_dense_restore_061
    btr qword [rsi + 392], 5
    cmp ecx, 51
    jb run_sieve_dense_restore_061
    mov rax, qword [rsi + 400]
    btr rax, 2
    btr rax, 63
    mov qword [rsi + 400], rax
    cmp ecx, 52
    jb run_sieve_dense_restore_061
    btr qword [rsi + 408], 60
    cmp ecx, 53
    jb run_sieve_dense_restore_061
    btr qword [rsi + 416], 57
    cmp ecx, 54
    jb run_sieve_dense_restore_061
    btr qword [rsi + 424], 54
    cmp ecx, 55
    jb run_sieve_dense_restore_061
    btr qword [rsi + 432], 51
    cmp ecx, 56
    jb run_sieve_dense_restore_061
    btr qword [rsi + 440], 48
    cmp ecx, 57
    jb run_sieve_dense_restore_061
    btr qword [rsi + 448], 45
    cmp ecx, 58
    jb run_sieve_dense_restore_061
    btr qword [rsi + 456], 42
    cmp ecx, 59
    jb run_sieve_dense_restore_061
    btr qword [rsi + 464], 39
    cmp ecx, 60
    jb run_sieve_dense_restore_061
    btr qword [rsi + 472], 36
    cmp ecx, 61
    jb run_sieve_dense_restore_061
    btr qword [rsi + 480], 33
run_sieve_dense_restore_061:
    bts qword [r14], 30
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_063:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 63
    jb run_sieve_dense_tail_063
align 16
run_sieve_dense_loop_063:
    btr qword [rsi], 31
    btr qword [rsi + 8], 30
    btr qword [rsi + 16], 29
    btr qword [rsi + 24], 28
    btr qword [rsi + 32], 27
    btr qword [rsi + 40], 26
    btr qword [rsi + 48], 25
    btr qword [rsi + 56], 24
    btr qword [rsi + 64], 23
    btr qword [rsi + 72], 22
    btr qword [rsi + 80], 21
    btr qword [rsi + 88], 20
    btr qword [rsi + 96], 19
    btr qword [rsi + 104], 18
    btr qword [rsi + 112], 17
    btr qword [rsi + 120], 16
    btr qword [rsi + 128], 15
    btr qword [rsi + 136], 14
    btr qword [rsi + 144], 13
    btr qword [rsi + 152], 12
    btr qword [rsi + 160], 11
    btr qword [rsi + 168], 10
    btr qword [rsi + 176], 9
    btr qword [rsi + 184], 8
    btr qword [rsi + 192], 7
    btr qword [rsi + 200], 6
    btr qword [rsi + 208], 5
    btr qword [rsi + 216], 4
    btr qword [rsi + 224], 3
    btr qword [rsi + 232], 2
    btr qword [rsi + 240], 1
    mov rax, qword [rsi + 248]
    btr rax, 0
    btr rax, 63
    mov qword [rsi + 248], rax
    btr qword [rsi + 256], 62
    btr qword [rsi + 264], 61
    btr qword [rsi + 272], 60
    btr qword [rsi + 280], 59
    btr qword [rsi + 288], 58
    btr qword [rsi + 296], 57
    btr qword [rsi + 304], 56
    btr qword [rsi + 312], 55
    btr qword [rsi + 320], 54
    btr qword [rsi + 328], 53
    btr qword [rsi + 336], 52
    btr qword [rsi + 344], 51
    btr qword [rsi + 352], 50
    btr qword [rsi + 360], 49
    btr qword [rsi + 368], 48
    btr qword [rsi + 376], 47
    btr qword [rsi + 384], 46
    btr qword [rsi + 392], 45
    btr qword [rsi + 400], 44
    btr qword [rsi + 408], 43
    btr qword [rsi + 416], 42
    btr qword [rsi + 424], 41
    btr qword [rsi + 432], 40
    btr qword [rsi + 440], 39
    btr qword [rsi + 448], 38
    btr qword [rsi + 456], 37
    btr qword [rsi + 464], 36
    btr qword [rsi + 472], 35
    btr qword [rsi + 480], 34
    btr qword [rsi + 488], 33
    btr qword [rsi + 496], 32
    add rsi, 504
    sub ecx, 63
    cmp ecx, 63
    jae run_sieve_dense_loop_063
run_sieve_dense_tail_063:
    test ecx, ecx
    jz run_sieve_dense_restore_063
    cmp ecx, 1
    jb run_sieve_dense_restore_063
    btr qword [rsi], 31
    cmp ecx, 2
    jb run_sieve_dense_restore_063
    btr qword [rsi + 8], 30
    cmp ecx, 3
    jb run_sieve_dense_restore_063
    btr qword [rsi + 16], 29
    cmp ecx, 4
    jb run_sieve_dense_restore_063
    btr qword [rsi + 24], 28
    cmp ecx, 5
    jb run_sieve_dense_restore_063
    btr qword [rsi + 32], 27
    cmp ecx, 6
    jb run_sieve_dense_restore_063
    btr qword [rsi + 40], 26
    cmp ecx, 7
    jb run_sieve_dense_restore_063
    btr qword [rsi + 48], 25
    cmp ecx, 8
    jb run_sieve_dense_restore_063
    btr qword [rsi + 56], 24
    cmp ecx, 9
    jb run_sieve_dense_restore_063
    btr qword [rsi + 64], 23
    cmp ecx, 10
    jb run_sieve_dense_restore_063
    btr qword [rsi + 72], 22
    cmp ecx, 11
    jb run_sieve_dense_restore_063
    btr qword [rsi + 80], 21
    cmp ecx, 12
    jb run_sieve_dense_restore_063
    btr qword [rsi + 88], 20
    cmp ecx, 13
    jb run_sieve_dense_restore_063
    btr qword [rsi + 96], 19
    cmp ecx, 14
    jb run_sieve_dense_restore_063
    btr qword [rsi + 104], 18
    cmp ecx, 15
    jb run_sieve_dense_restore_063
    btr qword [rsi + 112], 17
    cmp ecx, 16
    jb run_sieve_dense_restore_063
    btr qword [rsi + 120], 16
    cmp ecx, 17
    jb run_sieve_dense_restore_063
    btr qword [rsi + 128], 15
    cmp ecx, 18
    jb run_sieve_dense_restore_063
    btr qword [rsi + 136], 14
    cmp ecx, 19
    jb run_sieve_dense_restore_063
    btr qword [rsi + 144], 13
    cmp ecx, 20
    jb run_sieve_dense_restore_063
    btr qword [rsi + 152], 12
    cmp ecx, 21
    jb run_sieve_dense_restore_063
    btr qword [rsi + 160], 11
    cmp ecx, 22
    jb run_sieve_dense_restore_063
    btr qword [rsi + 168], 10
    cmp ecx, 23
    jb run_sieve_dense_restore_063
    btr qword [rsi + 176], 9
    cmp ecx, 24
    jb run_sieve_dense_restore_063
    btr qword [rsi + 184], 8
    cmp ecx, 25
    jb run_sieve_dense_restore_063
    btr qword [rsi + 192], 7
    cmp ecx, 26
    jb run_sieve_dense_restore_063
    btr qword [rsi + 200], 6
    cmp ecx, 27
    jb run_sieve_dense_restore_063
    btr qword [rsi + 208], 5
    cmp ecx, 28
    jb run_sieve_dense_restore_063
    btr qword [rsi + 216], 4
    cmp ecx, 29
    jb run_sieve_dense_restore_063
    btr qword [rsi + 224], 3
    cmp ecx, 30
    jb run_sieve_dense_restore_063
    btr qword [rsi + 232], 2
    cmp ecx, 31
    jb run_sieve_dense_restore_063
    btr qword [rsi + 240], 1
    cmp ecx, 32
    jb run_sieve_dense_restore_063
    mov rax, qword [rsi + 248]
    btr rax, 0
    btr rax, 63
    mov qword [rsi + 248], rax
    cmp ecx, 33
    jb run_sieve_dense_restore_063
    btr qword [rsi + 256], 62
    cmp ecx, 34
    jb run_sieve_dense_restore_063
    btr qword [rsi + 264], 61
    cmp ecx, 35
    jb run_sieve_dense_restore_063
    btr qword [rsi + 272], 60
    cmp ecx, 36
    jb run_sieve_dense_restore_063
    btr qword [rsi + 280], 59
    cmp ecx, 37
    jb run_sieve_dense_restore_063
    btr qword [rsi + 288], 58
    cmp ecx, 38
    jb run_sieve_dense_restore_063
    btr qword [rsi + 296], 57
    cmp ecx, 39
    jb run_sieve_dense_restore_063
    btr qword [rsi + 304], 56
    cmp ecx, 40
    jb run_sieve_dense_restore_063
    btr qword [rsi + 312], 55
    cmp ecx, 41
    jb run_sieve_dense_restore_063
    btr qword [rsi + 320], 54
    cmp ecx, 42
    jb run_sieve_dense_restore_063
    btr qword [rsi + 328], 53
    cmp ecx, 43
    jb run_sieve_dense_restore_063
    btr qword [rsi + 336], 52
    cmp ecx, 44
    jb run_sieve_dense_restore_063
    btr qword [rsi + 344], 51
    cmp ecx, 45
    jb run_sieve_dense_restore_063
    btr qword [rsi + 352], 50
    cmp ecx, 46
    jb run_sieve_dense_restore_063
    btr qword [rsi + 360], 49
    cmp ecx, 47
    jb run_sieve_dense_restore_063
    btr qword [rsi + 368], 48
    cmp ecx, 48
    jb run_sieve_dense_restore_063
    btr qword [rsi + 376], 47
    cmp ecx, 49
    jb run_sieve_dense_restore_063
    btr qword [rsi + 384], 46
    cmp ecx, 50
    jb run_sieve_dense_restore_063
    btr qword [rsi + 392], 45
    cmp ecx, 51
    jb run_sieve_dense_restore_063
    btr qword [rsi + 400], 44
    cmp ecx, 52
    jb run_sieve_dense_restore_063
    btr qword [rsi + 408], 43
    cmp ecx, 53
    jb run_sieve_dense_restore_063
    btr qword [rsi + 416], 42
    cmp ecx, 54
    jb run_sieve_dense_restore_063
    btr qword [rsi + 424], 41
    cmp ecx, 55
    jb run_sieve_dense_restore_063
    btr qword [rsi + 432], 40
    cmp ecx, 56
    jb run_sieve_dense_restore_063
    btr qword [rsi + 440], 39
    cmp ecx, 57
    jb run_sieve_dense_restore_063
    btr qword [rsi + 448], 38
    cmp ecx, 58
    jb run_sieve_dense_restore_063
    btr qword [rsi + 456], 37
    cmp ecx, 59
    jb run_sieve_dense_restore_063
    btr qword [rsi + 464], 36
    cmp ecx, 60
    jb run_sieve_dense_restore_063
    btr qword [rsi + 472], 35
    cmp ecx, 61
    jb run_sieve_dense_restore_063
    btr qword [rsi + 480], 34
    cmp ecx, 62
    jb run_sieve_dense_restore_063
    btr qword [rsi + 488], 33
    cmp ecx, 63
    jb run_sieve_dense_restore_063
    btr qword [rsi + 496], 32
run_sieve_dense_restore_063:
    bts qword [r14], 31
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_065:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 65
    jb run_sieve_dense_tail_065
align 16
run_sieve_dense_loop_065:
    btr qword [rsi], 32
    btr qword [rsi + 8], 33
    btr qword [rsi + 16], 34
    btr qword [rsi + 24], 35
    btr qword [rsi + 32], 36
    btr qword [rsi + 40], 37
    btr qword [rsi + 48], 38
    btr qword [rsi + 56], 39
    btr qword [rsi + 64], 40
    btr qword [rsi + 72], 41
    btr qword [rsi + 80], 42
    btr qword [rsi + 88], 43
    btr qword [rsi + 96], 44
    btr qword [rsi + 104], 45
    btr qword [rsi + 112], 46
    btr qword [rsi + 120], 47
    btr qword [rsi + 128], 48
    btr qword [rsi + 136], 49
    btr qword [rsi + 144], 50
    btr qword [rsi + 152], 51
    btr qword [rsi + 160], 52
    btr qword [rsi + 168], 53
    btr qword [rsi + 176], 54
    btr qword [rsi + 184], 55
    btr qword [rsi + 192], 56
    btr qword [rsi + 200], 57
    btr qword [rsi + 208], 58
    btr qword [rsi + 216], 59
    btr qword [rsi + 224], 60
    btr qword [rsi + 232], 61
    btr qword [rsi + 240], 62
    btr qword [rsi + 248], 63
    btr qword [rsi + 264], 0
    btr qword [rsi + 272], 1
    btr qword [rsi + 280], 2
    btr qword [rsi + 288], 3
    btr qword [rsi + 296], 4
    btr qword [rsi + 304], 5
    btr qword [rsi + 312], 6
    btr qword [rsi + 320], 7
    btr qword [rsi + 328], 8
    btr qword [rsi + 336], 9
    btr qword [rsi + 344], 10
    btr qword [rsi + 352], 11
    btr qword [rsi + 360], 12
    btr qword [rsi + 368], 13
    btr qword [rsi + 376], 14
    btr qword [rsi + 384], 15
    btr qword [rsi + 392], 16
    btr qword [rsi + 400], 17
    btr qword [rsi + 408], 18
    btr qword [rsi + 416], 19
    btr qword [rsi + 424], 20
    btr qword [rsi + 432], 21
    btr qword [rsi + 440], 22
    btr qword [rsi + 448], 23
    btr qword [rsi + 456], 24
    btr qword [rsi + 464], 25
    btr qword [rsi + 472], 26
    btr qword [rsi + 480], 27
    btr qword [rsi + 488], 28
    btr qword [rsi + 496], 29
    btr qword [rsi + 504], 30
    btr qword [rsi + 512], 31
    add rsi, 520
    sub ecx, 65
    cmp ecx, 65
    jae run_sieve_dense_loop_065
run_sieve_dense_tail_065:
    test ecx, ecx
    jz run_sieve_dense_restore_065
    cmp ecx, 1
    jb run_sieve_dense_restore_065
    btr qword [rsi], 32
    cmp ecx, 2
    jb run_sieve_dense_restore_065
    btr qword [rsi + 8], 33
    cmp ecx, 3
    jb run_sieve_dense_restore_065
    btr qword [rsi + 16], 34
    cmp ecx, 4
    jb run_sieve_dense_restore_065
    btr qword [rsi + 24], 35
    cmp ecx, 5
    jb run_sieve_dense_restore_065
    btr qword [rsi + 32], 36
    cmp ecx, 6
    jb run_sieve_dense_restore_065
    btr qword [rsi + 40], 37
    cmp ecx, 7
    jb run_sieve_dense_restore_065
    btr qword [rsi + 48], 38
    cmp ecx, 8
    jb run_sieve_dense_restore_065
    btr qword [rsi + 56], 39
    cmp ecx, 9
    jb run_sieve_dense_restore_065
    btr qword [rsi + 64], 40
    cmp ecx, 10
    jb run_sieve_dense_restore_065
    btr qword [rsi + 72], 41
    cmp ecx, 11
    jb run_sieve_dense_restore_065
    btr qword [rsi + 80], 42
    cmp ecx, 12
    jb run_sieve_dense_restore_065
    btr qword [rsi + 88], 43
    cmp ecx, 13
    jb run_sieve_dense_restore_065
    btr qword [rsi + 96], 44
    cmp ecx, 14
    jb run_sieve_dense_restore_065
    btr qword [rsi + 104], 45
    cmp ecx, 15
    jb run_sieve_dense_restore_065
    btr qword [rsi + 112], 46
    cmp ecx, 16
    jb run_sieve_dense_restore_065
    btr qword [rsi + 120], 47
    cmp ecx, 17
    jb run_sieve_dense_restore_065
    btr qword [rsi + 128], 48
    cmp ecx, 18
    jb run_sieve_dense_restore_065
    btr qword [rsi + 136], 49
    cmp ecx, 19
    jb run_sieve_dense_restore_065
    btr qword [rsi + 144], 50
    cmp ecx, 20
    jb run_sieve_dense_restore_065
    btr qword [rsi + 152], 51
    cmp ecx, 21
    jb run_sieve_dense_restore_065
    btr qword [rsi + 160], 52
    cmp ecx, 22
    jb run_sieve_dense_restore_065
    btr qword [rsi + 168], 53
    cmp ecx, 23
    jb run_sieve_dense_restore_065
    btr qword [rsi + 176], 54
    cmp ecx, 24
    jb run_sieve_dense_restore_065
    btr qword [rsi + 184], 55
    cmp ecx, 25
    jb run_sieve_dense_restore_065
    btr qword [rsi + 192], 56
    cmp ecx, 26
    jb run_sieve_dense_restore_065
    btr qword [rsi + 200], 57
    cmp ecx, 27
    jb run_sieve_dense_restore_065
    btr qword [rsi + 208], 58
    cmp ecx, 28
    jb run_sieve_dense_restore_065
    btr qword [rsi + 216], 59
    cmp ecx, 29
    jb run_sieve_dense_restore_065
    btr qword [rsi + 224], 60
    cmp ecx, 30
    jb run_sieve_dense_restore_065
    btr qword [rsi + 232], 61
    cmp ecx, 31
    jb run_sieve_dense_restore_065
    btr qword [rsi + 240], 62
    cmp ecx, 32
    jb run_sieve_dense_restore_065
    btr qword [rsi + 248], 63
    cmp ecx, 34
    jb run_sieve_dense_restore_065
    btr qword [rsi + 264], 0
    cmp ecx, 35
    jb run_sieve_dense_restore_065
    btr qword [rsi + 272], 1
    cmp ecx, 36
    jb run_sieve_dense_restore_065
    btr qword [rsi + 280], 2
    cmp ecx, 37
    jb run_sieve_dense_restore_065
    btr qword [rsi + 288], 3
    cmp ecx, 38
    jb run_sieve_dense_restore_065
    btr qword [rsi + 296], 4
    cmp ecx, 39
    jb run_sieve_dense_restore_065
    btr qword [rsi + 304], 5
    cmp ecx, 40
    jb run_sieve_dense_restore_065
    btr qword [rsi + 312], 6
    cmp ecx, 41
    jb run_sieve_dense_restore_065
    btr qword [rsi + 320], 7
    cmp ecx, 42
    jb run_sieve_dense_restore_065
    btr qword [rsi + 328], 8
    cmp ecx, 43
    jb run_sieve_dense_restore_065
    btr qword [rsi + 336], 9
    cmp ecx, 44
    jb run_sieve_dense_restore_065
    btr qword [rsi + 344], 10
    cmp ecx, 45
    jb run_sieve_dense_restore_065
    btr qword [rsi + 352], 11
    cmp ecx, 46
    jb run_sieve_dense_restore_065
    btr qword [rsi + 360], 12
    cmp ecx, 47
    jb run_sieve_dense_restore_065
    btr qword [rsi + 368], 13
    cmp ecx, 48
    jb run_sieve_dense_restore_065
    btr qword [rsi + 376], 14
    cmp ecx, 49
    jb run_sieve_dense_restore_065
    btr qword [rsi + 384], 15
    cmp ecx, 50
    jb run_sieve_dense_restore_065
    btr qword [rsi + 392], 16
    cmp ecx, 51
    jb run_sieve_dense_restore_065
    btr qword [rsi + 400], 17
    cmp ecx, 52
    jb run_sieve_dense_restore_065
    btr qword [rsi + 408], 18
    cmp ecx, 53
    jb run_sieve_dense_restore_065
    btr qword [rsi + 416], 19
    cmp ecx, 54
    jb run_sieve_dense_restore_065
    btr qword [rsi + 424], 20
    cmp ecx, 55
    jb run_sieve_dense_restore_065
    btr qword [rsi + 432], 21
    cmp ecx, 56
    jb run_sieve_dense_restore_065
    btr qword [rsi + 440], 22
    cmp ecx, 57
    jb run_sieve_dense_restore_065
    btr qword [rsi + 448], 23
    cmp ecx, 58
    jb run_sieve_dense_restore_065
    btr qword [rsi + 456], 24
    cmp ecx, 59
    jb run_sieve_dense_restore_065
    btr qword [rsi + 464], 25
    cmp ecx, 60
    jb run_sieve_dense_restore_065
    btr qword [rsi + 472], 26
    cmp ecx, 61
    jb run_sieve_dense_restore_065
    btr qword [rsi + 480], 27
    cmp ecx, 62
    jb run_sieve_dense_restore_065
    btr qword [rsi + 488], 28
    cmp ecx, 63
    jb run_sieve_dense_restore_065
    btr qword [rsi + 496], 29
    cmp ecx, 64
    jb run_sieve_dense_restore_065
    btr qword [rsi + 504], 30
    cmp ecx, 65
    jb run_sieve_dense_restore_065
    btr qword [rsi + 512], 31
run_sieve_dense_restore_065:
    bts qword [r14], 32
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_067:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 67
    jb run_sieve_dense_tail_067
align 16
run_sieve_dense_loop_067:
    btr qword [rsi], 33
    btr qword [rsi + 8], 36
    btr qword [rsi + 16], 39
    btr qword [rsi + 24], 42
    btr qword [rsi + 32], 45
    btr qword [rsi + 40], 48
    btr qword [rsi + 48], 51
    btr qword [rsi + 56], 54
    btr qword [rsi + 64], 57
    btr qword [rsi + 72], 60
    btr qword [rsi + 80], 63
    btr qword [rsi + 96], 2
    btr qword [rsi + 104], 5
    btr qword [rsi + 112], 8
    btr qword [rsi + 120], 11
    btr qword [rsi + 128], 14
    btr qword [rsi + 136], 17
    btr qword [rsi + 144], 20
    btr qword [rsi + 152], 23
    btr qword [rsi + 160], 26
    btr qword [rsi + 168], 29
    btr qword [rsi + 176], 32
    btr qword [rsi + 184], 35
    btr qword [rsi + 192], 38
    btr qword [rsi + 200], 41
    btr qword [rsi + 208], 44
    btr qword [rsi + 216], 47
    btr qword [rsi + 224], 50
    btr qword [rsi + 232], 53
    btr qword [rsi + 240], 56
    btr qword [rsi + 248], 59
    btr qword [rsi + 256], 62
    btr qword [rsi + 272], 1
    btr qword [rsi + 280], 4
    btr qword [rsi + 288], 7
    btr qword [rsi + 296], 10
    btr qword [rsi + 304], 13
    btr qword [rsi + 312], 16
    btr qword [rsi + 320], 19
    btr qword [rsi + 328], 22
    btr qword [rsi + 336], 25
    btr qword [rsi + 344], 28
    btr qword [rsi + 352], 31
    btr qword [rsi + 360], 34
    btr qword [rsi + 368], 37
    btr qword [rsi + 376], 40
    btr qword [rsi + 384], 43
    btr qword [rsi + 392], 46
    btr qword [rsi + 400], 49
    btr qword [rsi + 408], 52
    btr qword [rsi + 416], 55
    btr qword [rsi + 424], 58
    btr qword [rsi + 432], 61
    btr qword [rsi + 448], 0
    btr qword [rsi + 456], 3
    btr qword [rsi + 464], 6
    btr qword [rsi + 472], 9
    btr qword [rsi + 480], 12
    btr qword [rsi + 488], 15
    btr qword [rsi + 496], 18
    btr qword [rsi + 504], 21
    btr qword [rsi + 512], 24
    btr qword [rsi + 520], 27
    btr qword [rsi + 528], 30
    add rsi, 536
    sub ecx, 67
    cmp ecx, 67
    jae run_sieve_dense_loop_067
run_sieve_dense_tail_067:
    test ecx, ecx
    jz run_sieve_dense_restore_067
    cmp ecx, 1
    jb run_sieve_dense_restore_067
    btr qword [rsi], 33
    cmp ecx, 2
    jb run_sieve_dense_restore_067
    btr qword [rsi + 8], 36
    cmp ecx, 3
    jb run_sieve_dense_restore_067
    btr qword [rsi + 16], 39
    cmp ecx, 4
    jb run_sieve_dense_restore_067
    btr qword [rsi + 24], 42
    cmp ecx, 5
    jb run_sieve_dense_restore_067
    btr qword [rsi + 32], 45
    cmp ecx, 6
    jb run_sieve_dense_restore_067
    btr qword [rsi + 40], 48
    cmp ecx, 7
    jb run_sieve_dense_restore_067
    btr qword [rsi + 48], 51
    cmp ecx, 8
    jb run_sieve_dense_restore_067
    btr qword [rsi + 56], 54
    cmp ecx, 9
    jb run_sieve_dense_restore_067
    btr qword [rsi + 64], 57
    cmp ecx, 10
    jb run_sieve_dense_restore_067
    btr qword [rsi + 72], 60
    cmp ecx, 11
    jb run_sieve_dense_restore_067
    btr qword [rsi + 80], 63
    cmp ecx, 13
    jb run_sieve_dense_restore_067
    btr qword [rsi + 96], 2
    cmp ecx, 14
    jb run_sieve_dense_restore_067
    btr qword [rsi + 104], 5
    cmp ecx, 15
    jb run_sieve_dense_restore_067
    btr qword [rsi + 112], 8
    cmp ecx, 16
    jb run_sieve_dense_restore_067
    btr qword [rsi + 120], 11
    cmp ecx, 17
    jb run_sieve_dense_restore_067
    btr qword [rsi + 128], 14
    cmp ecx, 18
    jb run_sieve_dense_restore_067
    btr qword [rsi + 136], 17
    cmp ecx, 19
    jb run_sieve_dense_restore_067
    btr qword [rsi + 144], 20
    cmp ecx, 20
    jb run_sieve_dense_restore_067
    btr qword [rsi + 152], 23
    cmp ecx, 21
    jb run_sieve_dense_restore_067
    btr qword [rsi + 160], 26
    cmp ecx, 22
    jb run_sieve_dense_restore_067
    btr qword [rsi + 168], 29
    cmp ecx, 23
    jb run_sieve_dense_restore_067
    btr qword [rsi + 176], 32
    cmp ecx, 24
    jb run_sieve_dense_restore_067
    btr qword [rsi + 184], 35
    cmp ecx, 25
    jb run_sieve_dense_restore_067
    btr qword [rsi + 192], 38
    cmp ecx, 26
    jb run_sieve_dense_restore_067
    btr qword [rsi + 200], 41
    cmp ecx, 27
    jb run_sieve_dense_restore_067
    btr qword [rsi + 208], 44
    cmp ecx, 28
    jb run_sieve_dense_restore_067
    btr qword [rsi + 216], 47
    cmp ecx, 29
    jb run_sieve_dense_restore_067
    btr qword [rsi + 224], 50
    cmp ecx, 30
    jb run_sieve_dense_restore_067
    btr qword [rsi + 232], 53
    cmp ecx, 31
    jb run_sieve_dense_restore_067
    btr qword [rsi + 240], 56
    cmp ecx, 32
    jb run_sieve_dense_restore_067
    btr qword [rsi + 248], 59
    cmp ecx, 33
    jb run_sieve_dense_restore_067
    btr qword [rsi + 256], 62
    cmp ecx, 35
    jb run_sieve_dense_restore_067
    btr qword [rsi + 272], 1
    cmp ecx, 36
    jb run_sieve_dense_restore_067
    btr qword [rsi + 280], 4
    cmp ecx, 37
    jb run_sieve_dense_restore_067
    btr qword [rsi + 288], 7
    cmp ecx, 38
    jb run_sieve_dense_restore_067
    btr qword [rsi + 296], 10
    cmp ecx, 39
    jb run_sieve_dense_restore_067
    btr qword [rsi + 304], 13
    cmp ecx, 40
    jb run_sieve_dense_restore_067
    btr qword [rsi + 312], 16
    cmp ecx, 41
    jb run_sieve_dense_restore_067
    btr qword [rsi + 320], 19
    cmp ecx, 42
    jb run_sieve_dense_restore_067
    btr qword [rsi + 328], 22
    cmp ecx, 43
    jb run_sieve_dense_restore_067
    btr qword [rsi + 336], 25
    cmp ecx, 44
    jb run_sieve_dense_restore_067
    btr qword [rsi + 344], 28
    cmp ecx, 45
    jb run_sieve_dense_restore_067
    btr qword [rsi + 352], 31
    cmp ecx, 46
    jb run_sieve_dense_restore_067
    btr qword [rsi + 360], 34
    cmp ecx, 47
    jb run_sieve_dense_restore_067
    btr qword [rsi + 368], 37
    cmp ecx, 48
    jb run_sieve_dense_restore_067
    btr qword [rsi + 376], 40
    cmp ecx, 49
    jb run_sieve_dense_restore_067
    btr qword [rsi + 384], 43
    cmp ecx, 50
    jb run_sieve_dense_restore_067
    btr qword [rsi + 392], 46
    cmp ecx, 51
    jb run_sieve_dense_restore_067
    btr qword [rsi + 400], 49
    cmp ecx, 52
    jb run_sieve_dense_restore_067
    btr qword [rsi + 408], 52
    cmp ecx, 53
    jb run_sieve_dense_restore_067
    btr qword [rsi + 416], 55
    cmp ecx, 54
    jb run_sieve_dense_restore_067
    btr qword [rsi + 424], 58
    cmp ecx, 55
    jb run_sieve_dense_restore_067
    btr qword [rsi + 432], 61
    cmp ecx, 57
    jb run_sieve_dense_restore_067
    btr qword [rsi + 448], 0
    cmp ecx, 58
    jb run_sieve_dense_restore_067
    btr qword [rsi + 456], 3
    cmp ecx, 59
    jb run_sieve_dense_restore_067
    btr qword [rsi + 464], 6
    cmp ecx, 60
    jb run_sieve_dense_restore_067
    btr qword [rsi + 472], 9
    cmp ecx, 61
    jb run_sieve_dense_restore_067
    btr qword [rsi + 480], 12
    cmp ecx, 62
    jb run_sieve_dense_restore_067
    btr qword [rsi + 488], 15
    cmp ecx, 63
    jb run_sieve_dense_restore_067
    btr qword [rsi + 496], 18
    cmp ecx, 64
    jb run_sieve_dense_restore_067
    btr qword [rsi + 504], 21
    cmp ecx, 65
    jb run_sieve_dense_restore_067
    btr qword [rsi + 512], 24
    cmp ecx, 66
    jb run_sieve_dense_restore_067
    btr qword [rsi + 520], 27
    cmp ecx, 67
    jb run_sieve_dense_restore_067
    btr qword [rsi + 528], 30
run_sieve_dense_restore_067:
    bts qword [r14], 33
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_069:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 69
    jb run_sieve_dense_tail_069
align 16
run_sieve_dense_loop_069:
    btr qword [rsi], 34
    btr qword [rsi + 8], 39
    btr qword [rsi + 16], 44
    btr qword [rsi + 24], 49
    btr qword [rsi + 32], 54
    btr qword [rsi + 40], 59
    btr qword [rsi + 56], 0
    btr qword [rsi + 64], 5
    btr qword [rsi + 72], 10
    btr qword [rsi + 80], 15
    btr qword [rsi + 88], 20
    btr qword [rsi + 96], 25
    btr qword [rsi + 104], 30
    btr qword [rsi + 112], 35
    btr qword [rsi + 120], 40
    btr qword [rsi + 128], 45
    btr qword [rsi + 136], 50
    btr qword [rsi + 144], 55
    btr qword [rsi + 152], 60
    btr qword [rsi + 168], 1
    btr qword [rsi + 176], 6
    btr qword [rsi + 184], 11
    btr qword [rsi + 192], 16
    btr qword [rsi + 200], 21
    btr qword [rsi + 208], 26
    btr qword [rsi + 216], 31
    btr qword [rsi + 224], 36
    btr qword [rsi + 232], 41
    btr qword [rsi + 240], 46
    btr qword [rsi + 248], 51
    btr qword [rsi + 256], 56
    btr qword [rsi + 264], 61
    btr qword [rsi + 280], 2
    btr qword [rsi + 288], 7
    btr qword [rsi + 296], 12
    btr qword [rsi + 304], 17
    btr qword [rsi + 312], 22
    btr qword [rsi + 320], 27
    btr qword [rsi + 328], 32
    btr qword [rsi + 336], 37
    btr qword [rsi + 344], 42
    btr qword [rsi + 352], 47
    btr qword [rsi + 360], 52
    btr qword [rsi + 368], 57
    btr qword [rsi + 376], 62
    btr qword [rsi + 392], 3
    btr qword [rsi + 400], 8
    btr qword [rsi + 408], 13
    btr qword [rsi + 416], 18
    btr qword [rsi + 424], 23
    btr qword [rsi + 432], 28
    btr qword [rsi + 440], 33
    btr qword [rsi + 448], 38
    btr qword [rsi + 456], 43
    btr qword [rsi + 464], 48
    btr qword [rsi + 472], 53
    btr qword [rsi + 480], 58
    btr qword [rsi + 488], 63
    btr qword [rsi + 504], 4
    btr qword [rsi + 512], 9
    btr qword [rsi + 520], 14
    btr qword [rsi + 528], 19
    btr qword [rsi + 536], 24
    btr qword [rsi + 544], 29
    add rsi, 552
    sub ecx, 69
    cmp ecx, 69
    jae run_sieve_dense_loop_069
run_sieve_dense_tail_069:
    test ecx, ecx
    jz run_sieve_dense_restore_069
    cmp ecx, 1
    jb run_sieve_dense_restore_069
    btr qword [rsi], 34
    cmp ecx, 2
    jb run_sieve_dense_restore_069
    btr qword [rsi + 8], 39
    cmp ecx, 3
    jb run_sieve_dense_restore_069
    btr qword [rsi + 16], 44
    cmp ecx, 4
    jb run_sieve_dense_restore_069
    btr qword [rsi + 24], 49
    cmp ecx, 5
    jb run_sieve_dense_restore_069
    btr qword [rsi + 32], 54
    cmp ecx, 6
    jb run_sieve_dense_restore_069
    btr qword [rsi + 40], 59
    cmp ecx, 8
    jb run_sieve_dense_restore_069
    btr qword [rsi + 56], 0
    cmp ecx, 9
    jb run_sieve_dense_restore_069
    btr qword [rsi + 64], 5
    cmp ecx, 10
    jb run_sieve_dense_restore_069
    btr qword [rsi + 72], 10
    cmp ecx, 11
    jb run_sieve_dense_restore_069
    btr qword [rsi + 80], 15
    cmp ecx, 12
    jb run_sieve_dense_restore_069
    btr qword [rsi + 88], 20
    cmp ecx, 13
    jb run_sieve_dense_restore_069
    btr qword [rsi + 96], 25
    cmp ecx, 14
    jb run_sieve_dense_restore_069
    btr qword [rsi + 104], 30
    cmp ecx, 15
    jb run_sieve_dense_restore_069
    btr qword [rsi + 112], 35
    cmp ecx, 16
    jb run_sieve_dense_restore_069
    btr qword [rsi + 120], 40
    cmp ecx, 17
    jb run_sieve_dense_restore_069
    btr qword [rsi + 128], 45
    cmp ecx, 18
    jb run_sieve_dense_restore_069
    btr qword [rsi + 136], 50
    cmp ecx, 19
    jb run_sieve_dense_restore_069
    btr qword [rsi + 144], 55
    cmp ecx, 20
    jb run_sieve_dense_restore_069
    btr qword [rsi + 152], 60
    cmp ecx, 22
    jb run_sieve_dense_restore_069
    btr qword [rsi + 168], 1
    cmp ecx, 23
    jb run_sieve_dense_restore_069
    btr qword [rsi + 176], 6
    cmp ecx, 24
    jb run_sieve_dense_restore_069
    btr qword [rsi + 184], 11
    cmp ecx, 25
    jb run_sieve_dense_restore_069
    btr qword [rsi + 192], 16
    cmp ecx, 26
    jb run_sieve_dense_restore_069
    btr qword [rsi + 200], 21
    cmp ecx, 27
    jb run_sieve_dense_restore_069
    btr qword [rsi + 208], 26
    cmp ecx, 28
    jb run_sieve_dense_restore_069
    btr qword [rsi + 216], 31
    cmp ecx, 29
    jb run_sieve_dense_restore_069
    btr qword [rsi + 224], 36
    cmp ecx, 30
    jb run_sieve_dense_restore_069
    btr qword [rsi + 232], 41
    cmp ecx, 31
    jb run_sieve_dense_restore_069
    btr qword [rsi + 240], 46
    cmp ecx, 32
    jb run_sieve_dense_restore_069
    btr qword [rsi + 248], 51
    cmp ecx, 33
    jb run_sieve_dense_restore_069
    btr qword [rsi + 256], 56
    cmp ecx, 34
    jb run_sieve_dense_restore_069
    btr qword [rsi + 264], 61
    cmp ecx, 36
    jb run_sieve_dense_restore_069
    btr qword [rsi + 280], 2
    cmp ecx, 37
    jb run_sieve_dense_restore_069
    btr qword [rsi + 288], 7
    cmp ecx, 38
    jb run_sieve_dense_restore_069
    btr qword [rsi + 296], 12
    cmp ecx, 39
    jb run_sieve_dense_restore_069
    btr qword [rsi + 304], 17
    cmp ecx, 40
    jb run_sieve_dense_restore_069
    btr qword [rsi + 312], 22
    cmp ecx, 41
    jb run_sieve_dense_restore_069
    btr qword [rsi + 320], 27
    cmp ecx, 42
    jb run_sieve_dense_restore_069
    btr qword [rsi + 328], 32
    cmp ecx, 43
    jb run_sieve_dense_restore_069
    btr qword [rsi + 336], 37
    cmp ecx, 44
    jb run_sieve_dense_restore_069
    btr qword [rsi + 344], 42
    cmp ecx, 45
    jb run_sieve_dense_restore_069
    btr qword [rsi + 352], 47
    cmp ecx, 46
    jb run_sieve_dense_restore_069
    btr qword [rsi + 360], 52
    cmp ecx, 47
    jb run_sieve_dense_restore_069
    btr qword [rsi + 368], 57
    cmp ecx, 48
    jb run_sieve_dense_restore_069
    btr qword [rsi + 376], 62
    cmp ecx, 50
    jb run_sieve_dense_restore_069
    btr qword [rsi + 392], 3
    cmp ecx, 51
    jb run_sieve_dense_restore_069
    btr qword [rsi + 400], 8
    cmp ecx, 52
    jb run_sieve_dense_restore_069
    btr qword [rsi + 408], 13
    cmp ecx, 53
    jb run_sieve_dense_restore_069
    btr qword [rsi + 416], 18
    cmp ecx, 54
    jb run_sieve_dense_restore_069
    btr qword [rsi + 424], 23
    cmp ecx, 55
    jb run_sieve_dense_restore_069
    btr qword [rsi + 432], 28
    cmp ecx, 56
    jb run_sieve_dense_restore_069
    btr qword [rsi + 440], 33
    cmp ecx, 57
    jb run_sieve_dense_restore_069
    btr qword [rsi + 448], 38
    cmp ecx, 58
    jb run_sieve_dense_restore_069
    btr qword [rsi + 456], 43
    cmp ecx, 59
    jb run_sieve_dense_restore_069
    btr qword [rsi + 464], 48
    cmp ecx, 60
    jb run_sieve_dense_restore_069
    btr qword [rsi + 472], 53
    cmp ecx, 61
    jb run_sieve_dense_restore_069
    btr qword [rsi + 480], 58
    cmp ecx, 62
    jb run_sieve_dense_restore_069
    btr qword [rsi + 488], 63
    cmp ecx, 64
    jb run_sieve_dense_restore_069
    btr qword [rsi + 504], 4
    cmp ecx, 65
    jb run_sieve_dense_restore_069
    btr qword [rsi + 512], 9
    cmp ecx, 66
    jb run_sieve_dense_restore_069
    btr qword [rsi + 520], 14
    cmp ecx, 67
    jb run_sieve_dense_restore_069
    btr qword [rsi + 528], 19
    cmp ecx, 68
    jb run_sieve_dense_restore_069
    btr qword [rsi + 536], 24
    cmp ecx, 69
    jb run_sieve_dense_restore_069
    btr qword [rsi + 544], 29
run_sieve_dense_restore_069:
    bts qword [r14], 34
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_071:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 71
    jb run_sieve_dense_tail_071
align 16
run_sieve_dense_loop_071:
    btr qword [rsi], 35
    btr qword [rsi + 8], 42
    btr qword [rsi + 16], 49
    btr qword [rsi + 24], 56
    btr qword [rsi + 32], 63
    btr qword [rsi + 48], 6
    btr qword [rsi + 56], 13
    btr qword [rsi + 64], 20
    btr qword [rsi + 72], 27
    btr qword [rsi + 80], 34
    btr qword [rsi + 88], 41
    btr qword [rsi + 96], 48
    btr qword [rsi + 104], 55
    btr qword [rsi + 112], 62
    btr qword [rsi + 128], 5
    btr qword [rsi + 136], 12
    btr qword [rsi + 144], 19
    btr qword [rsi + 152], 26
    btr qword [rsi + 160], 33
    btr qword [rsi + 168], 40
    btr qword [rsi + 176], 47
    btr qword [rsi + 184], 54
    btr qword [rsi + 192], 61
    btr qword [rsi + 208], 4
    btr qword [rsi + 216], 11
    btr qword [rsi + 224], 18
    btr qword [rsi + 232], 25
    btr qword [rsi + 240], 32
    btr qword [rsi + 248], 39
    btr qword [rsi + 256], 46
    btr qword [rsi + 264], 53
    btr qword [rsi + 272], 60
    btr qword [rsi + 288], 3
    btr qword [rsi + 296], 10
    btr qword [rsi + 304], 17
    btr qword [rsi + 312], 24
    btr qword [rsi + 320], 31
    btr qword [rsi + 328], 38
    btr qword [rsi + 336], 45
    btr qword [rsi + 344], 52
    btr qword [rsi + 352], 59
    btr qword [rsi + 368], 2
    btr qword [rsi + 376], 9
    btr qword [rsi + 384], 16
    btr qword [rsi + 392], 23
    btr qword [rsi + 400], 30
    btr qword [rsi + 408], 37
    btr qword [rsi + 416], 44
    btr qword [rsi + 424], 51
    btr qword [rsi + 432], 58
    btr qword [rsi + 448], 1
    btr qword [rsi + 456], 8
    btr qword [rsi + 464], 15
    btr qword [rsi + 472], 22
    btr qword [rsi + 480], 29
    btr qword [rsi + 488], 36
    btr qword [rsi + 496], 43
    btr qword [rsi + 504], 50
    btr qword [rsi + 512], 57
    btr qword [rsi + 528], 0
    btr qword [rsi + 536], 7
    btr qword [rsi + 544], 14
    btr qword [rsi + 552], 21
    btr qword [rsi + 560], 28
    add rsi, 568
    sub ecx, 71
    cmp ecx, 71
    jae run_sieve_dense_loop_071
run_sieve_dense_tail_071:
    test ecx, ecx
    jz run_sieve_dense_restore_071
    cmp ecx, 1
    jb run_sieve_dense_restore_071
    btr qword [rsi], 35
    cmp ecx, 2
    jb run_sieve_dense_restore_071
    btr qword [rsi + 8], 42
    cmp ecx, 3
    jb run_sieve_dense_restore_071
    btr qword [rsi + 16], 49
    cmp ecx, 4
    jb run_sieve_dense_restore_071
    btr qword [rsi + 24], 56
    cmp ecx, 5
    jb run_sieve_dense_restore_071
    btr qword [rsi + 32], 63
    cmp ecx, 7
    jb run_sieve_dense_restore_071
    btr qword [rsi + 48], 6
    cmp ecx, 8
    jb run_sieve_dense_restore_071
    btr qword [rsi + 56], 13
    cmp ecx, 9
    jb run_sieve_dense_restore_071
    btr qword [rsi + 64], 20
    cmp ecx, 10
    jb run_sieve_dense_restore_071
    btr qword [rsi + 72], 27
    cmp ecx, 11
    jb run_sieve_dense_restore_071
    btr qword [rsi + 80], 34
    cmp ecx, 12
    jb run_sieve_dense_restore_071
    btr qword [rsi + 88], 41
    cmp ecx, 13
    jb run_sieve_dense_restore_071
    btr qword [rsi + 96], 48
    cmp ecx, 14
    jb run_sieve_dense_restore_071
    btr qword [rsi + 104], 55
    cmp ecx, 15
    jb run_sieve_dense_restore_071
    btr qword [rsi + 112], 62
    cmp ecx, 17
    jb run_sieve_dense_restore_071
    btr qword [rsi + 128], 5
    cmp ecx, 18
    jb run_sieve_dense_restore_071
    btr qword [rsi + 136], 12
    cmp ecx, 19
    jb run_sieve_dense_restore_071
    btr qword [rsi + 144], 19
    cmp ecx, 20
    jb run_sieve_dense_restore_071
    btr qword [rsi + 152], 26
    cmp ecx, 21
    jb run_sieve_dense_restore_071
    btr qword [rsi + 160], 33
    cmp ecx, 22
    jb run_sieve_dense_restore_071
    btr qword [rsi + 168], 40
    cmp ecx, 23
    jb run_sieve_dense_restore_071
    btr qword [rsi + 176], 47
    cmp ecx, 24
    jb run_sieve_dense_restore_071
    btr qword [rsi + 184], 54
    cmp ecx, 25
    jb run_sieve_dense_restore_071
    btr qword [rsi + 192], 61
    cmp ecx, 27
    jb run_sieve_dense_restore_071
    btr qword [rsi + 208], 4
    cmp ecx, 28
    jb run_sieve_dense_restore_071
    btr qword [rsi + 216], 11
    cmp ecx, 29
    jb run_sieve_dense_restore_071
    btr qword [rsi + 224], 18
    cmp ecx, 30
    jb run_sieve_dense_restore_071
    btr qword [rsi + 232], 25
    cmp ecx, 31
    jb run_sieve_dense_restore_071
    btr qword [rsi + 240], 32
    cmp ecx, 32
    jb run_sieve_dense_restore_071
    btr qword [rsi + 248], 39
    cmp ecx, 33
    jb run_sieve_dense_restore_071
    btr qword [rsi + 256], 46
    cmp ecx, 34
    jb run_sieve_dense_restore_071
    btr qword [rsi + 264], 53
    cmp ecx, 35
    jb run_sieve_dense_restore_071
    btr qword [rsi + 272], 60
    cmp ecx, 37
    jb run_sieve_dense_restore_071
    btr qword [rsi + 288], 3
    cmp ecx, 38
    jb run_sieve_dense_restore_071
    btr qword [rsi + 296], 10
    cmp ecx, 39
    jb run_sieve_dense_restore_071
    btr qword [rsi + 304], 17
    cmp ecx, 40
    jb run_sieve_dense_restore_071
    btr qword [rsi + 312], 24
    cmp ecx, 41
    jb run_sieve_dense_restore_071
    btr qword [rsi + 320], 31
    cmp ecx, 42
    jb run_sieve_dense_restore_071
    btr qword [rsi + 328], 38
    cmp ecx, 43
    jb run_sieve_dense_restore_071
    btr qword [rsi + 336], 45
    cmp ecx, 44
    jb run_sieve_dense_restore_071
    btr qword [rsi + 344], 52
    cmp ecx, 45
    jb run_sieve_dense_restore_071
    btr qword [rsi + 352], 59
    cmp ecx, 47
    jb run_sieve_dense_restore_071
    btr qword [rsi + 368], 2
    cmp ecx, 48
    jb run_sieve_dense_restore_071
    btr qword [rsi + 376], 9
    cmp ecx, 49
    jb run_sieve_dense_restore_071
    btr qword [rsi + 384], 16
    cmp ecx, 50
    jb run_sieve_dense_restore_071
    btr qword [rsi + 392], 23
    cmp ecx, 51
    jb run_sieve_dense_restore_071
    btr qword [rsi + 400], 30
    cmp ecx, 52
    jb run_sieve_dense_restore_071
    btr qword [rsi + 408], 37
    cmp ecx, 53
    jb run_sieve_dense_restore_071
    btr qword [rsi + 416], 44
    cmp ecx, 54
    jb run_sieve_dense_restore_071
    btr qword [rsi + 424], 51
    cmp ecx, 55
    jb run_sieve_dense_restore_071
    btr qword [rsi + 432], 58
    cmp ecx, 57
    jb run_sieve_dense_restore_071
    btr qword [rsi + 448], 1
    cmp ecx, 58
    jb run_sieve_dense_restore_071
    btr qword [rsi + 456], 8
    cmp ecx, 59
    jb run_sieve_dense_restore_071
    btr qword [rsi + 464], 15
    cmp ecx, 60
    jb run_sieve_dense_restore_071
    btr qword [rsi + 472], 22
    cmp ecx, 61
    jb run_sieve_dense_restore_071
    btr qword [rsi + 480], 29
    cmp ecx, 62
    jb run_sieve_dense_restore_071
    btr qword [rsi + 488], 36
    cmp ecx, 63
    jb run_sieve_dense_restore_071
    btr qword [rsi + 496], 43
    cmp ecx, 64
    jb run_sieve_dense_restore_071
    btr qword [rsi + 504], 50
    cmp ecx, 65
    jb run_sieve_dense_restore_071
    btr qword [rsi + 512], 57
    cmp ecx, 67
    jb run_sieve_dense_restore_071
    btr qword [rsi + 528], 0
    cmp ecx, 68
    jb run_sieve_dense_restore_071
    btr qword [rsi + 536], 7
    cmp ecx, 69
    jb run_sieve_dense_restore_071
    btr qword [rsi + 544], 14
    cmp ecx, 70
    jb run_sieve_dense_restore_071
    btr qword [rsi + 552], 21
    cmp ecx, 71
    jb run_sieve_dense_restore_071
    btr qword [rsi + 560], 28
run_sieve_dense_restore_071:
    bts qword [r14], 35
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_073:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 73
    jb run_sieve_dense_tail_073
align 16
run_sieve_dense_loop_073:
    btr qword [rsi], 36
    btr qword [rsi + 8], 45
    btr qword [rsi + 16], 54
    btr qword [rsi + 24], 63
    btr qword [rsi + 40], 8
    btr qword [rsi + 48], 17
    btr qword [rsi + 56], 26
    btr qword [rsi + 64], 35
    btr qword [rsi + 72], 44
    btr qword [rsi + 80], 53
    btr qword [rsi + 88], 62
    btr qword [rsi + 104], 7
    btr qword [rsi + 112], 16
    btr qword [rsi + 120], 25
    btr qword [rsi + 128], 34
    btr qword [rsi + 136], 43
    btr qword [rsi + 144], 52
    btr qword [rsi + 152], 61
    btr qword [rsi + 168], 6
    btr qword [rsi + 176], 15
    btr qword [rsi + 184], 24
    btr qword [rsi + 192], 33
    btr qword [rsi + 200], 42
    btr qword [rsi + 208], 51
    btr qword [rsi + 216], 60
    btr qword [rsi + 232], 5
    btr qword [rsi + 240], 14
    btr qword [rsi + 248], 23
    btr qword [rsi + 256], 32
    btr qword [rsi + 264], 41
    btr qword [rsi + 272], 50
    btr qword [rsi + 280], 59
    btr qword [rsi + 296], 4
    btr qword [rsi + 304], 13
    btr qword [rsi + 312], 22
    btr qword [rsi + 320], 31
    btr qword [rsi + 328], 40
    btr qword [rsi + 336], 49
    btr qword [rsi + 344], 58
    btr qword [rsi + 360], 3
    btr qword [rsi + 368], 12
    btr qword [rsi + 376], 21
    btr qword [rsi + 384], 30
    btr qword [rsi + 392], 39
    btr qword [rsi + 400], 48
    btr qword [rsi + 408], 57
    btr qword [rsi + 424], 2
    btr qword [rsi + 432], 11
    btr qword [rsi + 440], 20
    btr qword [rsi + 448], 29
    btr qword [rsi + 456], 38
    btr qword [rsi + 464], 47
    btr qword [rsi + 472], 56
    btr qword [rsi + 488], 1
    btr qword [rsi + 496], 10
    btr qword [rsi + 504], 19
    btr qword [rsi + 512], 28
    btr qword [rsi + 520], 37
    btr qword [rsi + 528], 46
    btr qword [rsi + 536], 55
    btr qword [rsi + 552], 0
    btr qword [rsi + 560], 9
    btr qword [rsi + 568], 18
    btr qword [rsi + 576], 27
    add rsi, 584
    sub ecx, 73
    cmp ecx, 73
    jae run_sieve_dense_loop_073
run_sieve_dense_tail_073:
    test ecx, ecx
    jz run_sieve_dense_restore_073
    cmp ecx, 1
    jb run_sieve_dense_restore_073
    btr qword [rsi], 36
    cmp ecx, 2
    jb run_sieve_dense_restore_073
    btr qword [rsi + 8], 45
    cmp ecx, 3
    jb run_sieve_dense_restore_073
    btr qword [rsi + 16], 54
    cmp ecx, 4
    jb run_sieve_dense_restore_073
    btr qword [rsi + 24], 63
    cmp ecx, 6
    jb run_sieve_dense_restore_073
    btr qword [rsi + 40], 8
    cmp ecx, 7
    jb run_sieve_dense_restore_073
    btr qword [rsi + 48], 17
    cmp ecx, 8
    jb run_sieve_dense_restore_073
    btr qword [rsi + 56], 26
    cmp ecx, 9
    jb run_sieve_dense_restore_073
    btr qword [rsi + 64], 35
    cmp ecx, 10
    jb run_sieve_dense_restore_073
    btr qword [rsi + 72], 44
    cmp ecx, 11
    jb run_sieve_dense_restore_073
    btr qword [rsi + 80], 53
    cmp ecx, 12
    jb run_sieve_dense_restore_073
    btr qword [rsi + 88], 62
    cmp ecx, 14
    jb run_sieve_dense_restore_073
    btr qword [rsi + 104], 7
    cmp ecx, 15
    jb run_sieve_dense_restore_073
    btr qword [rsi + 112], 16
    cmp ecx, 16
    jb run_sieve_dense_restore_073
    btr qword [rsi + 120], 25
    cmp ecx, 17
    jb run_sieve_dense_restore_073
    btr qword [rsi + 128], 34
    cmp ecx, 18
    jb run_sieve_dense_restore_073
    btr qword [rsi + 136], 43
    cmp ecx, 19
    jb run_sieve_dense_restore_073
    btr qword [rsi + 144], 52
    cmp ecx, 20
    jb run_sieve_dense_restore_073
    btr qword [rsi + 152], 61
    cmp ecx, 22
    jb run_sieve_dense_restore_073
    btr qword [rsi + 168], 6
    cmp ecx, 23
    jb run_sieve_dense_restore_073
    btr qword [rsi + 176], 15
    cmp ecx, 24
    jb run_sieve_dense_restore_073
    btr qword [rsi + 184], 24
    cmp ecx, 25
    jb run_sieve_dense_restore_073
    btr qword [rsi + 192], 33
    cmp ecx, 26
    jb run_sieve_dense_restore_073
    btr qword [rsi + 200], 42
    cmp ecx, 27
    jb run_sieve_dense_restore_073
    btr qword [rsi + 208], 51
    cmp ecx, 28
    jb run_sieve_dense_restore_073
    btr qword [rsi + 216], 60
    cmp ecx, 30
    jb run_sieve_dense_restore_073
    btr qword [rsi + 232], 5
    cmp ecx, 31
    jb run_sieve_dense_restore_073
    btr qword [rsi + 240], 14
    cmp ecx, 32
    jb run_sieve_dense_restore_073
    btr qword [rsi + 248], 23
    cmp ecx, 33
    jb run_sieve_dense_restore_073
    btr qword [rsi + 256], 32
    cmp ecx, 34
    jb run_sieve_dense_restore_073
    btr qword [rsi + 264], 41
    cmp ecx, 35
    jb run_sieve_dense_restore_073
    btr qword [rsi + 272], 50
    cmp ecx, 36
    jb run_sieve_dense_restore_073
    btr qword [rsi + 280], 59
    cmp ecx, 38
    jb run_sieve_dense_restore_073
    btr qword [rsi + 296], 4
    cmp ecx, 39
    jb run_sieve_dense_restore_073
    btr qword [rsi + 304], 13
    cmp ecx, 40
    jb run_sieve_dense_restore_073
    btr qword [rsi + 312], 22
    cmp ecx, 41
    jb run_sieve_dense_restore_073
    btr qword [rsi + 320], 31
    cmp ecx, 42
    jb run_sieve_dense_restore_073
    btr qword [rsi + 328], 40
    cmp ecx, 43
    jb run_sieve_dense_restore_073
    btr qword [rsi + 336], 49
    cmp ecx, 44
    jb run_sieve_dense_restore_073
    btr qword [rsi + 344], 58
    cmp ecx, 46
    jb run_sieve_dense_restore_073
    btr qword [rsi + 360], 3
    cmp ecx, 47
    jb run_sieve_dense_restore_073
    btr qword [rsi + 368], 12
    cmp ecx, 48
    jb run_sieve_dense_restore_073
    btr qword [rsi + 376], 21
    cmp ecx, 49
    jb run_sieve_dense_restore_073
    btr qword [rsi + 384], 30
    cmp ecx, 50
    jb run_sieve_dense_restore_073
    btr qword [rsi + 392], 39
    cmp ecx, 51
    jb run_sieve_dense_restore_073
    btr qword [rsi + 400], 48
    cmp ecx, 52
    jb run_sieve_dense_restore_073
    btr qword [rsi + 408], 57
    cmp ecx, 54
    jb run_sieve_dense_restore_073
    btr qword [rsi + 424], 2
    cmp ecx, 55
    jb run_sieve_dense_restore_073
    btr qword [rsi + 432], 11
    cmp ecx, 56
    jb run_sieve_dense_restore_073
    btr qword [rsi + 440], 20
    cmp ecx, 57
    jb run_sieve_dense_restore_073
    btr qword [rsi + 448], 29
    cmp ecx, 58
    jb run_sieve_dense_restore_073
    btr qword [rsi + 456], 38
    cmp ecx, 59
    jb run_sieve_dense_restore_073
    btr qword [rsi + 464], 47
    cmp ecx, 60
    jb run_sieve_dense_restore_073
    btr qword [rsi + 472], 56
    cmp ecx, 62
    jb run_sieve_dense_restore_073
    btr qword [rsi + 488], 1
    cmp ecx, 63
    jb run_sieve_dense_restore_073
    btr qword [rsi + 496], 10
    cmp ecx, 64
    jb run_sieve_dense_restore_073
    btr qword [rsi + 504], 19
    cmp ecx, 65
    jb run_sieve_dense_restore_073
    btr qword [rsi + 512], 28
    cmp ecx, 66
    jb run_sieve_dense_restore_073
    btr qword [rsi + 520], 37
    cmp ecx, 67
    jb run_sieve_dense_restore_073
    btr qword [rsi + 528], 46
    cmp ecx, 68
    jb run_sieve_dense_restore_073
    btr qword [rsi + 536], 55
    cmp ecx, 70
    jb run_sieve_dense_restore_073
    btr qword [rsi + 552], 0
    cmp ecx, 71
    jb run_sieve_dense_restore_073
    btr qword [rsi + 560], 9
    cmp ecx, 72
    jb run_sieve_dense_restore_073
    btr qword [rsi + 568], 18
    cmp ecx, 73
    jb run_sieve_dense_restore_073
    btr qword [rsi + 576], 27
run_sieve_dense_restore_073:
    bts qword [r14], 36
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_075:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 75
    jb run_sieve_dense_tail_075
align 16
run_sieve_dense_loop_075:
    btr qword [rsi], 37
    btr qword [rsi + 8], 48
    btr qword [rsi + 16], 59
    btr qword [rsi + 32], 6
    btr qword [rsi + 40], 17
    btr qword [rsi + 48], 28
    btr qword [rsi + 56], 39
    btr qword [rsi + 64], 50
    btr qword [rsi + 72], 61
    btr qword [rsi + 88], 8
    btr qword [rsi + 96], 19
    btr qword [rsi + 104], 30
    btr qword [rsi + 112], 41
    btr qword [rsi + 120], 52
    btr qword [rsi + 128], 63
    btr qword [rsi + 144], 10
    btr qword [rsi + 152], 21
    btr qword [rsi + 160], 32
    btr qword [rsi + 168], 43
    btr qword [rsi + 176], 54
    btr qword [rsi + 192], 1
    btr qword [rsi + 200], 12
    btr qword [rsi + 208], 23
    btr qword [rsi + 216], 34
    btr qword [rsi + 224], 45
    btr qword [rsi + 232], 56
    btr qword [rsi + 248], 3
    btr qword [rsi + 256], 14
    btr qword [rsi + 264], 25
    btr qword [rsi + 272], 36
    btr qword [rsi + 280], 47
    btr qword [rsi + 288], 58
    btr qword [rsi + 304], 5
    btr qword [rsi + 312], 16
    btr qword [rsi + 320], 27
    btr qword [rsi + 328], 38
    btr qword [rsi + 336], 49
    btr qword [rsi + 344], 60
    btr qword [rsi + 360], 7
    btr qword [rsi + 368], 18
    btr qword [rsi + 376], 29
    btr qword [rsi + 384], 40
    btr qword [rsi + 392], 51
    btr qword [rsi + 400], 62
    btr qword [rsi + 416], 9
    btr qword [rsi + 424], 20
    btr qword [rsi + 432], 31
    btr qword [rsi + 440], 42
    btr qword [rsi + 448], 53
    btr qword [rsi + 464], 0
    btr qword [rsi + 472], 11
    btr qword [rsi + 480], 22
    btr qword [rsi + 488], 33
    btr qword [rsi + 496], 44
    btr qword [rsi + 504], 55
    btr qword [rsi + 520], 2
    btr qword [rsi + 528], 13
    btr qword [rsi + 536], 24
    btr qword [rsi + 544], 35
    btr qword [rsi + 552], 46
    btr qword [rsi + 560], 57
    btr qword [rsi + 576], 4
    btr qword [rsi + 584], 15
    btr qword [rsi + 592], 26
    add rsi, 600
    sub ecx, 75
    cmp ecx, 75
    jae run_sieve_dense_loop_075
run_sieve_dense_tail_075:
    test ecx, ecx
    jz run_sieve_dense_restore_075
    cmp ecx, 1
    jb run_sieve_dense_restore_075
    btr qword [rsi], 37
    cmp ecx, 2
    jb run_sieve_dense_restore_075
    btr qword [rsi + 8], 48
    cmp ecx, 3
    jb run_sieve_dense_restore_075
    btr qword [rsi + 16], 59
    cmp ecx, 5
    jb run_sieve_dense_restore_075
    btr qword [rsi + 32], 6
    cmp ecx, 6
    jb run_sieve_dense_restore_075
    btr qword [rsi + 40], 17
    cmp ecx, 7
    jb run_sieve_dense_restore_075
    btr qword [rsi + 48], 28
    cmp ecx, 8
    jb run_sieve_dense_restore_075
    btr qword [rsi + 56], 39
    cmp ecx, 9
    jb run_sieve_dense_restore_075
    btr qword [rsi + 64], 50
    cmp ecx, 10
    jb run_sieve_dense_restore_075
    btr qword [rsi + 72], 61
    cmp ecx, 12
    jb run_sieve_dense_restore_075
    btr qword [rsi + 88], 8
    cmp ecx, 13
    jb run_sieve_dense_restore_075
    btr qword [rsi + 96], 19
    cmp ecx, 14
    jb run_sieve_dense_restore_075
    btr qword [rsi + 104], 30
    cmp ecx, 15
    jb run_sieve_dense_restore_075
    btr qword [rsi + 112], 41
    cmp ecx, 16
    jb run_sieve_dense_restore_075
    btr qword [rsi + 120], 52
    cmp ecx, 17
    jb run_sieve_dense_restore_075
    btr qword [rsi + 128], 63
    cmp ecx, 19
    jb run_sieve_dense_restore_075
    btr qword [rsi + 144], 10
    cmp ecx, 20
    jb run_sieve_dense_restore_075
    btr qword [rsi + 152], 21
    cmp ecx, 21
    jb run_sieve_dense_restore_075
    btr qword [rsi + 160], 32
    cmp ecx, 22
    jb run_sieve_dense_restore_075
    btr qword [rsi + 168], 43
    cmp ecx, 23
    jb run_sieve_dense_restore_075
    btr qword [rsi + 176], 54
    cmp ecx, 25
    jb run_sieve_dense_restore_075
    btr qword [rsi + 192], 1
    cmp ecx, 26
    jb run_sieve_dense_restore_075
    btr qword [rsi + 200], 12
    cmp ecx, 27
    jb run_sieve_dense_restore_075
    btr qword [rsi + 208], 23
    cmp ecx, 28
    jb run_sieve_dense_restore_075
    btr qword [rsi + 216], 34
    cmp ecx, 29
    jb run_sieve_dense_restore_075
    btr qword [rsi + 224], 45
    cmp ecx, 30
    jb run_sieve_dense_restore_075
    btr qword [rsi + 232], 56
    cmp ecx, 32
    jb run_sieve_dense_restore_075
    btr qword [rsi + 248], 3
    cmp ecx, 33
    jb run_sieve_dense_restore_075
    btr qword [rsi + 256], 14
    cmp ecx, 34
    jb run_sieve_dense_restore_075
    btr qword [rsi + 264], 25
    cmp ecx, 35
    jb run_sieve_dense_restore_075
    btr qword [rsi + 272], 36
    cmp ecx, 36
    jb run_sieve_dense_restore_075
    btr qword [rsi + 280], 47
    cmp ecx, 37
    jb run_sieve_dense_restore_075
    btr qword [rsi + 288], 58
    cmp ecx, 39
    jb run_sieve_dense_restore_075
    btr qword [rsi + 304], 5
    cmp ecx, 40
    jb run_sieve_dense_restore_075
    btr qword [rsi + 312], 16
    cmp ecx, 41
    jb run_sieve_dense_restore_075
    btr qword [rsi + 320], 27
    cmp ecx, 42
    jb run_sieve_dense_restore_075
    btr qword [rsi + 328], 38
    cmp ecx, 43
    jb run_sieve_dense_restore_075
    btr qword [rsi + 336], 49
    cmp ecx, 44
    jb run_sieve_dense_restore_075
    btr qword [rsi + 344], 60
    cmp ecx, 46
    jb run_sieve_dense_restore_075
    btr qword [rsi + 360], 7
    cmp ecx, 47
    jb run_sieve_dense_restore_075
    btr qword [rsi + 368], 18
    cmp ecx, 48
    jb run_sieve_dense_restore_075
    btr qword [rsi + 376], 29
    cmp ecx, 49
    jb run_sieve_dense_restore_075
    btr qword [rsi + 384], 40
    cmp ecx, 50
    jb run_sieve_dense_restore_075
    btr qword [rsi + 392], 51
    cmp ecx, 51
    jb run_sieve_dense_restore_075
    btr qword [rsi + 400], 62
    cmp ecx, 53
    jb run_sieve_dense_restore_075
    btr qword [rsi + 416], 9
    cmp ecx, 54
    jb run_sieve_dense_restore_075
    btr qword [rsi + 424], 20
    cmp ecx, 55
    jb run_sieve_dense_restore_075
    btr qword [rsi + 432], 31
    cmp ecx, 56
    jb run_sieve_dense_restore_075
    btr qword [rsi + 440], 42
    cmp ecx, 57
    jb run_sieve_dense_restore_075
    btr qword [rsi + 448], 53
    cmp ecx, 59
    jb run_sieve_dense_restore_075
    btr qword [rsi + 464], 0
    cmp ecx, 60
    jb run_sieve_dense_restore_075
    btr qword [rsi + 472], 11
    cmp ecx, 61
    jb run_sieve_dense_restore_075
    btr qword [rsi + 480], 22
    cmp ecx, 62
    jb run_sieve_dense_restore_075
    btr qword [rsi + 488], 33
    cmp ecx, 63
    jb run_sieve_dense_restore_075
    btr qword [rsi + 496], 44
    cmp ecx, 64
    jb run_sieve_dense_restore_075
    btr qword [rsi + 504], 55
    cmp ecx, 66
    jb run_sieve_dense_restore_075
    btr qword [rsi + 520], 2
    cmp ecx, 67
    jb run_sieve_dense_restore_075
    btr qword [rsi + 528], 13
    cmp ecx, 68
    jb run_sieve_dense_restore_075
    btr qword [rsi + 536], 24
    cmp ecx, 69
    jb run_sieve_dense_restore_075
    btr qword [rsi + 544], 35
    cmp ecx, 70
    jb run_sieve_dense_restore_075
    btr qword [rsi + 552], 46
    cmp ecx, 71
    jb run_sieve_dense_restore_075
    btr qword [rsi + 560], 57
    cmp ecx, 73
    jb run_sieve_dense_restore_075
    btr qword [rsi + 576], 4
    cmp ecx, 74
    jb run_sieve_dense_restore_075
    btr qword [rsi + 584], 15
    cmp ecx, 75
    jb run_sieve_dense_restore_075
    btr qword [rsi + 592], 26
run_sieve_dense_restore_075:
    bts qword [r14], 37
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_077:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 77
    jb run_sieve_dense_tail_077
align 16
run_sieve_dense_loop_077:
    btr qword [rsi], 38
    btr qword [rsi + 8], 51
    btr qword [rsi + 24], 0
    btr qword [rsi + 32], 13
    btr qword [rsi + 40], 26
    btr qword [rsi + 48], 39
    btr qword [rsi + 56], 52
    btr qword [rsi + 72], 1
    btr qword [rsi + 80], 14
    btr qword [rsi + 88], 27
    btr qword [rsi + 96], 40
    btr qword [rsi + 104], 53
    btr qword [rsi + 120], 2
    btr qword [rsi + 128], 15
    btr qword [rsi + 136], 28
    btr qword [rsi + 144], 41
    btr qword [rsi + 152], 54
    btr qword [rsi + 168], 3
    btr qword [rsi + 176], 16
    btr qword [rsi + 184], 29
    btr qword [rsi + 192], 42
    btr qword [rsi + 200], 55
    btr qword [rsi + 216], 4
    btr qword [rsi + 224], 17
    btr qword [rsi + 232], 30
    btr qword [rsi + 240], 43
    btr qword [rsi + 248], 56
    btr qword [rsi + 264], 5
    btr qword [rsi + 272], 18
    btr qword [rsi + 280], 31
    btr qword [rsi + 288], 44
    btr qword [rsi + 296], 57
    btr qword [rsi + 312], 6
    btr qword [rsi + 320], 19
    btr qword [rsi + 328], 32
    btr qword [rsi + 336], 45
    btr qword [rsi + 344], 58
    btr qword [rsi + 360], 7
    btr qword [rsi + 368], 20
    btr qword [rsi + 376], 33
    btr qword [rsi + 384], 46
    btr qword [rsi + 392], 59
    btr qword [rsi + 408], 8
    btr qword [rsi + 416], 21
    btr qword [rsi + 424], 34
    btr qword [rsi + 432], 47
    btr qword [rsi + 440], 60
    btr qword [rsi + 456], 9
    btr qword [rsi + 464], 22
    btr qword [rsi + 472], 35
    btr qword [rsi + 480], 48
    btr qword [rsi + 488], 61
    btr qword [rsi + 504], 10
    btr qword [rsi + 512], 23
    btr qword [rsi + 520], 36
    btr qword [rsi + 528], 49
    btr qword [rsi + 536], 62
    btr qword [rsi + 552], 11
    btr qword [rsi + 560], 24
    btr qword [rsi + 568], 37
    btr qword [rsi + 576], 50
    btr qword [rsi + 584], 63
    btr qword [rsi + 600], 12
    btr qword [rsi + 608], 25
    add rsi, 616
    sub ecx, 77
    cmp ecx, 77
    jae run_sieve_dense_loop_077
run_sieve_dense_tail_077:
    test ecx, ecx
    jz run_sieve_dense_restore_077
    cmp ecx, 1
    jb run_sieve_dense_restore_077
    btr qword [rsi], 38
    cmp ecx, 2
    jb run_sieve_dense_restore_077
    btr qword [rsi + 8], 51
    cmp ecx, 4
    jb run_sieve_dense_restore_077
    btr qword [rsi + 24], 0
    cmp ecx, 5
    jb run_sieve_dense_restore_077
    btr qword [rsi + 32], 13
    cmp ecx, 6
    jb run_sieve_dense_restore_077
    btr qword [rsi + 40], 26
    cmp ecx, 7
    jb run_sieve_dense_restore_077
    btr qword [rsi + 48], 39
    cmp ecx, 8
    jb run_sieve_dense_restore_077
    btr qword [rsi + 56], 52
    cmp ecx, 10
    jb run_sieve_dense_restore_077
    btr qword [rsi + 72], 1
    cmp ecx, 11
    jb run_sieve_dense_restore_077
    btr qword [rsi + 80], 14
    cmp ecx, 12
    jb run_sieve_dense_restore_077
    btr qword [rsi + 88], 27
    cmp ecx, 13
    jb run_sieve_dense_restore_077
    btr qword [rsi + 96], 40
    cmp ecx, 14
    jb run_sieve_dense_restore_077
    btr qword [rsi + 104], 53
    cmp ecx, 16
    jb run_sieve_dense_restore_077
    btr qword [rsi + 120], 2
    cmp ecx, 17
    jb run_sieve_dense_restore_077
    btr qword [rsi + 128], 15
    cmp ecx, 18
    jb run_sieve_dense_restore_077
    btr qword [rsi + 136], 28
    cmp ecx, 19
    jb run_sieve_dense_restore_077
    btr qword [rsi + 144], 41
    cmp ecx, 20
    jb run_sieve_dense_restore_077
    btr qword [rsi + 152], 54
    cmp ecx, 22
    jb run_sieve_dense_restore_077
    btr qword [rsi + 168], 3
    cmp ecx, 23
    jb run_sieve_dense_restore_077
    btr qword [rsi + 176], 16
    cmp ecx, 24
    jb run_sieve_dense_restore_077
    btr qword [rsi + 184], 29
    cmp ecx, 25
    jb run_sieve_dense_restore_077
    btr qword [rsi + 192], 42
    cmp ecx, 26
    jb run_sieve_dense_restore_077
    btr qword [rsi + 200], 55
    cmp ecx, 28
    jb run_sieve_dense_restore_077
    btr qword [rsi + 216], 4
    cmp ecx, 29
    jb run_sieve_dense_restore_077
    btr qword [rsi + 224], 17
    cmp ecx, 30
    jb run_sieve_dense_restore_077
    btr qword [rsi + 232], 30
    cmp ecx, 31
    jb run_sieve_dense_restore_077
    btr qword [rsi + 240], 43
    cmp ecx, 32
    jb run_sieve_dense_restore_077
    btr qword [rsi + 248], 56
    cmp ecx, 34
    jb run_sieve_dense_restore_077
    btr qword [rsi + 264], 5
    cmp ecx, 35
    jb run_sieve_dense_restore_077
    btr qword [rsi + 272], 18
    cmp ecx, 36
    jb run_sieve_dense_restore_077
    btr qword [rsi + 280], 31
    cmp ecx, 37
    jb run_sieve_dense_restore_077
    btr qword [rsi + 288], 44
    cmp ecx, 38
    jb run_sieve_dense_restore_077
    btr qword [rsi + 296], 57
    cmp ecx, 40
    jb run_sieve_dense_restore_077
    btr qword [rsi + 312], 6
    cmp ecx, 41
    jb run_sieve_dense_restore_077
    btr qword [rsi + 320], 19
    cmp ecx, 42
    jb run_sieve_dense_restore_077
    btr qword [rsi + 328], 32
    cmp ecx, 43
    jb run_sieve_dense_restore_077
    btr qword [rsi + 336], 45
    cmp ecx, 44
    jb run_sieve_dense_restore_077
    btr qword [rsi + 344], 58
    cmp ecx, 46
    jb run_sieve_dense_restore_077
    btr qword [rsi + 360], 7
    cmp ecx, 47
    jb run_sieve_dense_restore_077
    btr qword [rsi + 368], 20
    cmp ecx, 48
    jb run_sieve_dense_restore_077
    btr qword [rsi + 376], 33
    cmp ecx, 49
    jb run_sieve_dense_restore_077
    btr qword [rsi + 384], 46
    cmp ecx, 50
    jb run_sieve_dense_restore_077
    btr qword [rsi + 392], 59
    cmp ecx, 52
    jb run_sieve_dense_restore_077
    btr qword [rsi + 408], 8
    cmp ecx, 53
    jb run_sieve_dense_restore_077
    btr qword [rsi + 416], 21
    cmp ecx, 54
    jb run_sieve_dense_restore_077
    btr qword [rsi + 424], 34
    cmp ecx, 55
    jb run_sieve_dense_restore_077
    btr qword [rsi + 432], 47
    cmp ecx, 56
    jb run_sieve_dense_restore_077
    btr qword [rsi + 440], 60
    cmp ecx, 58
    jb run_sieve_dense_restore_077
    btr qword [rsi + 456], 9
    cmp ecx, 59
    jb run_sieve_dense_restore_077
    btr qword [rsi + 464], 22
    cmp ecx, 60
    jb run_sieve_dense_restore_077
    btr qword [rsi + 472], 35
    cmp ecx, 61
    jb run_sieve_dense_restore_077
    btr qword [rsi + 480], 48
    cmp ecx, 62
    jb run_sieve_dense_restore_077
    btr qword [rsi + 488], 61
    cmp ecx, 64
    jb run_sieve_dense_restore_077
    btr qword [rsi + 504], 10
    cmp ecx, 65
    jb run_sieve_dense_restore_077
    btr qword [rsi + 512], 23
    cmp ecx, 66
    jb run_sieve_dense_restore_077
    btr qword [rsi + 520], 36
    cmp ecx, 67
    jb run_sieve_dense_restore_077
    btr qword [rsi + 528], 49
    cmp ecx, 68
    jb run_sieve_dense_restore_077
    btr qword [rsi + 536], 62
    cmp ecx, 70
    jb run_sieve_dense_restore_077
    btr qword [rsi + 552], 11
    cmp ecx, 71
    jb run_sieve_dense_restore_077
    btr qword [rsi + 560], 24
    cmp ecx, 72
    jb run_sieve_dense_restore_077
    btr qword [rsi + 568], 37
    cmp ecx, 73
    jb run_sieve_dense_restore_077
    btr qword [rsi + 576], 50
    cmp ecx, 74
    jb run_sieve_dense_restore_077
    btr qword [rsi + 584], 63
    cmp ecx, 76
    jb run_sieve_dense_restore_077
    btr qword [rsi + 600], 12
    cmp ecx, 77
    jb run_sieve_dense_restore_077
    btr qword [rsi + 608], 25
run_sieve_dense_restore_077:
    bts qword [r14], 38
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_079:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 79
    jb run_sieve_dense_tail_079
align 16
run_sieve_dense_loop_079:
    btr qword [rsi], 39
    btr qword [rsi + 8], 54
    btr qword [rsi + 24], 5
    btr qword [rsi + 32], 20
    btr qword [rsi + 40], 35
    btr qword [rsi + 48], 50
    btr qword [rsi + 64], 1
    btr qword [rsi + 72], 16
    btr qword [rsi + 80], 31
    btr qword [rsi + 88], 46
    btr qword [rsi + 96], 61
    btr qword [rsi + 112], 12
    btr qword [rsi + 120], 27
    btr qword [rsi + 128], 42
    btr qword [rsi + 136], 57
    btr qword [rsi + 152], 8
    btr qword [rsi + 160], 23
    btr qword [rsi + 168], 38
    btr qword [rsi + 176], 53
    btr qword [rsi + 192], 4
    btr qword [rsi + 200], 19
    btr qword [rsi + 208], 34
    btr qword [rsi + 216], 49
    btr qword [rsi + 232], 0
    btr qword [rsi + 240], 15
    btr qword [rsi + 248], 30
    btr qword [rsi + 256], 45
    btr qword [rsi + 264], 60
    btr qword [rsi + 280], 11
    btr qword [rsi + 288], 26
    btr qword [rsi + 296], 41
    btr qword [rsi + 304], 56
    btr qword [rsi + 320], 7
    btr qword [rsi + 328], 22
    btr qword [rsi + 336], 37
    btr qword [rsi + 344], 52
    btr qword [rsi + 360], 3
    btr qword [rsi + 368], 18
    btr qword [rsi + 376], 33
    btr qword [rsi + 384], 48
    btr qword [rsi + 392], 63
    btr qword [rsi + 408], 14
    btr qword [rsi + 416], 29
    btr qword [rsi + 424], 44
    btr qword [rsi + 432], 59
    btr qword [rsi + 448], 10
    btr qword [rsi + 456], 25
    btr qword [rsi + 464], 40
    btr qword [rsi + 472], 55
    btr qword [rsi + 488], 6
    btr qword [rsi + 496], 21
    btr qword [rsi + 504], 36
    btr qword [rsi + 512], 51
    btr qword [rsi + 528], 2
    btr qword [rsi + 536], 17
    btr qword [rsi + 544], 32
    btr qword [rsi + 552], 47
    btr qword [rsi + 560], 62
    btr qword [rsi + 576], 13
    btr qword [rsi + 584], 28
    btr qword [rsi + 592], 43
    btr qword [rsi + 600], 58
    btr qword [rsi + 616], 9
    btr qword [rsi + 624], 24
    add rsi, 632
    sub ecx, 79
    cmp ecx, 79
    jae run_sieve_dense_loop_079
run_sieve_dense_tail_079:
    test ecx, ecx
    jz run_sieve_dense_restore_079
    cmp ecx, 1
    jb run_sieve_dense_restore_079
    btr qword [rsi], 39
    cmp ecx, 2
    jb run_sieve_dense_restore_079
    btr qword [rsi + 8], 54
    cmp ecx, 4
    jb run_sieve_dense_restore_079
    btr qword [rsi + 24], 5
    cmp ecx, 5
    jb run_sieve_dense_restore_079
    btr qword [rsi + 32], 20
    cmp ecx, 6
    jb run_sieve_dense_restore_079
    btr qword [rsi + 40], 35
    cmp ecx, 7
    jb run_sieve_dense_restore_079
    btr qword [rsi + 48], 50
    cmp ecx, 9
    jb run_sieve_dense_restore_079
    btr qword [rsi + 64], 1
    cmp ecx, 10
    jb run_sieve_dense_restore_079
    btr qword [rsi + 72], 16
    cmp ecx, 11
    jb run_sieve_dense_restore_079
    btr qword [rsi + 80], 31
    cmp ecx, 12
    jb run_sieve_dense_restore_079
    btr qword [rsi + 88], 46
    cmp ecx, 13
    jb run_sieve_dense_restore_079
    btr qword [rsi + 96], 61
    cmp ecx, 15
    jb run_sieve_dense_restore_079
    btr qword [rsi + 112], 12
    cmp ecx, 16
    jb run_sieve_dense_restore_079
    btr qword [rsi + 120], 27
    cmp ecx, 17
    jb run_sieve_dense_restore_079
    btr qword [rsi + 128], 42
    cmp ecx, 18
    jb run_sieve_dense_restore_079
    btr qword [rsi + 136], 57
    cmp ecx, 20
    jb run_sieve_dense_restore_079
    btr qword [rsi + 152], 8
    cmp ecx, 21
    jb run_sieve_dense_restore_079
    btr qword [rsi + 160], 23
    cmp ecx, 22
    jb run_sieve_dense_restore_079
    btr qword [rsi + 168], 38
    cmp ecx, 23
    jb run_sieve_dense_restore_079
    btr qword [rsi + 176], 53
    cmp ecx, 25
    jb run_sieve_dense_restore_079
    btr qword [rsi + 192], 4
    cmp ecx, 26
    jb run_sieve_dense_restore_079
    btr qword [rsi + 200], 19
    cmp ecx, 27
    jb run_sieve_dense_restore_079
    btr qword [rsi + 208], 34
    cmp ecx, 28
    jb run_sieve_dense_restore_079
    btr qword [rsi + 216], 49
    cmp ecx, 30
    jb run_sieve_dense_restore_079
    btr qword [rsi + 232], 0
    cmp ecx, 31
    jb run_sieve_dense_restore_079
    btr qword [rsi + 240], 15
    cmp ecx, 32
    jb run_sieve_dense_restore_079
    btr qword [rsi + 248], 30
    cmp ecx, 33
    jb run_sieve_dense_restore_079
    btr qword [rsi + 256], 45
    cmp ecx, 34
    jb run_sieve_dense_restore_079
    btr qword [rsi + 264], 60
    cmp ecx, 36
    jb run_sieve_dense_restore_079
    btr qword [rsi + 280], 11
    cmp ecx, 37
    jb run_sieve_dense_restore_079
    btr qword [rsi + 288], 26
    cmp ecx, 38
    jb run_sieve_dense_restore_079
    btr qword [rsi + 296], 41
    cmp ecx, 39
    jb run_sieve_dense_restore_079
    btr qword [rsi + 304], 56
    cmp ecx, 41
    jb run_sieve_dense_restore_079
    btr qword [rsi + 320], 7
    cmp ecx, 42
    jb run_sieve_dense_restore_079
    btr qword [rsi + 328], 22
    cmp ecx, 43
    jb run_sieve_dense_restore_079
    btr qword [rsi + 336], 37
    cmp ecx, 44
    jb run_sieve_dense_restore_079
    btr qword [rsi + 344], 52
    cmp ecx, 46
    jb run_sieve_dense_restore_079
    btr qword [rsi + 360], 3
    cmp ecx, 47
    jb run_sieve_dense_restore_079
    btr qword [rsi + 368], 18
    cmp ecx, 48
    jb run_sieve_dense_restore_079
    btr qword [rsi + 376], 33
    cmp ecx, 49
    jb run_sieve_dense_restore_079
    btr qword [rsi + 384], 48
    cmp ecx, 50
    jb run_sieve_dense_restore_079
    btr qword [rsi + 392], 63
    cmp ecx, 52
    jb run_sieve_dense_restore_079
    btr qword [rsi + 408], 14
    cmp ecx, 53
    jb run_sieve_dense_restore_079
    btr qword [rsi + 416], 29
    cmp ecx, 54
    jb run_sieve_dense_restore_079
    btr qword [rsi + 424], 44
    cmp ecx, 55
    jb run_sieve_dense_restore_079
    btr qword [rsi + 432], 59
    cmp ecx, 57
    jb run_sieve_dense_restore_079
    btr qword [rsi + 448], 10
    cmp ecx, 58
    jb run_sieve_dense_restore_079
    btr qword [rsi + 456], 25
    cmp ecx, 59
    jb run_sieve_dense_restore_079
    btr qword [rsi + 464], 40
    cmp ecx, 60
    jb run_sieve_dense_restore_079
    btr qword [rsi + 472], 55
    cmp ecx, 62
    jb run_sieve_dense_restore_079
    btr qword [rsi + 488], 6
    cmp ecx, 63
    jb run_sieve_dense_restore_079
    btr qword [rsi + 496], 21
    cmp ecx, 64
    jb run_sieve_dense_restore_079
    btr qword [rsi + 504], 36
    cmp ecx, 65
    jb run_sieve_dense_restore_079
    btr qword [rsi + 512], 51
    cmp ecx, 67
    jb run_sieve_dense_restore_079
    btr qword [rsi + 528], 2
    cmp ecx, 68
    jb run_sieve_dense_restore_079
    btr qword [rsi + 536], 17
    cmp ecx, 69
    jb run_sieve_dense_restore_079
    btr qword [rsi + 544], 32
    cmp ecx, 70
    jb run_sieve_dense_restore_079
    btr qword [rsi + 552], 47
    cmp ecx, 71
    jb run_sieve_dense_restore_079
    btr qword [rsi + 560], 62
    cmp ecx, 73
    jb run_sieve_dense_restore_079
    btr qword [rsi + 576], 13
    cmp ecx, 74
    jb run_sieve_dense_restore_079
    btr qword [rsi + 584], 28
    cmp ecx, 75
    jb run_sieve_dense_restore_079
    btr qword [rsi + 592], 43
    cmp ecx, 76
    jb run_sieve_dense_restore_079
    btr qword [rsi + 600], 58
    cmp ecx, 78
    jb run_sieve_dense_restore_079
    btr qword [rsi + 616], 9
    cmp ecx, 79
    jb run_sieve_dense_restore_079
    btr qword [rsi + 624], 24
run_sieve_dense_restore_079:
    bts qword [r14], 39
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_081:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 81
    jb run_sieve_dense_tail_081
align 16
run_sieve_dense_loop_081:
    btr qword [rsi], 40
    btr qword [rsi + 8], 57
    btr qword [rsi + 24], 10
    btr qword [rsi + 32], 27
    btr qword [rsi + 40], 44
    btr qword [rsi + 48], 61
    btr qword [rsi + 64], 14
    btr qword [rsi + 72], 31
    btr qword [rsi + 80], 48
    btr qword [rsi + 96], 1
    btr qword [rsi + 104], 18
    btr qword [rsi + 112], 35
    btr qword [rsi + 120], 52
    btr qword [rsi + 136], 5
    btr qword [rsi + 144], 22
    btr qword [rsi + 152], 39
    btr qword [rsi + 160], 56
    btr qword [rsi + 176], 9
    btr qword [rsi + 184], 26
    btr qword [rsi + 192], 43
    btr qword [rsi + 200], 60
    btr qword [rsi + 216], 13
    btr qword [rsi + 224], 30
    btr qword [rsi + 232], 47
    btr qword [rsi + 248], 0
    btr qword [rsi + 256], 17
    btr qword [rsi + 264], 34
    btr qword [rsi + 272], 51
    btr qword [rsi + 288], 4
    btr qword [rsi + 296], 21
    btr qword [rsi + 304], 38
    btr qword [rsi + 312], 55
    btr qword [rsi + 328], 8
    btr qword [rsi + 336], 25
    btr qword [rsi + 344], 42
    btr qword [rsi + 352], 59
    btr qword [rsi + 368], 12
    btr qword [rsi + 376], 29
    btr qword [rsi + 384], 46
    btr qword [rsi + 392], 63
    btr qword [rsi + 408], 16
    btr qword [rsi + 416], 33
    btr qword [rsi + 424], 50
    btr qword [rsi + 440], 3
    btr qword [rsi + 448], 20
    btr qword [rsi + 456], 37
    btr qword [rsi + 464], 54
    btr qword [rsi + 480], 7
    btr qword [rsi + 488], 24
    btr qword [rsi + 496], 41
    btr qword [rsi + 504], 58
    btr qword [rsi + 520], 11
    btr qword [rsi + 528], 28
    btr qword [rsi + 536], 45
    btr qword [rsi + 544], 62
    btr qword [rsi + 560], 15
    btr qword [rsi + 568], 32
    btr qword [rsi + 576], 49
    btr qword [rsi + 592], 2
    btr qword [rsi + 600], 19
    btr qword [rsi + 608], 36
    btr qword [rsi + 616], 53
    btr qword [rsi + 632], 6
    btr qword [rsi + 640], 23
    add rsi, 648
    sub ecx, 81
    cmp ecx, 81
    jae run_sieve_dense_loop_081
run_sieve_dense_tail_081:
    test ecx, ecx
    jz run_sieve_dense_restore_081
    cmp ecx, 1
    jb run_sieve_dense_restore_081
    btr qword [rsi], 40
    cmp ecx, 2
    jb run_sieve_dense_restore_081
    btr qword [rsi + 8], 57
    cmp ecx, 4
    jb run_sieve_dense_restore_081
    btr qword [rsi + 24], 10
    cmp ecx, 5
    jb run_sieve_dense_restore_081
    btr qword [rsi + 32], 27
    cmp ecx, 6
    jb run_sieve_dense_restore_081
    btr qword [rsi + 40], 44
    cmp ecx, 7
    jb run_sieve_dense_restore_081
    btr qword [rsi + 48], 61
    cmp ecx, 9
    jb run_sieve_dense_restore_081
    btr qword [rsi + 64], 14
    cmp ecx, 10
    jb run_sieve_dense_restore_081
    btr qword [rsi + 72], 31
    cmp ecx, 11
    jb run_sieve_dense_restore_081
    btr qword [rsi + 80], 48
    cmp ecx, 13
    jb run_sieve_dense_restore_081
    btr qword [rsi + 96], 1
    cmp ecx, 14
    jb run_sieve_dense_restore_081
    btr qword [rsi + 104], 18
    cmp ecx, 15
    jb run_sieve_dense_restore_081
    btr qword [rsi + 112], 35
    cmp ecx, 16
    jb run_sieve_dense_restore_081
    btr qword [rsi + 120], 52
    cmp ecx, 18
    jb run_sieve_dense_restore_081
    btr qword [rsi + 136], 5
    cmp ecx, 19
    jb run_sieve_dense_restore_081
    btr qword [rsi + 144], 22
    cmp ecx, 20
    jb run_sieve_dense_restore_081
    btr qword [rsi + 152], 39
    cmp ecx, 21
    jb run_sieve_dense_restore_081
    btr qword [rsi + 160], 56
    cmp ecx, 23
    jb run_sieve_dense_restore_081
    btr qword [rsi + 176], 9
    cmp ecx, 24
    jb run_sieve_dense_restore_081
    btr qword [rsi + 184], 26
    cmp ecx, 25
    jb run_sieve_dense_restore_081
    btr qword [rsi + 192], 43
    cmp ecx, 26
    jb run_sieve_dense_restore_081
    btr qword [rsi + 200], 60
    cmp ecx, 28
    jb run_sieve_dense_restore_081
    btr qword [rsi + 216], 13
    cmp ecx, 29
    jb run_sieve_dense_restore_081
    btr qword [rsi + 224], 30
    cmp ecx, 30
    jb run_sieve_dense_restore_081
    btr qword [rsi + 232], 47
    cmp ecx, 32
    jb run_sieve_dense_restore_081
    btr qword [rsi + 248], 0
    cmp ecx, 33
    jb run_sieve_dense_restore_081
    btr qword [rsi + 256], 17
    cmp ecx, 34
    jb run_sieve_dense_restore_081
    btr qword [rsi + 264], 34
    cmp ecx, 35
    jb run_sieve_dense_restore_081
    btr qword [rsi + 272], 51
    cmp ecx, 37
    jb run_sieve_dense_restore_081
    btr qword [rsi + 288], 4
    cmp ecx, 38
    jb run_sieve_dense_restore_081
    btr qword [rsi + 296], 21
    cmp ecx, 39
    jb run_sieve_dense_restore_081
    btr qword [rsi + 304], 38
    cmp ecx, 40
    jb run_sieve_dense_restore_081
    btr qword [rsi + 312], 55
    cmp ecx, 42
    jb run_sieve_dense_restore_081
    btr qword [rsi + 328], 8
    cmp ecx, 43
    jb run_sieve_dense_restore_081
    btr qword [rsi + 336], 25
    cmp ecx, 44
    jb run_sieve_dense_restore_081
    btr qword [rsi + 344], 42
    cmp ecx, 45
    jb run_sieve_dense_restore_081
    btr qword [rsi + 352], 59
    cmp ecx, 47
    jb run_sieve_dense_restore_081
    btr qword [rsi + 368], 12
    cmp ecx, 48
    jb run_sieve_dense_restore_081
    btr qword [rsi + 376], 29
    cmp ecx, 49
    jb run_sieve_dense_restore_081
    btr qword [rsi + 384], 46
    cmp ecx, 50
    jb run_sieve_dense_restore_081
    btr qword [rsi + 392], 63
    cmp ecx, 52
    jb run_sieve_dense_restore_081
    btr qword [rsi + 408], 16
    cmp ecx, 53
    jb run_sieve_dense_restore_081
    btr qword [rsi + 416], 33
    cmp ecx, 54
    jb run_sieve_dense_restore_081
    btr qword [rsi + 424], 50
    cmp ecx, 56
    jb run_sieve_dense_restore_081
    btr qword [rsi + 440], 3
    cmp ecx, 57
    jb run_sieve_dense_restore_081
    btr qword [rsi + 448], 20
    cmp ecx, 58
    jb run_sieve_dense_restore_081
    btr qword [rsi + 456], 37
    cmp ecx, 59
    jb run_sieve_dense_restore_081
    btr qword [rsi + 464], 54
    cmp ecx, 61
    jb run_sieve_dense_restore_081
    btr qword [rsi + 480], 7
    cmp ecx, 62
    jb run_sieve_dense_restore_081
    btr qword [rsi + 488], 24
    cmp ecx, 63
    jb run_sieve_dense_restore_081
    btr qword [rsi + 496], 41
    cmp ecx, 64
    jb run_sieve_dense_restore_081
    btr qword [rsi + 504], 58
    cmp ecx, 66
    jb run_sieve_dense_restore_081
    btr qword [rsi + 520], 11
    cmp ecx, 67
    jb run_sieve_dense_restore_081
    btr qword [rsi + 528], 28
    cmp ecx, 68
    jb run_sieve_dense_restore_081
    btr qword [rsi + 536], 45
    cmp ecx, 69
    jb run_sieve_dense_restore_081
    btr qword [rsi + 544], 62
    cmp ecx, 71
    jb run_sieve_dense_restore_081
    btr qword [rsi + 560], 15
    cmp ecx, 72
    jb run_sieve_dense_restore_081
    btr qword [rsi + 568], 32
    cmp ecx, 73
    jb run_sieve_dense_restore_081
    btr qword [rsi + 576], 49
    cmp ecx, 75
    jb run_sieve_dense_restore_081
    btr qword [rsi + 592], 2
    cmp ecx, 76
    jb run_sieve_dense_restore_081
    btr qword [rsi + 600], 19
    cmp ecx, 77
    jb run_sieve_dense_restore_081
    btr qword [rsi + 608], 36
    cmp ecx, 78
    jb run_sieve_dense_restore_081
    btr qword [rsi + 616], 53
    cmp ecx, 80
    jb run_sieve_dense_restore_081
    btr qword [rsi + 632], 6
    cmp ecx, 81
    jb run_sieve_dense_restore_081
    btr qword [rsi + 640], 23
run_sieve_dense_restore_081:
    bts qword [r14], 40
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_083:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 83
    jb run_sieve_dense_tail_083
align 16
run_sieve_dense_loop_083:
    btr qword [rsi], 41
    btr qword [rsi + 8], 60
    btr qword [rsi + 24], 15
    btr qword [rsi + 32], 34
    btr qword [rsi + 40], 53
    btr qword [rsi + 56], 8
    btr qword [rsi + 64], 27
    btr qword [rsi + 72], 46
    btr qword [rsi + 88], 1
    btr qword [rsi + 96], 20
    btr qword [rsi + 104], 39
    btr qword [rsi + 112], 58
    btr qword [rsi + 128], 13
    btr qword [rsi + 136], 32
    btr qword [rsi + 144], 51
    btr qword [rsi + 160], 6
    btr qword [rsi + 168], 25
    btr qword [rsi + 176], 44
    btr qword [rsi + 184], 63
    btr qword [rsi + 200], 18
    btr qword [rsi + 208], 37
    btr qword [rsi + 216], 56
    btr qword [rsi + 232], 11
    btr qword [rsi + 240], 30
    btr qword [rsi + 248], 49
    btr qword [rsi + 264], 4
    btr qword [rsi + 272], 23
    btr qword [rsi + 280], 42
    btr qword [rsi + 288], 61
    btr qword [rsi + 304], 16
    btr qword [rsi + 312], 35
    btr qword [rsi + 320], 54
    btr qword [rsi + 336], 9
    btr qword [rsi + 344], 28
    btr qword [rsi + 352], 47
    btr qword [rsi + 368], 2
    btr qword [rsi + 376], 21
    btr qword [rsi + 384], 40
    btr qword [rsi + 392], 59
    btr qword [rsi + 408], 14
    btr qword [rsi + 416], 33
    btr qword [rsi + 424], 52
    btr qword [rsi + 440], 7
    btr qword [rsi + 448], 26
    btr qword [rsi + 456], 45
    btr qword [rsi + 472], 0
    btr qword [rsi + 480], 19
    btr qword [rsi + 488], 38
    btr qword [rsi + 496], 57
    btr qword [rsi + 512], 12
    btr qword [rsi + 520], 31
    btr qword [rsi + 528], 50
    btr qword [rsi + 544], 5
    btr qword [rsi + 552], 24
    btr qword [rsi + 560], 43
    btr qword [rsi + 568], 62
    btr qword [rsi + 584], 17
    btr qword [rsi + 592], 36
    btr qword [rsi + 600], 55
    btr qword [rsi + 616], 10
    btr qword [rsi + 624], 29
    btr qword [rsi + 632], 48
    btr qword [rsi + 648], 3
    btr qword [rsi + 656], 22
    add rsi, 664
    sub ecx, 83
    cmp ecx, 83
    jae run_sieve_dense_loop_083
run_sieve_dense_tail_083:
    test ecx, ecx
    jz run_sieve_dense_restore_083
    cmp ecx, 1
    jb run_sieve_dense_restore_083
    btr qword [rsi], 41
    cmp ecx, 2
    jb run_sieve_dense_restore_083
    btr qword [rsi + 8], 60
    cmp ecx, 4
    jb run_sieve_dense_restore_083
    btr qword [rsi + 24], 15
    cmp ecx, 5
    jb run_sieve_dense_restore_083
    btr qword [rsi + 32], 34
    cmp ecx, 6
    jb run_sieve_dense_restore_083
    btr qword [rsi + 40], 53
    cmp ecx, 8
    jb run_sieve_dense_restore_083
    btr qword [rsi + 56], 8
    cmp ecx, 9
    jb run_sieve_dense_restore_083
    btr qword [rsi + 64], 27
    cmp ecx, 10
    jb run_sieve_dense_restore_083
    btr qword [rsi + 72], 46
    cmp ecx, 12
    jb run_sieve_dense_restore_083
    btr qword [rsi + 88], 1
    cmp ecx, 13
    jb run_sieve_dense_restore_083
    btr qword [rsi + 96], 20
    cmp ecx, 14
    jb run_sieve_dense_restore_083
    btr qword [rsi + 104], 39
    cmp ecx, 15
    jb run_sieve_dense_restore_083
    btr qword [rsi + 112], 58
    cmp ecx, 17
    jb run_sieve_dense_restore_083
    btr qword [rsi + 128], 13
    cmp ecx, 18
    jb run_sieve_dense_restore_083
    btr qword [rsi + 136], 32
    cmp ecx, 19
    jb run_sieve_dense_restore_083
    btr qword [rsi + 144], 51
    cmp ecx, 21
    jb run_sieve_dense_restore_083
    btr qword [rsi + 160], 6
    cmp ecx, 22
    jb run_sieve_dense_restore_083
    btr qword [rsi + 168], 25
    cmp ecx, 23
    jb run_sieve_dense_restore_083
    btr qword [rsi + 176], 44
    cmp ecx, 24
    jb run_sieve_dense_restore_083
    btr qword [rsi + 184], 63
    cmp ecx, 26
    jb run_sieve_dense_restore_083
    btr qword [rsi + 200], 18
    cmp ecx, 27
    jb run_sieve_dense_restore_083
    btr qword [rsi + 208], 37
    cmp ecx, 28
    jb run_sieve_dense_restore_083
    btr qword [rsi + 216], 56
    cmp ecx, 30
    jb run_sieve_dense_restore_083
    btr qword [rsi + 232], 11
    cmp ecx, 31
    jb run_sieve_dense_restore_083
    btr qword [rsi + 240], 30
    cmp ecx, 32
    jb run_sieve_dense_restore_083
    btr qword [rsi + 248], 49
    cmp ecx, 34
    jb run_sieve_dense_restore_083
    btr qword [rsi + 264], 4
    cmp ecx, 35
    jb run_sieve_dense_restore_083
    btr qword [rsi + 272], 23
    cmp ecx, 36
    jb run_sieve_dense_restore_083
    btr qword [rsi + 280], 42
    cmp ecx, 37
    jb run_sieve_dense_restore_083
    btr qword [rsi + 288], 61
    cmp ecx, 39
    jb run_sieve_dense_restore_083
    btr qword [rsi + 304], 16
    cmp ecx, 40
    jb run_sieve_dense_restore_083
    btr qword [rsi + 312], 35
    cmp ecx, 41
    jb run_sieve_dense_restore_083
    btr qword [rsi + 320], 54
    cmp ecx, 43
    jb run_sieve_dense_restore_083
    btr qword [rsi + 336], 9
    cmp ecx, 44
    jb run_sieve_dense_restore_083
    btr qword [rsi + 344], 28
    cmp ecx, 45
    jb run_sieve_dense_restore_083
    btr qword [rsi + 352], 47
    cmp ecx, 47
    jb run_sieve_dense_restore_083
    btr qword [rsi + 368], 2
    cmp ecx, 48
    jb run_sieve_dense_restore_083
    btr qword [rsi + 376], 21
    cmp ecx, 49
    jb run_sieve_dense_restore_083
    btr qword [rsi + 384], 40
    cmp ecx, 50
    jb run_sieve_dense_restore_083
    btr qword [rsi + 392], 59
    cmp ecx, 52
    jb run_sieve_dense_restore_083
    btr qword [rsi + 408], 14
    cmp ecx, 53
    jb run_sieve_dense_restore_083
    btr qword [rsi + 416], 33
    cmp ecx, 54
    jb run_sieve_dense_restore_083
    btr qword [rsi + 424], 52
    cmp ecx, 56
    jb run_sieve_dense_restore_083
    btr qword [rsi + 440], 7
    cmp ecx, 57
    jb run_sieve_dense_restore_083
    btr qword [rsi + 448], 26
    cmp ecx, 58
    jb run_sieve_dense_restore_083
    btr qword [rsi + 456], 45
    cmp ecx, 60
    jb run_sieve_dense_restore_083
    btr qword [rsi + 472], 0
    cmp ecx, 61
    jb run_sieve_dense_restore_083
    btr qword [rsi + 480], 19
    cmp ecx, 62
    jb run_sieve_dense_restore_083
    btr qword [rsi + 488], 38
    cmp ecx, 63
    jb run_sieve_dense_restore_083
    btr qword [rsi + 496], 57
    cmp ecx, 65
    jb run_sieve_dense_restore_083
    btr qword [rsi + 512], 12
    cmp ecx, 66
    jb run_sieve_dense_restore_083
    btr qword [rsi + 520], 31
    cmp ecx, 67
    jb run_sieve_dense_restore_083
    btr qword [rsi + 528], 50
    cmp ecx, 69
    jb run_sieve_dense_restore_083
    btr qword [rsi + 544], 5
    cmp ecx, 70
    jb run_sieve_dense_restore_083
    btr qword [rsi + 552], 24
    cmp ecx, 71
    jb run_sieve_dense_restore_083
    btr qword [rsi + 560], 43
    cmp ecx, 72
    jb run_sieve_dense_restore_083
    btr qword [rsi + 568], 62
    cmp ecx, 74
    jb run_sieve_dense_restore_083
    btr qword [rsi + 584], 17
    cmp ecx, 75
    jb run_sieve_dense_restore_083
    btr qword [rsi + 592], 36
    cmp ecx, 76
    jb run_sieve_dense_restore_083
    btr qword [rsi + 600], 55
    cmp ecx, 78
    jb run_sieve_dense_restore_083
    btr qword [rsi + 616], 10
    cmp ecx, 79
    jb run_sieve_dense_restore_083
    btr qword [rsi + 624], 29
    cmp ecx, 80
    jb run_sieve_dense_restore_083
    btr qword [rsi + 632], 48
    cmp ecx, 82
    jb run_sieve_dense_restore_083
    btr qword [rsi + 648], 3
    cmp ecx, 83
    jb run_sieve_dense_restore_083
    btr qword [rsi + 656], 22
run_sieve_dense_restore_083:
    bts qword [r14], 41
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_085:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 85
    jb run_sieve_dense_tail_085
align 16
run_sieve_dense_loop_085:
    btr qword [rsi], 42
    btr qword [rsi + 8], 63
    btr qword [rsi + 24], 20
    btr qword [rsi + 32], 41
    btr qword [rsi + 40], 62
    btr qword [rsi + 56], 19
    btr qword [rsi + 64], 40
    btr qword [rsi + 72], 61
    btr qword [rsi + 88], 18
    btr qword [rsi + 96], 39
    btr qword [rsi + 104], 60
    btr qword [rsi + 120], 17
    btr qword [rsi + 128], 38
    btr qword [rsi + 136], 59
    btr qword [rsi + 152], 16
    btr qword [rsi + 160], 37
    btr qword [rsi + 168], 58
    btr qword [rsi + 184], 15
    btr qword [rsi + 192], 36
    btr qword [rsi + 200], 57
    btr qword [rsi + 216], 14
    btr qword [rsi + 224], 35
    btr qword [rsi + 232], 56
    btr qword [rsi + 248], 13
    btr qword [rsi + 256], 34
    btr qword [rsi + 264], 55
    btr qword [rsi + 280], 12
    btr qword [rsi + 288], 33
    btr qword [rsi + 296], 54
    btr qword [rsi + 312], 11
    btr qword [rsi + 320], 32
    btr qword [rsi + 328], 53
    btr qword [rsi + 344], 10
    btr qword [rsi + 352], 31
    btr qword [rsi + 360], 52
    btr qword [rsi + 376], 9
    btr qword [rsi + 384], 30
    btr qword [rsi + 392], 51
    btr qword [rsi + 408], 8
    btr qword [rsi + 416], 29
    btr qword [rsi + 424], 50
    btr qword [rsi + 440], 7
    btr qword [rsi + 448], 28
    btr qword [rsi + 456], 49
    btr qword [rsi + 472], 6
    btr qword [rsi + 480], 27
    btr qword [rsi + 488], 48
    btr qword [rsi + 504], 5
    btr qword [rsi + 512], 26
    btr qword [rsi + 520], 47
    btr qword [rsi + 536], 4
    btr qword [rsi + 544], 25
    btr qword [rsi + 552], 46
    btr qword [rsi + 568], 3
    btr qword [rsi + 576], 24
    btr qword [rsi + 584], 45
    btr qword [rsi + 600], 2
    btr qword [rsi + 608], 23
    btr qword [rsi + 616], 44
    btr qword [rsi + 632], 1
    btr qword [rsi + 640], 22
    btr qword [rsi + 648], 43
    btr qword [rsi + 664], 0
    btr qword [rsi + 672], 21
    add rsi, 680
    sub ecx, 85
    cmp ecx, 85
    jae run_sieve_dense_loop_085
run_sieve_dense_tail_085:
    test ecx, ecx
    jz run_sieve_dense_restore_085
    cmp ecx, 1
    jb run_sieve_dense_restore_085
    btr qword [rsi], 42
    cmp ecx, 2
    jb run_sieve_dense_restore_085
    btr qword [rsi + 8], 63
    cmp ecx, 4
    jb run_sieve_dense_restore_085
    btr qword [rsi + 24], 20
    cmp ecx, 5
    jb run_sieve_dense_restore_085
    btr qword [rsi + 32], 41
    cmp ecx, 6
    jb run_sieve_dense_restore_085
    btr qword [rsi + 40], 62
    cmp ecx, 8
    jb run_sieve_dense_restore_085
    btr qword [rsi + 56], 19
    cmp ecx, 9
    jb run_sieve_dense_restore_085
    btr qword [rsi + 64], 40
    cmp ecx, 10
    jb run_sieve_dense_restore_085
    btr qword [rsi + 72], 61
    cmp ecx, 12
    jb run_sieve_dense_restore_085
    btr qword [rsi + 88], 18
    cmp ecx, 13
    jb run_sieve_dense_restore_085
    btr qword [rsi + 96], 39
    cmp ecx, 14
    jb run_sieve_dense_restore_085
    btr qword [rsi + 104], 60
    cmp ecx, 16
    jb run_sieve_dense_restore_085
    btr qword [rsi + 120], 17
    cmp ecx, 17
    jb run_sieve_dense_restore_085
    btr qword [rsi + 128], 38
    cmp ecx, 18
    jb run_sieve_dense_restore_085
    btr qword [rsi + 136], 59
    cmp ecx, 20
    jb run_sieve_dense_restore_085
    btr qword [rsi + 152], 16
    cmp ecx, 21
    jb run_sieve_dense_restore_085
    btr qword [rsi + 160], 37
    cmp ecx, 22
    jb run_sieve_dense_restore_085
    btr qword [rsi + 168], 58
    cmp ecx, 24
    jb run_sieve_dense_restore_085
    btr qword [rsi + 184], 15
    cmp ecx, 25
    jb run_sieve_dense_restore_085
    btr qword [rsi + 192], 36
    cmp ecx, 26
    jb run_sieve_dense_restore_085
    btr qword [rsi + 200], 57
    cmp ecx, 28
    jb run_sieve_dense_restore_085
    btr qword [rsi + 216], 14
    cmp ecx, 29
    jb run_sieve_dense_restore_085
    btr qword [rsi + 224], 35
    cmp ecx, 30
    jb run_sieve_dense_restore_085
    btr qword [rsi + 232], 56
    cmp ecx, 32
    jb run_sieve_dense_restore_085
    btr qword [rsi + 248], 13
    cmp ecx, 33
    jb run_sieve_dense_restore_085
    btr qword [rsi + 256], 34
    cmp ecx, 34
    jb run_sieve_dense_restore_085
    btr qword [rsi + 264], 55
    cmp ecx, 36
    jb run_sieve_dense_restore_085
    btr qword [rsi + 280], 12
    cmp ecx, 37
    jb run_sieve_dense_restore_085
    btr qword [rsi + 288], 33
    cmp ecx, 38
    jb run_sieve_dense_restore_085
    btr qword [rsi + 296], 54
    cmp ecx, 40
    jb run_sieve_dense_restore_085
    btr qword [rsi + 312], 11
    cmp ecx, 41
    jb run_sieve_dense_restore_085
    btr qword [rsi + 320], 32
    cmp ecx, 42
    jb run_sieve_dense_restore_085
    btr qword [rsi + 328], 53
    cmp ecx, 44
    jb run_sieve_dense_restore_085
    btr qword [rsi + 344], 10
    cmp ecx, 45
    jb run_sieve_dense_restore_085
    btr qword [rsi + 352], 31
    cmp ecx, 46
    jb run_sieve_dense_restore_085
    btr qword [rsi + 360], 52
    cmp ecx, 48
    jb run_sieve_dense_restore_085
    btr qword [rsi + 376], 9
    cmp ecx, 49
    jb run_sieve_dense_restore_085
    btr qword [rsi + 384], 30
    cmp ecx, 50
    jb run_sieve_dense_restore_085
    btr qword [rsi + 392], 51
    cmp ecx, 52
    jb run_sieve_dense_restore_085
    btr qword [rsi + 408], 8
    cmp ecx, 53
    jb run_sieve_dense_restore_085
    btr qword [rsi + 416], 29
    cmp ecx, 54
    jb run_sieve_dense_restore_085
    btr qword [rsi + 424], 50
    cmp ecx, 56
    jb run_sieve_dense_restore_085
    btr qword [rsi + 440], 7
    cmp ecx, 57
    jb run_sieve_dense_restore_085
    btr qword [rsi + 448], 28
    cmp ecx, 58
    jb run_sieve_dense_restore_085
    btr qword [rsi + 456], 49
    cmp ecx, 60
    jb run_sieve_dense_restore_085
    btr qword [rsi + 472], 6
    cmp ecx, 61
    jb run_sieve_dense_restore_085
    btr qword [rsi + 480], 27
    cmp ecx, 62
    jb run_sieve_dense_restore_085
    btr qword [rsi + 488], 48
    cmp ecx, 64
    jb run_sieve_dense_restore_085
    btr qword [rsi + 504], 5
    cmp ecx, 65
    jb run_sieve_dense_restore_085
    btr qword [rsi + 512], 26
    cmp ecx, 66
    jb run_sieve_dense_restore_085
    btr qword [rsi + 520], 47
    cmp ecx, 68
    jb run_sieve_dense_restore_085
    btr qword [rsi + 536], 4
    cmp ecx, 69
    jb run_sieve_dense_restore_085
    btr qword [rsi + 544], 25
    cmp ecx, 70
    jb run_sieve_dense_restore_085
    btr qword [rsi + 552], 46
    cmp ecx, 72
    jb run_sieve_dense_restore_085
    btr qword [rsi + 568], 3
    cmp ecx, 73
    jb run_sieve_dense_restore_085
    btr qword [rsi + 576], 24
    cmp ecx, 74
    jb run_sieve_dense_restore_085
    btr qword [rsi + 584], 45
    cmp ecx, 76
    jb run_sieve_dense_restore_085
    btr qword [rsi + 600], 2
    cmp ecx, 77
    jb run_sieve_dense_restore_085
    btr qword [rsi + 608], 23
    cmp ecx, 78
    jb run_sieve_dense_restore_085
    btr qword [rsi + 616], 44
    cmp ecx, 80
    jb run_sieve_dense_restore_085
    btr qword [rsi + 632], 1
    cmp ecx, 81
    jb run_sieve_dense_restore_085
    btr qword [rsi + 640], 22
    cmp ecx, 82
    jb run_sieve_dense_restore_085
    btr qword [rsi + 648], 43
    cmp ecx, 84
    jb run_sieve_dense_restore_085
    btr qword [rsi + 664], 0
    cmp ecx, 85
    jb run_sieve_dense_restore_085
    btr qword [rsi + 672], 21
run_sieve_dense_restore_085:
    bts qword [r14], 42
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_087:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 87
    jb run_sieve_dense_tail_087
align 16
run_sieve_dense_loop_087:
    btr qword [rsi], 43
    btr qword [rsi + 16], 2
    btr qword [rsi + 24], 25
    btr qword [rsi + 32], 48
    btr qword [rsi + 48], 7
    btr qword [rsi + 56], 30
    btr qword [rsi + 64], 53
    btr qword [rsi + 80], 12
    btr qword [rsi + 88], 35
    btr qword [rsi + 96], 58
    btr qword [rsi + 112], 17
    btr qword [rsi + 120], 40
    btr qword [rsi + 128], 63
    btr qword [rsi + 144], 22
    btr qword [rsi + 152], 45
    btr qword [rsi + 168], 4
    btr qword [rsi + 176], 27
    btr qword [rsi + 184], 50
    btr qword [rsi + 200], 9
    btr qword [rsi + 208], 32
    btr qword [rsi + 216], 55
    btr qword [rsi + 232], 14
    btr qword [rsi + 240], 37
    btr qword [rsi + 248], 60
    btr qword [rsi + 264], 19
    btr qword [rsi + 272], 42
    btr qword [rsi + 288], 1
    btr qword [rsi + 296], 24
    btr qword [rsi + 304], 47
    btr qword [rsi + 320], 6
    btr qword [rsi + 328], 29
    btr qword [rsi + 336], 52
    btr qword [rsi + 352], 11
    btr qword [rsi + 360], 34
    btr qword [rsi + 368], 57
    btr qword [rsi + 384], 16
    btr qword [rsi + 392], 39
    btr qword [rsi + 400], 62
    btr qword [rsi + 416], 21
    btr qword [rsi + 424], 44
    btr qword [rsi + 440], 3
    btr qword [rsi + 448], 26
    btr qword [rsi + 456], 49
    btr qword [rsi + 472], 8
    btr qword [rsi + 480], 31
    btr qword [rsi + 488], 54
    btr qword [rsi + 504], 13
    btr qword [rsi + 512], 36
    btr qword [rsi + 520], 59
    btr qword [rsi + 536], 18
    btr qword [rsi + 544], 41
    btr qword [rsi + 560], 0
    btr qword [rsi + 568], 23
    btr qword [rsi + 576], 46
    btr qword [rsi + 592], 5
    btr qword [rsi + 600], 28
    btr qword [rsi + 608], 51
    btr qword [rsi + 624], 10
    btr qword [rsi + 632], 33
    btr qword [rsi + 640], 56
    btr qword [rsi + 656], 15
    btr qword [rsi + 664], 38
    btr qword [rsi + 672], 61
    btr qword [rsi + 688], 20
    add rsi, 696
    sub ecx, 87
    cmp ecx, 87
    jae run_sieve_dense_loop_087
run_sieve_dense_tail_087:
    test ecx, ecx
    jz run_sieve_dense_restore_087
    cmp ecx, 1
    jb run_sieve_dense_restore_087
    btr qword [rsi], 43
    cmp ecx, 3
    jb run_sieve_dense_restore_087
    btr qword [rsi + 16], 2
    cmp ecx, 4
    jb run_sieve_dense_restore_087
    btr qword [rsi + 24], 25
    cmp ecx, 5
    jb run_sieve_dense_restore_087
    btr qword [rsi + 32], 48
    cmp ecx, 7
    jb run_sieve_dense_restore_087
    btr qword [rsi + 48], 7
    cmp ecx, 8
    jb run_sieve_dense_restore_087
    btr qword [rsi + 56], 30
    cmp ecx, 9
    jb run_sieve_dense_restore_087
    btr qword [rsi + 64], 53
    cmp ecx, 11
    jb run_sieve_dense_restore_087
    btr qword [rsi + 80], 12
    cmp ecx, 12
    jb run_sieve_dense_restore_087
    btr qword [rsi + 88], 35
    cmp ecx, 13
    jb run_sieve_dense_restore_087
    btr qword [rsi + 96], 58
    cmp ecx, 15
    jb run_sieve_dense_restore_087
    btr qword [rsi + 112], 17
    cmp ecx, 16
    jb run_sieve_dense_restore_087
    btr qword [rsi + 120], 40
    cmp ecx, 17
    jb run_sieve_dense_restore_087
    btr qword [rsi + 128], 63
    cmp ecx, 19
    jb run_sieve_dense_restore_087
    btr qword [rsi + 144], 22
    cmp ecx, 20
    jb run_sieve_dense_restore_087
    btr qword [rsi + 152], 45
    cmp ecx, 22
    jb run_sieve_dense_restore_087
    btr qword [rsi + 168], 4
    cmp ecx, 23
    jb run_sieve_dense_restore_087
    btr qword [rsi + 176], 27
    cmp ecx, 24
    jb run_sieve_dense_restore_087
    btr qword [rsi + 184], 50
    cmp ecx, 26
    jb run_sieve_dense_restore_087
    btr qword [rsi + 200], 9
    cmp ecx, 27
    jb run_sieve_dense_restore_087
    btr qword [rsi + 208], 32
    cmp ecx, 28
    jb run_sieve_dense_restore_087
    btr qword [rsi + 216], 55
    cmp ecx, 30
    jb run_sieve_dense_restore_087
    btr qword [rsi + 232], 14
    cmp ecx, 31
    jb run_sieve_dense_restore_087
    btr qword [rsi + 240], 37
    cmp ecx, 32
    jb run_sieve_dense_restore_087
    btr qword [rsi + 248], 60
    cmp ecx, 34
    jb run_sieve_dense_restore_087
    btr qword [rsi + 264], 19
    cmp ecx, 35
    jb run_sieve_dense_restore_087
    btr qword [rsi + 272], 42
    cmp ecx, 37
    jb run_sieve_dense_restore_087
    btr qword [rsi + 288], 1
    cmp ecx, 38
    jb run_sieve_dense_restore_087
    btr qword [rsi + 296], 24
    cmp ecx, 39
    jb run_sieve_dense_restore_087
    btr qword [rsi + 304], 47
    cmp ecx, 41
    jb run_sieve_dense_restore_087
    btr qword [rsi + 320], 6
    cmp ecx, 42
    jb run_sieve_dense_restore_087
    btr qword [rsi + 328], 29
    cmp ecx, 43
    jb run_sieve_dense_restore_087
    btr qword [rsi + 336], 52
    cmp ecx, 45
    jb run_sieve_dense_restore_087
    btr qword [rsi + 352], 11
    cmp ecx, 46
    jb run_sieve_dense_restore_087
    btr qword [rsi + 360], 34
    cmp ecx, 47
    jb run_sieve_dense_restore_087
    btr qword [rsi + 368], 57
    cmp ecx, 49
    jb run_sieve_dense_restore_087
    btr qword [rsi + 384], 16
    cmp ecx, 50
    jb run_sieve_dense_restore_087
    btr qword [rsi + 392], 39
    cmp ecx, 51
    jb run_sieve_dense_restore_087
    btr qword [rsi + 400], 62
    cmp ecx, 53
    jb run_sieve_dense_restore_087
    btr qword [rsi + 416], 21
    cmp ecx, 54
    jb run_sieve_dense_restore_087
    btr qword [rsi + 424], 44
    cmp ecx, 56
    jb run_sieve_dense_restore_087
    btr qword [rsi + 440], 3
    cmp ecx, 57
    jb run_sieve_dense_restore_087
    btr qword [rsi + 448], 26
    cmp ecx, 58
    jb run_sieve_dense_restore_087
    btr qword [rsi + 456], 49
    cmp ecx, 60
    jb run_sieve_dense_restore_087
    btr qword [rsi + 472], 8
    cmp ecx, 61
    jb run_sieve_dense_restore_087
    btr qword [rsi + 480], 31
    cmp ecx, 62
    jb run_sieve_dense_restore_087
    btr qword [rsi + 488], 54
    cmp ecx, 64
    jb run_sieve_dense_restore_087
    btr qword [rsi + 504], 13
    cmp ecx, 65
    jb run_sieve_dense_restore_087
    btr qword [rsi + 512], 36
    cmp ecx, 66
    jb run_sieve_dense_restore_087
    btr qword [rsi + 520], 59
    cmp ecx, 68
    jb run_sieve_dense_restore_087
    btr qword [rsi + 536], 18
    cmp ecx, 69
    jb run_sieve_dense_restore_087
    btr qword [rsi + 544], 41
    cmp ecx, 71
    jb run_sieve_dense_restore_087
    btr qword [rsi + 560], 0
    cmp ecx, 72
    jb run_sieve_dense_restore_087
    btr qword [rsi + 568], 23
    cmp ecx, 73
    jb run_sieve_dense_restore_087
    btr qword [rsi + 576], 46
    cmp ecx, 75
    jb run_sieve_dense_restore_087
    btr qword [rsi + 592], 5
    cmp ecx, 76
    jb run_sieve_dense_restore_087
    btr qword [rsi + 600], 28
    cmp ecx, 77
    jb run_sieve_dense_restore_087
    btr qword [rsi + 608], 51
    cmp ecx, 79
    jb run_sieve_dense_restore_087
    btr qword [rsi + 624], 10
    cmp ecx, 80
    jb run_sieve_dense_restore_087
    btr qword [rsi + 632], 33
    cmp ecx, 81
    jb run_sieve_dense_restore_087
    btr qword [rsi + 640], 56
    cmp ecx, 83
    jb run_sieve_dense_restore_087
    btr qword [rsi + 656], 15
    cmp ecx, 84
    jb run_sieve_dense_restore_087
    btr qword [rsi + 664], 38
    cmp ecx, 85
    jb run_sieve_dense_restore_087
    btr qword [rsi + 672], 61
    cmp ecx, 87
    jb run_sieve_dense_restore_087
    btr qword [rsi + 688], 20
run_sieve_dense_restore_087:
    bts qword [r14], 43
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_089:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 89
    jb run_sieve_dense_tail_089
align 16
run_sieve_dense_loop_089:
    btr qword [rsi], 44
    btr qword [rsi + 16], 5
    btr qword [rsi + 24], 30
    btr qword [rsi + 32], 55
    btr qword [rsi + 48], 16
    btr qword [rsi + 56], 41
    btr qword [rsi + 72], 2
    btr qword [rsi + 80], 27
    btr qword [rsi + 88], 52
    btr qword [rsi + 104], 13
    btr qword [rsi + 112], 38
    btr qword [rsi + 120], 63
    btr qword [rsi + 136], 24
    btr qword [rsi + 144], 49
    btr qword [rsi + 160], 10
    btr qword [rsi + 168], 35
    btr qword [rsi + 176], 60
    btr qword [rsi + 192], 21
    btr qword [rsi + 200], 46
    btr qword [rsi + 216], 7
    btr qword [rsi + 224], 32
    btr qword [rsi + 232], 57
    btr qword [rsi + 248], 18
    btr qword [rsi + 256], 43
    btr qword [rsi + 272], 4
    btr qword [rsi + 280], 29
    btr qword [rsi + 288], 54
    btr qword [rsi + 304], 15
    btr qword [rsi + 312], 40
    btr qword [rsi + 328], 1
    btr qword [rsi + 336], 26
    btr qword [rsi + 344], 51
    btr qword [rsi + 360], 12
    btr qword [rsi + 368], 37
    btr qword [rsi + 376], 62
    btr qword [rsi + 392], 23
    btr qword [rsi + 400], 48
    btr qword [rsi + 416], 9
    btr qword [rsi + 424], 34
    btr qword [rsi + 432], 59
    btr qword [rsi + 448], 20
    btr qword [rsi + 456], 45
    btr qword [rsi + 472], 6
    btr qword [rsi + 480], 31
    btr qword [rsi + 488], 56
    btr qword [rsi + 504], 17
    btr qword [rsi + 512], 42
    btr qword [rsi + 528], 3
    btr qword [rsi + 536], 28
    btr qword [rsi + 544], 53
    btr qword [rsi + 560], 14
    btr qword [rsi + 568], 39
    btr qword [rsi + 584], 0
    btr qword [rsi + 592], 25
    btr qword [rsi + 600], 50
    btr qword [rsi + 616], 11
    btr qword [rsi + 624], 36
    btr qword [rsi + 632], 61
    btr qword [rsi + 648], 22
    btr qword [rsi + 656], 47
    btr qword [rsi + 672], 8
    btr qword [rsi + 680], 33
    btr qword [rsi + 688], 58
    btr qword [rsi + 704], 19
    add rsi, 712
    sub ecx, 89
    cmp ecx, 89
    jae run_sieve_dense_loop_089
run_sieve_dense_tail_089:
    test ecx, ecx
    jz run_sieve_dense_restore_089
    cmp ecx, 1
    jb run_sieve_dense_restore_089
    btr qword [rsi], 44
    cmp ecx, 3
    jb run_sieve_dense_restore_089
    btr qword [rsi + 16], 5
    cmp ecx, 4
    jb run_sieve_dense_restore_089
    btr qword [rsi + 24], 30
    cmp ecx, 5
    jb run_sieve_dense_restore_089
    btr qword [rsi + 32], 55
    cmp ecx, 7
    jb run_sieve_dense_restore_089
    btr qword [rsi + 48], 16
    cmp ecx, 8
    jb run_sieve_dense_restore_089
    btr qword [rsi + 56], 41
    cmp ecx, 10
    jb run_sieve_dense_restore_089
    btr qword [rsi + 72], 2
    cmp ecx, 11
    jb run_sieve_dense_restore_089
    btr qword [rsi + 80], 27
    cmp ecx, 12
    jb run_sieve_dense_restore_089
    btr qword [rsi + 88], 52
    cmp ecx, 14
    jb run_sieve_dense_restore_089
    btr qword [rsi + 104], 13
    cmp ecx, 15
    jb run_sieve_dense_restore_089
    btr qword [rsi + 112], 38
    cmp ecx, 16
    jb run_sieve_dense_restore_089
    btr qword [rsi + 120], 63
    cmp ecx, 18
    jb run_sieve_dense_restore_089
    btr qword [rsi + 136], 24
    cmp ecx, 19
    jb run_sieve_dense_restore_089
    btr qword [rsi + 144], 49
    cmp ecx, 21
    jb run_sieve_dense_restore_089
    btr qword [rsi + 160], 10
    cmp ecx, 22
    jb run_sieve_dense_restore_089
    btr qword [rsi + 168], 35
    cmp ecx, 23
    jb run_sieve_dense_restore_089
    btr qword [rsi + 176], 60
    cmp ecx, 25
    jb run_sieve_dense_restore_089
    btr qword [rsi + 192], 21
    cmp ecx, 26
    jb run_sieve_dense_restore_089
    btr qword [rsi + 200], 46
    cmp ecx, 28
    jb run_sieve_dense_restore_089
    btr qword [rsi + 216], 7
    cmp ecx, 29
    jb run_sieve_dense_restore_089
    btr qword [rsi + 224], 32
    cmp ecx, 30
    jb run_sieve_dense_restore_089
    btr qword [rsi + 232], 57
    cmp ecx, 32
    jb run_sieve_dense_restore_089
    btr qword [rsi + 248], 18
    cmp ecx, 33
    jb run_sieve_dense_restore_089
    btr qword [rsi + 256], 43
    cmp ecx, 35
    jb run_sieve_dense_restore_089
    btr qword [rsi + 272], 4
    cmp ecx, 36
    jb run_sieve_dense_restore_089
    btr qword [rsi + 280], 29
    cmp ecx, 37
    jb run_sieve_dense_restore_089
    btr qword [rsi + 288], 54
    cmp ecx, 39
    jb run_sieve_dense_restore_089
    btr qword [rsi + 304], 15
    cmp ecx, 40
    jb run_sieve_dense_restore_089
    btr qword [rsi + 312], 40
    cmp ecx, 42
    jb run_sieve_dense_restore_089
    btr qword [rsi + 328], 1
    cmp ecx, 43
    jb run_sieve_dense_restore_089
    btr qword [rsi + 336], 26
    cmp ecx, 44
    jb run_sieve_dense_restore_089
    btr qword [rsi + 344], 51
    cmp ecx, 46
    jb run_sieve_dense_restore_089
    btr qword [rsi + 360], 12
    cmp ecx, 47
    jb run_sieve_dense_restore_089
    btr qword [rsi + 368], 37
    cmp ecx, 48
    jb run_sieve_dense_restore_089
    btr qword [rsi + 376], 62
    cmp ecx, 50
    jb run_sieve_dense_restore_089
    btr qword [rsi + 392], 23
    cmp ecx, 51
    jb run_sieve_dense_restore_089
    btr qword [rsi + 400], 48
    cmp ecx, 53
    jb run_sieve_dense_restore_089
    btr qword [rsi + 416], 9
    cmp ecx, 54
    jb run_sieve_dense_restore_089
    btr qword [rsi + 424], 34
    cmp ecx, 55
    jb run_sieve_dense_restore_089
    btr qword [rsi + 432], 59
    cmp ecx, 57
    jb run_sieve_dense_restore_089
    btr qword [rsi + 448], 20
    cmp ecx, 58
    jb run_sieve_dense_restore_089
    btr qword [rsi + 456], 45
    cmp ecx, 60
    jb run_sieve_dense_restore_089
    btr qword [rsi + 472], 6
    cmp ecx, 61
    jb run_sieve_dense_restore_089
    btr qword [rsi + 480], 31
    cmp ecx, 62
    jb run_sieve_dense_restore_089
    btr qword [rsi + 488], 56
    cmp ecx, 64
    jb run_sieve_dense_restore_089
    btr qword [rsi + 504], 17
    cmp ecx, 65
    jb run_sieve_dense_restore_089
    btr qword [rsi + 512], 42
    cmp ecx, 67
    jb run_sieve_dense_restore_089
    btr qword [rsi + 528], 3
    cmp ecx, 68
    jb run_sieve_dense_restore_089
    btr qword [rsi + 536], 28
    cmp ecx, 69
    jb run_sieve_dense_restore_089
    btr qword [rsi + 544], 53
    cmp ecx, 71
    jb run_sieve_dense_restore_089
    btr qword [rsi + 560], 14
    cmp ecx, 72
    jb run_sieve_dense_restore_089
    btr qword [rsi + 568], 39
    cmp ecx, 74
    jb run_sieve_dense_restore_089
    btr qword [rsi + 584], 0
    cmp ecx, 75
    jb run_sieve_dense_restore_089
    btr qword [rsi + 592], 25
    cmp ecx, 76
    jb run_sieve_dense_restore_089
    btr qword [rsi + 600], 50
    cmp ecx, 78
    jb run_sieve_dense_restore_089
    btr qword [rsi + 616], 11
    cmp ecx, 79
    jb run_sieve_dense_restore_089
    btr qword [rsi + 624], 36
    cmp ecx, 80
    jb run_sieve_dense_restore_089
    btr qword [rsi + 632], 61
    cmp ecx, 82
    jb run_sieve_dense_restore_089
    btr qword [rsi + 648], 22
    cmp ecx, 83
    jb run_sieve_dense_restore_089
    btr qword [rsi + 656], 47
    cmp ecx, 85
    jb run_sieve_dense_restore_089
    btr qword [rsi + 672], 8
    cmp ecx, 86
    jb run_sieve_dense_restore_089
    btr qword [rsi + 680], 33
    cmp ecx, 87
    jb run_sieve_dense_restore_089
    btr qword [rsi + 688], 58
    cmp ecx, 89
    jb run_sieve_dense_restore_089
    btr qword [rsi + 704], 19
run_sieve_dense_restore_089:
    bts qword [r14], 44
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_091:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 91
    jb run_sieve_dense_tail_091
align 16
run_sieve_dense_loop_091:
    btr qword [rsi], 45
    btr qword [rsi + 16], 8
    btr qword [rsi + 24], 35
    btr qword [rsi + 32], 62
    btr qword [rsi + 48], 25
    btr qword [rsi + 56], 52
    btr qword [rsi + 72], 15
    btr qword [rsi + 80], 42
    btr qword [rsi + 96], 5
    btr qword [rsi + 104], 32
    btr qword [rsi + 112], 59
    btr qword [rsi + 128], 22
    btr qword [rsi + 136], 49
    btr qword [rsi + 152], 12
    btr qword [rsi + 160], 39
    btr qword [rsi + 176], 2
    btr qword [rsi + 184], 29
    btr qword [rsi + 192], 56
    btr qword [rsi + 208], 19
    btr qword [rsi + 216], 46
    btr qword [rsi + 232], 9
    btr qword [rsi + 240], 36
    btr qword [rsi + 248], 63
    btr qword [rsi + 264], 26
    btr qword [rsi + 272], 53
    btr qword [rsi + 288], 16
    btr qword [rsi + 296], 43
    btr qword [rsi + 312], 6
    btr qword [rsi + 320], 33
    btr qword [rsi + 328], 60
    btr qword [rsi + 344], 23
    btr qword [rsi + 352], 50
    btr qword [rsi + 368], 13
    btr qword [rsi + 376], 40
    btr qword [rsi + 392], 3
    btr qword [rsi + 400], 30
    btr qword [rsi + 408], 57
    btr qword [rsi + 424], 20
    btr qword [rsi + 432], 47
    btr qword [rsi + 448], 10
    btr qword [rsi + 456], 37
    btr qword [rsi + 472], 0
    btr qword [rsi + 480], 27
    btr qword [rsi + 488], 54
    btr qword [rsi + 504], 17
    btr qword [rsi + 512], 44
    btr qword [rsi + 528], 7
    btr qword [rsi + 536], 34
    btr qword [rsi + 544], 61
    btr qword [rsi + 560], 24
    btr qword [rsi + 568], 51
    btr qword [rsi + 584], 14
    btr qword [rsi + 592], 41
    btr qword [rsi + 608], 4
    btr qword [rsi + 616], 31
    btr qword [rsi + 624], 58
    btr qword [rsi + 640], 21
    btr qword [rsi + 648], 48
    btr qword [rsi + 664], 11
    btr qword [rsi + 672], 38
    btr qword [rsi + 688], 1
    btr qword [rsi + 696], 28
    btr qword [rsi + 704], 55
    btr qword [rsi + 720], 18
    add rsi, 728
    sub ecx, 91
    cmp ecx, 91
    jae run_sieve_dense_loop_091
run_sieve_dense_tail_091:
    test ecx, ecx
    jz run_sieve_dense_restore_091
    cmp ecx, 1
    jb run_sieve_dense_restore_091
    btr qword [rsi], 45
    cmp ecx, 3
    jb run_sieve_dense_restore_091
    btr qword [rsi + 16], 8
    cmp ecx, 4
    jb run_sieve_dense_restore_091
    btr qword [rsi + 24], 35
    cmp ecx, 5
    jb run_sieve_dense_restore_091
    btr qword [rsi + 32], 62
    cmp ecx, 7
    jb run_sieve_dense_restore_091
    btr qword [rsi + 48], 25
    cmp ecx, 8
    jb run_sieve_dense_restore_091
    btr qword [rsi + 56], 52
    cmp ecx, 10
    jb run_sieve_dense_restore_091
    btr qword [rsi + 72], 15
    cmp ecx, 11
    jb run_sieve_dense_restore_091
    btr qword [rsi + 80], 42
    cmp ecx, 13
    jb run_sieve_dense_restore_091
    btr qword [rsi + 96], 5
    cmp ecx, 14
    jb run_sieve_dense_restore_091
    btr qword [rsi + 104], 32
    cmp ecx, 15
    jb run_sieve_dense_restore_091
    btr qword [rsi + 112], 59
    cmp ecx, 17
    jb run_sieve_dense_restore_091
    btr qword [rsi + 128], 22
    cmp ecx, 18
    jb run_sieve_dense_restore_091
    btr qword [rsi + 136], 49
    cmp ecx, 20
    jb run_sieve_dense_restore_091
    btr qword [rsi + 152], 12
    cmp ecx, 21
    jb run_sieve_dense_restore_091
    btr qword [rsi + 160], 39
    cmp ecx, 23
    jb run_sieve_dense_restore_091
    btr qword [rsi + 176], 2
    cmp ecx, 24
    jb run_sieve_dense_restore_091
    btr qword [rsi + 184], 29
    cmp ecx, 25
    jb run_sieve_dense_restore_091
    btr qword [rsi + 192], 56
    cmp ecx, 27
    jb run_sieve_dense_restore_091
    btr qword [rsi + 208], 19
    cmp ecx, 28
    jb run_sieve_dense_restore_091
    btr qword [rsi + 216], 46
    cmp ecx, 30
    jb run_sieve_dense_restore_091
    btr qword [rsi + 232], 9
    cmp ecx, 31
    jb run_sieve_dense_restore_091
    btr qword [rsi + 240], 36
    cmp ecx, 32
    jb run_sieve_dense_restore_091
    btr qword [rsi + 248], 63
    cmp ecx, 34
    jb run_sieve_dense_restore_091
    btr qword [rsi + 264], 26
    cmp ecx, 35
    jb run_sieve_dense_restore_091
    btr qword [rsi + 272], 53
    cmp ecx, 37
    jb run_sieve_dense_restore_091
    btr qword [rsi + 288], 16
    cmp ecx, 38
    jb run_sieve_dense_restore_091
    btr qword [rsi + 296], 43
    cmp ecx, 40
    jb run_sieve_dense_restore_091
    btr qword [rsi + 312], 6
    cmp ecx, 41
    jb run_sieve_dense_restore_091
    btr qword [rsi + 320], 33
    cmp ecx, 42
    jb run_sieve_dense_restore_091
    btr qword [rsi + 328], 60
    cmp ecx, 44
    jb run_sieve_dense_restore_091
    btr qword [rsi + 344], 23
    cmp ecx, 45
    jb run_sieve_dense_restore_091
    btr qword [rsi + 352], 50
    cmp ecx, 47
    jb run_sieve_dense_restore_091
    btr qword [rsi + 368], 13
    cmp ecx, 48
    jb run_sieve_dense_restore_091
    btr qword [rsi + 376], 40
    cmp ecx, 50
    jb run_sieve_dense_restore_091
    btr qword [rsi + 392], 3
    cmp ecx, 51
    jb run_sieve_dense_restore_091
    btr qword [rsi + 400], 30
    cmp ecx, 52
    jb run_sieve_dense_restore_091
    btr qword [rsi + 408], 57
    cmp ecx, 54
    jb run_sieve_dense_restore_091
    btr qword [rsi + 424], 20
    cmp ecx, 55
    jb run_sieve_dense_restore_091
    btr qword [rsi + 432], 47
    cmp ecx, 57
    jb run_sieve_dense_restore_091
    btr qword [rsi + 448], 10
    cmp ecx, 58
    jb run_sieve_dense_restore_091
    btr qword [rsi + 456], 37
    cmp ecx, 60
    jb run_sieve_dense_restore_091
    btr qword [rsi + 472], 0
    cmp ecx, 61
    jb run_sieve_dense_restore_091
    btr qword [rsi + 480], 27
    cmp ecx, 62
    jb run_sieve_dense_restore_091
    btr qword [rsi + 488], 54
    cmp ecx, 64
    jb run_sieve_dense_restore_091
    btr qword [rsi + 504], 17
    cmp ecx, 65
    jb run_sieve_dense_restore_091
    btr qword [rsi + 512], 44
    cmp ecx, 67
    jb run_sieve_dense_restore_091
    btr qword [rsi + 528], 7
    cmp ecx, 68
    jb run_sieve_dense_restore_091
    btr qword [rsi + 536], 34
    cmp ecx, 69
    jb run_sieve_dense_restore_091
    btr qword [rsi + 544], 61
    cmp ecx, 71
    jb run_sieve_dense_restore_091
    btr qword [rsi + 560], 24
    cmp ecx, 72
    jb run_sieve_dense_restore_091
    btr qword [rsi + 568], 51
    cmp ecx, 74
    jb run_sieve_dense_restore_091
    btr qword [rsi + 584], 14
    cmp ecx, 75
    jb run_sieve_dense_restore_091
    btr qword [rsi + 592], 41
    cmp ecx, 77
    jb run_sieve_dense_restore_091
    btr qword [rsi + 608], 4
    cmp ecx, 78
    jb run_sieve_dense_restore_091
    btr qword [rsi + 616], 31
    cmp ecx, 79
    jb run_sieve_dense_restore_091
    btr qword [rsi + 624], 58
    cmp ecx, 81
    jb run_sieve_dense_restore_091
    btr qword [rsi + 640], 21
    cmp ecx, 82
    jb run_sieve_dense_restore_091
    btr qword [rsi + 648], 48
    cmp ecx, 84
    jb run_sieve_dense_restore_091
    btr qword [rsi + 664], 11
    cmp ecx, 85
    jb run_sieve_dense_restore_091
    btr qword [rsi + 672], 38
    cmp ecx, 87
    jb run_sieve_dense_restore_091
    btr qword [rsi + 688], 1
    cmp ecx, 88
    jb run_sieve_dense_restore_091
    btr qword [rsi + 696], 28
    cmp ecx, 89
    jb run_sieve_dense_restore_091
    btr qword [rsi + 704], 55
    cmp ecx, 91
    jb run_sieve_dense_restore_091
    btr qword [rsi + 720], 18
run_sieve_dense_restore_091:
    bts qword [r14], 45
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_093:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 93
    jb run_sieve_dense_tail_093
align 16
run_sieve_dense_loop_093:
    btr qword [rsi], 46
    btr qword [rsi + 16], 11
    btr qword [rsi + 24], 40
    btr qword [rsi + 40], 5
    btr qword [rsi + 48], 34
    btr qword [rsi + 56], 63
    btr qword [rsi + 72], 28
    btr qword [rsi + 80], 57
    btr qword [rsi + 96], 22
    btr qword [rsi + 104], 51
    btr qword [rsi + 120], 16
    btr qword [rsi + 128], 45
    btr qword [rsi + 144], 10
    btr qword [rsi + 152], 39
    btr qword [rsi + 168], 4
    btr qword [rsi + 176], 33
    btr qword [rsi + 184], 62
    btr qword [rsi + 200], 27
    btr qword [rsi + 208], 56
    btr qword [rsi + 224], 21
    btr qword [rsi + 232], 50
    btr qword [rsi + 248], 15
    btr qword [rsi + 256], 44
    btr qword [rsi + 272], 9
    btr qword [rsi + 280], 38
    btr qword [rsi + 296], 3
    btr qword [rsi + 304], 32
    btr qword [rsi + 312], 61
    btr qword [rsi + 328], 26
    btr qword [rsi + 336], 55
    btr qword [rsi + 352], 20
    btr qword [rsi + 360], 49
    btr qword [rsi + 376], 14
    btr qword [rsi + 384], 43
    btr qword [rsi + 400], 8
    btr qword [rsi + 408], 37
    btr qword [rsi + 424], 2
    btr qword [rsi + 432], 31
    btr qword [rsi + 440], 60
    btr qword [rsi + 456], 25
    btr qword [rsi + 464], 54
    btr qword [rsi + 480], 19
    btr qword [rsi + 488], 48
    btr qword [rsi + 504], 13
    btr qword [rsi + 512], 42
    btr qword [rsi + 528], 7
    btr qword [rsi + 536], 36
    btr qword [rsi + 552], 1
    btr qword [rsi + 560], 30
    btr qword [rsi + 568], 59
    btr qword [rsi + 584], 24
    btr qword [rsi + 592], 53
    btr qword [rsi + 608], 18
    btr qword [rsi + 616], 47
    btr qword [rsi + 632], 12
    btr qword [rsi + 640], 41
    btr qword [rsi + 656], 6
    btr qword [rsi + 664], 35
    btr qword [rsi + 680], 0
    btr qword [rsi + 688], 29
    btr qword [rsi + 696], 58
    btr qword [rsi + 712], 23
    btr qword [rsi + 720], 52
    btr qword [rsi + 736], 17
    add rsi, 744
    sub ecx, 93
    cmp ecx, 93
    jae run_sieve_dense_loop_093
run_sieve_dense_tail_093:
    test ecx, ecx
    jz run_sieve_dense_restore_093
    cmp ecx, 1
    jb run_sieve_dense_restore_093
    btr qword [rsi], 46
    cmp ecx, 3
    jb run_sieve_dense_restore_093
    btr qword [rsi + 16], 11
    cmp ecx, 4
    jb run_sieve_dense_restore_093
    btr qword [rsi + 24], 40
    cmp ecx, 6
    jb run_sieve_dense_restore_093
    btr qword [rsi + 40], 5
    cmp ecx, 7
    jb run_sieve_dense_restore_093
    btr qword [rsi + 48], 34
    cmp ecx, 8
    jb run_sieve_dense_restore_093
    btr qword [rsi + 56], 63
    cmp ecx, 10
    jb run_sieve_dense_restore_093
    btr qword [rsi + 72], 28
    cmp ecx, 11
    jb run_sieve_dense_restore_093
    btr qword [rsi + 80], 57
    cmp ecx, 13
    jb run_sieve_dense_restore_093
    btr qword [rsi + 96], 22
    cmp ecx, 14
    jb run_sieve_dense_restore_093
    btr qword [rsi + 104], 51
    cmp ecx, 16
    jb run_sieve_dense_restore_093
    btr qword [rsi + 120], 16
    cmp ecx, 17
    jb run_sieve_dense_restore_093
    btr qword [rsi + 128], 45
    cmp ecx, 19
    jb run_sieve_dense_restore_093
    btr qword [rsi + 144], 10
    cmp ecx, 20
    jb run_sieve_dense_restore_093
    btr qword [rsi + 152], 39
    cmp ecx, 22
    jb run_sieve_dense_restore_093
    btr qword [rsi + 168], 4
    cmp ecx, 23
    jb run_sieve_dense_restore_093
    btr qword [rsi + 176], 33
    cmp ecx, 24
    jb run_sieve_dense_restore_093
    btr qword [rsi + 184], 62
    cmp ecx, 26
    jb run_sieve_dense_restore_093
    btr qword [rsi + 200], 27
    cmp ecx, 27
    jb run_sieve_dense_restore_093
    btr qword [rsi + 208], 56
    cmp ecx, 29
    jb run_sieve_dense_restore_093
    btr qword [rsi + 224], 21
    cmp ecx, 30
    jb run_sieve_dense_restore_093
    btr qword [rsi + 232], 50
    cmp ecx, 32
    jb run_sieve_dense_restore_093
    btr qword [rsi + 248], 15
    cmp ecx, 33
    jb run_sieve_dense_restore_093
    btr qword [rsi + 256], 44
    cmp ecx, 35
    jb run_sieve_dense_restore_093
    btr qword [rsi + 272], 9
    cmp ecx, 36
    jb run_sieve_dense_restore_093
    btr qword [rsi + 280], 38
    cmp ecx, 38
    jb run_sieve_dense_restore_093
    btr qword [rsi + 296], 3
    cmp ecx, 39
    jb run_sieve_dense_restore_093
    btr qword [rsi + 304], 32
    cmp ecx, 40
    jb run_sieve_dense_restore_093
    btr qword [rsi + 312], 61
    cmp ecx, 42
    jb run_sieve_dense_restore_093
    btr qword [rsi + 328], 26
    cmp ecx, 43
    jb run_sieve_dense_restore_093
    btr qword [rsi + 336], 55
    cmp ecx, 45
    jb run_sieve_dense_restore_093
    btr qword [rsi + 352], 20
    cmp ecx, 46
    jb run_sieve_dense_restore_093
    btr qword [rsi + 360], 49
    cmp ecx, 48
    jb run_sieve_dense_restore_093
    btr qword [rsi + 376], 14
    cmp ecx, 49
    jb run_sieve_dense_restore_093
    btr qword [rsi + 384], 43
    cmp ecx, 51
    jb run_sieve_dense_restore_093
    btr qword [rsi + 400], 8
    cmp ecx, 52
    jb run_sieve_dense_restore_093
    btr qword [rsi + 408], 37
    cmp ecx, 54
    jb run_sieve_dense_restore_093
    btr qword [rsi + 424], 2
    cmp ecx, 55
    jb run_sieve_dense_restore_093
    btr qword [rsi + 432], 31
    cmp ecx, 56
    jb run_sieve_dense_restore_093
    btr qword [rsi + 440], 60
    cmp ecx, 58
    jb run_sieve_dense_restore_093
    btr qword [rsi + 456], 25
    cmp ecx, 59
    jb run_sieve_dense_restore_093
    btr qword [rsi + 464], 54
    cmp ecx, 61
    jb run_sieve_dense_restore_093
    btr qword [rsi + 480], 19
    cmp ecx, 62
    jb run_sieve_dense_restore_093
    btr qword [rsi + 488], 48
    cmp ecx, 64
    jb run_sieve_dense_restore_093
    btr qword [rsi + 504], 13
    cmp ecx, 65
    jb run_sieve_dense_restore_093
    btr qword [rsi + 512], 42
    cmp ecx, 67
    jb run_sieve_dense_restore_093
    btr qword [rsi + 528], 7
    cmp ecx, 68
    jb run_sieve_dense_restore_093
    btr qword [rsi + 536], 36
    cmp ecx, 70
    jb run_sieve_dense_restore_093
    btr qword [rsi + 552], 1
    cmp ecx, 71
    jb run_sieve_dense_restore_093
    btr qword [rsi + 560], 30
    cmp ecx, 72
    jb run_sieve_dense_restore_093
    btr qword [rsi + 568], 59
    cmp ecx, 74
    jb run_sieve_dense_restore_093
    btr qword [rsi + 584], 24
    cmp ecx, 75
    jb run_sieve_dense_restore_093
    btr qword [rsi + 592], 53
    cmp ecx, 77
    jb run_sieve_dense_restore_093
    btr qword [rsi + 608], 18
    cmp ecx, 78
    jb run_sieve_dense_restore_093
    btr qword [rsi + 616], 47
    cmp ecx, 80
    jb run_sieve_dense_restore_093
    btr qword [rsi + 632], 12
    cmp ecx, 81
    jb run_sieve_dense_restore_093
    btr qword [rsi + 640], 41
    cmp ecx, 83
    jb run_sieve_dense_restore_093
    btr qword [rsi + 656], 6
    cmp ecx, 84
    jb run_sieve_dense_restore_093
    btr qword [rsi + 664], 35
    cmp ecx, 86
    jb run_sieve_dense_restore_093
    btr qword [rsi + 680], 0
    cmp ecx, 87
    jb run_sieve_dense_restore_093
    btr qword [rsi + 688], 29
    cmp ecx, 88
    jb run_sieve_dense_restore_093
    btr qword [rsi + 696], 58
    cmp ecx, 90
    jb run_sieve_dense_restore_093
    btr qword [rsi + 712], 23
    cmp ecx, 91
    jb run_sieve_dense_restore_093
    btr qword [rsi + 720], 52
    cmp ecx, 93
    jb run_sieve_dense_restore_093
    btr qword [rsi + 736], 17
run_sieve_dense_restore_093:
    bts qword [r14], 46
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_095:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 95
    jb run_sieve_dense_tail_095
align 16
run_sieve_dense_loop_095:
    btr qword [rsi], 47
    btr qword [rsi + 16], 14
    btr qword [rsi + 24], 45
    btr qword [rsi + 40], 12
    btr qword [rsi + 48], 43
    btr qword [rsi + 64], 10
    btr qword [rsi + 72], 41
    btr qword [rsi + 88], 8
    btr qword [rsi + 96], 39
    btr qword [rsi + 112], 6
    btr qword [rsi + 120], 37
    btr qword [rsi + 136], 4
    btr qword [rsi + 144], 35
    btr qword [rsi + 160], 2
    btr qword [rsi + 168], 33
    btr qword [rsi + 184], 0
    btr qword [rsi + 192], 31
    btr qword [rsi + 200], 62
    btr qword [rsi + 216], 29
    btr qword [rsi + 224], 60
    btr qword [rsi + 240], 27
    btr qword [rsi + 248], 58
    btr qword [rsi + 264], 25
    btr qword [rsi + 272], 56
    btr qword [rsi + 288], 23
    btr qword [rsi + 296], 54
    btr qword [rsi + 312], 21
    btr qword [rsi + 320], 52
    btr qword [rsi + 336], 19
    btr qword [rsi + 344], 50
    btr qword [rsi + 360], 17
    btr qword [rsi + 368], 48
    btr qword [rsi + 384], 15
    btr qword [rsi + 392], 46
    btr qword [rsi + 408], 13
    btr qword [rsi + 416], 44
    btr qword [rsi + 432], 11
    btr qword [rsi + 440], 42
    btr qword [rsi + 456], 9
    btr qword [rsi + 464], 40
    btr qword [rsi + 480], 7
    btr qword [rsi + 488], 38
    btr qword [rsi + 504], 5
    btr qword [rsi + 512], 36
    btr qword [rsi + 528], 3
    btr qword [rsi + 536], 34
    btr qword [rsi + 552], 1
    btr qword [rsi + 560], 32
    btr qword [rsi + 568], 63
    btr qword [rsi + 584], 30
    btr qword [rsi + 592], 61
    btr qword [rsi + 608], 28
    btr qword [rsi + 616], 59
    btr qword [rsi + 632], 26
    btr qword [rsi + 640], 57
    btr qword [rsi + 656], 24
    btr qword [rsi + 664], 55
    btr qword [rsi + 680], 22
    btr qword [rsi + 688], 53
    btr qword [rsi + 704], 20
    btr qword [rsi + 712], 51
    btr qword [rsi + 728], 18
    btr qword [rsi + 736], 49
    btr qword [rsi + 752], 16
    add rsi, 760
    sub ecx, 95
    cmp ecx, 95
    jae run_sieve_dense_loop_095
run_sieve_dense_tail_095:
    test ecx, ecx
    jz run_sieve_dense_restore_095
    cmp ecx, 1
    jb run_sieve_dense_restore_095
    btr qword [rsi], 47
    cmp ecx, 3
    jb run_sieve_dense_restore_095
    btr qword [rsi + 16], 14
    cmp ecx, 4
    jb run_sieve_dense_restore_095
    btr qword [rsi + 24], 45
    cmp ecx, 6
    jb run_sieve_dense_restore_095
    btr qword [rsi + 40], 12
    cmp ecx, 7
    jb run_sieve_dense_restore_095
    btr qword [rsi + 48], 43
    cmp ecx, 9
    jb run_sieve_dense_restore_095
    btr qword [rsi + 64], 10
    cmp ecx, 10
    jb run_sieve_dense_restore_095
    btr qword [rsi + 72], 41
    cmp ecx, 12
    jb run_sieve_dense_restore_095
    btr qword [rsi + 88], 8
    cmp ecx, 13
    jb run_sieve_dense_restore_095
    btr qword [rsi + 96], 39
    cmp ecx, 15
    jb run_sieve_dense_restore_095
    btr qword [rsi + 112], 6
    cmp ecx, 16
    jb run_sieve_dense_restore_095
    btr qword [rsi + 120], 37
    cmp ecx, 18
    jb run_sieve_dense_restore_095
    btr qword [rsi + 136], 4
    cmp ecx, 19
    jb run_sieve_dense_restore_095
    btr qword [rsi + 144], 35
    cmp ecx, 21
    jb run_sieve_dense_restore_095
    btr qword [rsi + 160], 2
    cmp ecx, 22
    jb run_sieve_dense_restore_095
    btr qword [rsi + 168], 33
    cmp ecx, 24
    jb run_sieve_dense_restore_095
    btr qword [rsi + 184], 0
    cmp ecx, 25
    jb run_sieve_dense_restore_095
    btr qword [rsi + 192], 31
    cmp ecx, 26
    jb run_sieve_dense_restore_095
    btr qword [rsi + 200], 62
    cmp ecx, 28
    jb run_sieve_dense_restore_095
    btr qword [rsi + 216], 29
    cmp ecx, 29
    jb run_sieve_dense_restore_095
    btr qword [rsi + 224], 60
    cmp ecx, 31
    jb run_sieve_dense_restore_095
    btr qword [rsi + 240], 27
    cmp ecx, 32
    jb run_sieve_dense_restore_095
    btr qword [rsi + 248], 58
    cmp ecx, 34
    jb run_sieve_dense_restore_095
    btr qword [rsi + 264], 25
    cmp ecx, 35
    jb run_sieve_dense_restore_095
    btr qword [rsi + 272], 56
    cmp ecx, 37
    jb run_sieve_dense_restore_095
    btr qword [rsi + 288], 23
    cmp ecx, 38
    jb run_sieve_dense_restore_095
    btr qword [rsi + 296], 54
    cmp ecx, 40
    jb run_sieve_dense_restore_095
    btr qword [rsi + 312], 21
    cmp ecx, 41
    jb run_sieve_dense_restore_095
    btr qword [rsi + 320], 52
    cmp ecx, 43
    jb run_sieve_dense_restore_095
    btr qword [rsi + 336], 19
    cmp ecx, 44
    jb run_sieve_dense_restore_095
    btr qword [rsi + 344], 50
    cmp ecx, 46
    jb run_sieve_dense_restore_095
    btr qword [rsi + 360], 17
    cmp ecx, 47
    jb run_sieve_dense_restore_095
    btr qword [rsi + 368], 48
    cmp ecx, 49
    jb run_sieve_dense_restore_095
    btr qword [rsi + 384], 15
    cmp ecx, 50
    jb run_sieve_dense_restore_095
    btr qword [rsi + 392], 46
    cmp ecx, 52
    jb run_sieve_dense_restore_095
    btr qword [rsi + 408], 13
    cmp ecx, 53
    jb run_sieve_dense_restore_095
    btr qword [rsi + 416], 44
    cmp ecx, 55
    jb run_sieve_dense_restore_095
    btr qword [rsi + 432], 11
    cmp ecx, 56
    jb run_sieve_dense_restore_095
    btr qword [rsi + 440], 42
    cmp ecx, 58
    jb run_sieve_dense_restore_095
    btr qword [rsi + 456], 9
    cmp ecx, 59
    jb run_sieve_dense_restore_095
    btr qword [rsi + 464], 40
    cmp ecx, 61
    jb run_sieve_dense_restore_095
    btr qword [rsi + 480], 7
    cmp ecx, 62
    jb run_sieve_dense_restore_095
    btr qword [rsi + 488], 38
    cmp ecx, 64
    jb run_sieve_dense_restore_095
    btr qword [rsi + 504], 5
    cmp ecx, 65
    jb run_sieve_dense_restore_095
    btr qword [rsi + 512], 36
    cmp ecx, 67
    jb run_sieve_dense_restore_095
    btr qword [rsi + 528], 3
    cmp ecx, 68
    jb run_sieve_dense_restore_095
    btr qword [rsi + 536], 34
    cmp ecx, 70
    jb run_sieve_dense_restore_095
    btr qword [rsi + 552], 1
    cmp ecx, 71
    jb run_sieve_dense_restore_095
    btr qword [rsi + 560], 32
    cmp ecx, 72
    jb run_sieve_dense_restore_095
    btr qword [rsi + 568], 63
    cmp ecx, 74
    jb run_sieve_dense_restore_095
    btr qword [rsi + 584], 30
    cmp ecx, 75
    jb run_sieve_dense_restore_095
    btr qword [rsi + 592], 61
    cmp ecx, 77
    jb run_sieve_dense_restore_095
    btr qword [rsi + 608], 28
    cmp ecx, 78
    jb run_sieve_dense_restore_095
    btr qword [rsi + 616], 59
    cmp ecx, 80
    jb run_sieve_dense_restore_095
    btr qword [rsi + 632], 26
    cmp ecx, 81
    jb run_sieve_dense_restore_095
    btr qword [rsi + 640], 57
    cmp ecx, 83
    jb run_sieve_dense_restore_095
    btr qword [rsi + 656], 24
    cmp ecx, 84
    jb run_sieve_dense_restore_095
    btr qword [rsi + 664], 55
    cmp ecx, 86
    jb run_sieve_dense_restore_095
    btr qword [rsi + 680], 22
    cmp ecx, 87
    jb run_sieve_dense_restore_095
    btr qword [rsi + 688], 53
    cmp ecx, 89
    jb run_sieve_dense_restore_095
    btr qword [rsi + 704], 20
    cmp ecx, 90
    jb run_sieve_dense_restore_095
    btr qword [rsi + 712], 51
    cmp ecx, 92
    jb run_sieve_dense_restore_095
    btr qword [rsi + 728], 18
    cmp ecx, 93
    jb run_sieve_dense_restore_095
    btr qword [rsi + 736], 49
    cmp ecx, 95
    jb run_sieve_dense_restore_095
    btr qword [rsi + 752], 16
run_sieve_dense_restore_095:
    bts qword [r14], 47
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_097:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 97
    jb run_sieve_dense_tail_097
align 16
run_sieve_dense_loop_097:
    btr qword [rsi], 48
    btr qword [rsi + 16], 17
    btr qword [rsi + 24], 50
    btr qword [rsi + 40], 19
    btr qword [rsi + 48], 52
    btr qword [rsi + 64], 21
    btr qword [rsi + 72], 54
    btr qword [rsi + 88], 23
    btr qword [rsi + 96], 56
    btr qword [rsi + 112], 25
    btr qword [rsi + 120], 58
    btr qword [rsi + 136], 27
    btr qword [rsi + 144], 60
    btr qword [rsi + 160], 29
    btr qword [rsi + 168], 62
    btr qword [rsi + 184], 31
    btr qword [rsi + 200], 0
    btr qword [rsi + 208], 33
    btr qword [rsi + 224], 2
    btr qword [rsi + 232], 35
    btr qword [rsi + 248], 4
    btr qword [rsi + 256], 37
    btr qword [rsi + 272], 6
    btr qword [rsi + 280], 39
    btr qword [rsi + 296], 8
    btr qword [rsi + 304], 41
    btr qword [rsi + 320], 10
    btr qword [rsi + 328], 43
    btr qword [rsi + 344], 12
    btr qword [rsi + 352], 45
    btr qword [rsi + 368], 14
    btr qword [rsi + 376], 47
    btr qword [rsi + 392], 16
    btr qword [rsi + 400], 49
    btr qword [rsi + 416], 18
    btr qword [rsi + 424], 51
    btr qword [rsi + 440], 20
    btr qword [rsi + 448], 53
    btr qword [rsi + 464], 22
    btr qword [rsi + 472], 55
    btr qword [rsi + 488], 24
    btr qword [rsi + 496], 57
    btr qword [rsi + 512], 26
    btr qword [rsi + 520], 59
    btr qword [rsi + 536], 28
    btr qword [rsi + 544], 61
    btr qword [rsi + 560], 30
    btr qword [rsi + 568], 63
    btr qword [rsi + 584], 32
    btr qword [rsi + 600], 1
    btr qword [rsi + 608], 34
    btr qword [rsi + 624], 3
    btr qword [rsi + 632], 36
    btr qword [rsi + 648], 5
    btr qword [rsi + 656], 38
    btr qword [rsi + 672], 7
    btr qword [rsi + 680], 40
    btr qword [rsi + 696], 9
    btr qword [rsi + 704], 42
    btr qword [rsi + 720], 11
    btr qword [rsi + 728], 44
    btr qword [rsi + 744], 13
    btr qword [rsi + 752], 46
    btr qword [rsi + 768], 15
    add rsi, 776
    sub ecx, 97
    cmp ecx, 97
    jae run_sieve_dense_loop_097
run_sieve_dense_tail_097:
    test ecx, ecx
    jz run_sieve_dense_restore_097
    cmp ecx, 1
    jb run_sieve_dense_restore_097
    btr qword [rsi], 48
    cmp ecx, 3
    jb run_sieve_dense_restore_097
    btr qword [rsi + 16], 17
    cmp ecx, 4
    jb run_sieve_dense_restore_097
    btr qword [rsi + 24], 50
    cmp ecx, 6
    jb run_sieve_dense_restore_097
    btr qword [rsi + 40], 19
    cmp ecx, 7
    jb run_sieve_dense_restore_097
    btr qword [rsi + 48], 52
    cmp ecx, 9
    jb run_sieve_dense_restore_097
    btr qword [rsi + 64], 21
    cmp ecx, 10
    jb run_sieve_dense_restore_097
    btr qword [rsi + 72], 54
    cmp ecx, 12
    jb run_sieve_dense_restore_097
    btr qword [rsi + 88], 23
    cmp ecx, 13
    jb run_sieve_dense_restore_097
    btr qword [rsi + 96], 56
    cmp ecx, 15
    jb run_sieve_dense_restore_097
    btr qword [rsi + 112], 25
    cmp ecx, 16
    jb run_sieve_dense_restore_097
    btr qword [rsi + 120], 58
    cmp ecx, 18
    jb run_sieve_dense_restore_097
    btr qword [rsi + 136], 27
    cmp ecx, 19
    jb run_sieve_dense_restore_097
    btr qword [rsi + 144], 60
    cmp ecx, 21
    jb run_sieve_dense_restore_097
    btr qword [rsi + 160], 29
    cmp ecx, 22
    jb run_sieve_dense_restore_097
    btr qword [rsi + 168], 62
    cmp ecx, 24
    jb run_sieve_dense_restore_097
    btr qword [rsi + 184], 31
    cmp ecx, 26
    jb run_sieve_dense_restore_097
    btr qword [rsi + 200], 0
    cmp ecx, 27
    jb run_sieve_dense_restore_097
    btr qword [rsi + 208], 33
    cmp ecx, 29
    jb run_sieve_dense_restore_097
    btr qword [rsi + 224], 2
    cmp ecx, 30
    jb run_sieve_dense_restore_097
    btr qword [rsi + 232], 35
    cmp ecx, 32
    jb run_sieve_dense_restore_097
    btr qword [rsi + 248], 4
    cmp ecx, 33
    jb run_sieve_dense_restore_097
    btr qword [rsi + 256], 37
    cmp ecx, 35
    jb run_sieve_dense_restore_097
    btr qword [rsi + 272], 6
    cmp ecx, 36
    jb run_sieve_dense_restore_097
    btr qword [rsi + 280], 39
    cmp ecx, 38
    jb run_sieve_dense_restore_097
    btr qword [rsi + 296], 8
    cmp ecx, 39
    jb run_sieve_dense_restore_097
    btr qword [rsi + 304], 41
    cmp ecx, 41
    jb run_sieve_dense_restore_097
    btr qword [rsi + 320], 10
    cmp ecx, 42
    jb run_sieve_dense_restore_097
    btr qword [rsi + 328], 43
    cmp ecx, 44
    jb run_sieve_dense_restore_097
    btr qword [rsi + 344], 12
    cmp ecx, 45
    jb run_sieve_dense_restore_097
    btr qword [rsi + 352], 45
    cmp ecx, 47
    jb run_sieve_dense_restore_097
    btr qword [rsi + 368], 14
    cmp ecx, 48
    jb run_sieve_dense_restore_097
    btr qword [rsi + 376], 47
    cmp ecx, 50
    jb run_sieve_dense_restore_097
    btr qword [rsi + 392], 16
    cmp ecx, 51
    jb run_sieve_dense_restore_097
    btr qword [rsi + 400], 49
    cmp ecx, 53
    jb run_sieve_dense_restore_097
    btr qword [rsi + 416], 18
    cmp ecx, 54
    jb run_sieve_dense_restore_097
    btr qword [rsi + 424], 51
    cmp ecx, 56
    jb run_sieve_dense_restore_097
    btr qword [rsi + 440], 20
    cmp ecx, 57
    jb run_sieve_dense_restore_097
    btr qword [rsi + 448], 53
    cmp ecx, 59
    jb run_sieve_dense_restore_097
    btr qword [rsi + 464], 22
    cmp ecx, 60
    jb run_sieve_dense_restore_097
    btr qword [rsi + 472], 55
    cmp ecx, 62
    jb run_sieve_dense_restore_097
    btr qword [rsi + 488], 24
    cmp ecx, 63
    jb run_sieve_dense_restore_097
    btr qword [rsi + 496], 57
    cmp ecx, 65
    jb run_sieve_dense_restore_097
    btr qword [rsi + 512], 26
    cmp ecx, 66
    jb run_sieve_dense_restore_097
    btr qword [rsi + 520], 59
    cmp ecx, 68
    jb run_sieve_dense_restore_097
    btr qword [rsi + 536], 28
    cmp ecx, 69
    jb run_sieve_dense_restore_097
    btr qword [rsi + 544], 61
    cmp ecx, 71
    jb run_sieve_dense_restore_097
    btr qword [rsi + 560], 30
    cmp ecx, 72
    jb run_sieve_dense_restore_097
    btr qword [rsi + 568], 63
    cmp ecx, 74
    jb run_sieve_dense_restore_097
    btr qword [rsi + 584], 32
    cmp ecx, 76
    jb run_sieve_dense_restore_097
    btr qword [rsi + 600], 1
    cmp ecx, 77
    jb run_sieve_dense_restore_097
    btr qword [rsi + 608], 34
    cmp ecx, 79
    jb run_sieve_dense_restore_097
    btr qword [rsi + 624], 3
    cmp ecx, 80
    jb run_sieve_dense_restore_097
    btr qword [rsi + 632], 36
    cmp ecx, 82
    jb run_sieve_dense_restore_097
    btr qword [rsi + 648], 5
    cmp ecx, 83
    jb run_sieve_dense_restore_097
    btr qword [rsi + 656], 38
    cmp ecx, 85
    jb run_sieve_dense_restore_097
    btr qword [rsi + 672], 7
    cmp ecx, 86
    jb run_sieve_dense_restore_097
    btr qword [rsi + 680], 40
    cmp ecx, 88
    jb run_sieve_dense_restore_097
    btr qword [rsi + 696], 9
    cmp ecx, 89
    jb run_sieve_dense_restore_097
    btr qword [rsi + 704], 42
    cmp ecx, 91
    jb run_sieve_dense_restore_097
    btr qword [rsi + 720], 11
    cmp ecx, 92
    jb run_sieve_dense_restore_097
    btr qword [rsi + 728], 44
    cmp ecx, 94
    jb run_sieve_dense_restore_097
    btr qword [rsi + 744], 13
    cmp ecx, 95
    jb run_sieve_dense_restore_097
    btr qword [rsi + 752], 46
    cmp ecx, 97
    jb run_sieve_dense_restore_097
    btr qword [rsi + 768], 15
run_sieve_dense_restore_097:
    bts qword [r14], 48
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_099:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 99
    jb run_sieve_dense_tail_099
align 16
run_sieve_dense_loop_099:
    btr qword [rsi], 49
    btr qword [rsi + 16], 20
    btr qword [rsi + 24], 55
    btr qword [rsi + 40], 26
    btr qword [rsi + 48], 61
    btr qword [rsi + 64], 32
    btr qword [rsi + 80], 3
    btr qword [rsi + 88], 38
    btr qword [rsi + 104], 9
    btr qword [rsi + 112], 44
    btr qword [rsi + 128], 15
    btr qword [rsi + 136], 50
    btr qword [rsi + 152], 21
    btr qword [rsi + 160], 56
    btr qword [rsi + 176], 27
    btr qword [rsi + 184], 62
    btr qword [rsi + 200], 33
    btr qword [rsi + 216], 4
    btr qword [rsi + 224], 39
    btr qword [rsi + 240], 10
    btr qword [rsi + 248], 45
    btr qword [rsi + 264], 16
    btr qword [rsi + 272], 51
    btr qword [rsi + 288], 22
    btr qword [rsi + 296], 57
    btr qword [rsi + 312], 28
    btr qword [rsi + 320], 63
    btr qword [rsi + 336], 34
    btr qword [rsi + 352], 5
    btr qword [rsi + 360], 40
    btr qword [rsi + 376], 11
    btr qword [rsi + 384], 46
    btr qword [rsi + 400], 17
    btr qword [rsi + 408], 52
    btr qword [rsi + 424], 23
    btr qword [rsi + 432], 58
    btr qword [rsi + 448], 29
    btr qword [rsi + 464], 0
    btr qword [rsi + 472], 35
    btr qword [rsi + 488], 6
    btr qword [rsi + 496], 41
    btr qword [rsi + 512], 12
    btr qword [rsi + 520], 47
    btr qword [rsi + 536], 18
    btr qword [rsi + 544], 53
    btr qword [rsi + 560], 24
    btr qword [rsi + 568], 59
    btr qword [rsi + 584], 30
    btr qword [rsi + 600], 1
    btr qword [rsi + 608], 36
    btr qword [rsi + 624], 7
    btr qword [rsi + 632], 42
    btr qword [rsi + 648], 13
    btr qword [rsi + 656], 48
    btr qword [rsi + 672], 19
    btr qword [rsi + 680], 54
    btr qword [rsi + 696], 25
    btr qword [rsi + 704], 60
    btr qword [rsi + 720], 31
    btr qword [rsi + 736], 2
    btr qword [rsi + 744], 37
    btr qword [rsi + 760], 8
    btr qword [rsi + 768], 43
    btr qword [rsi + 784], 14
    add rsi, 792
    sub ecx, 99
    cmp ecx, 99
    jae run_sieve_dense_loop_099
run_sieve_dense_tail_099:
    test ecx, ecx
    jz run_sieve_dense_restore_099
    cmp ecx, 1
    jb run_sieve_dense_restore_099
    btr qword [rsi], 49
    cmp ecx, 3
    jb run_sieve_dense_restore_099
    btr qword [rsi + 16], 20
    cmp ecx, 4
    jb run_sieve_dense_restore_099
    btr qword [rsi + 24], 55
    cmp ecx, 6
    jb run_sieve_dense_restore_099
    btr qword [rsi + 40], 26
    cmp ecx, 7
    jb run_sieve_dense_restore_099
    btr qword [rsi + 48], 61
    cmp ecx, 9
    jb run_sieve_dense_restore_099
    btr qword [rsi + 64], 32
    cmp ecx, 11
    jb run_sieve_dense_restore_099
    btr qword [rsi + 80], 3
    cmp ecx, 12
    jb run_sieve_dense_restore_099
    btr qword [rsi + 88], 38
    cmp ecx, 14
    jb run_sieve_dense_restore_099
    btr qword [rsi + 104], 9
    cmp ecx, 15
    jb run_sieve_dense_restore_099
    btr qword [rsi + 112], 44
    cmp ecx, 17
    jb run_sieve_dense_restore_099
    btr qword [rsi + 128], 15
    cmp ecx, 18
    jb run_sieve_dense_restore_099
    btr qword [rsi + 136], 50
    cmp ecx, 20
    jb run_sieve_dense_restore_099
    btr qword [rsi + 152], 21
    cmp ecx, 21
    jb run_sieve_dense_restore_099
    btr qword [rsi + 160], 56
    cmp ecx, 23
    jb run_sieve_dense_restore_099
    btr qword [rsi + 176], 27
    cmp ecx, 24
    jb run_sieve_dense_restore_099
    btr qword [rsi + 184], 62
    cmp ecx, 26
    jb run_sieve_dense_restore_099
    btr qword [rsi + 200], 33
    cmp ecx, 28
    jb run_sieve_dense_restore_099
    btr qword [rsi + 216], 4
    cmp ecx, 29
    jb run_sieve_dense_restore_099
    btr qword [rsi + 224], 39
    cmp ecx, 31
    jb run_sieve_dense_restore_099
    btr qword [rsi + 240], 10
    cmp ecx, 32
    jb run_sieve_dense_restore_099
    btr qword [rsi + 248], 45
    cmp ecx, 34
    jb run_sieve_dense_restore_099
    btr qword [rsi + 264], 16
    cmp ecx, 35
    jb run_sieve_dense_restore_099
    btr qword [rsi + 272], 51
    cmp ecx, 37
    jb run_sieve_dense_restore_099
    btr qword [rsi + 288], 22
    cmp ecx, 38
    jb run_sieve_dense_restore_099
    btr qword [rsi + 296], 57
    cmp ecx, 40
    jb run_sieve_dense_restore_099
    btr qword [rsi + 312], 28
    cmp ecx, 41
    jb run_sieve_dense_restore_099
    btr qword [rsi + 320], 63
    cmp ecx, 43
    jb run_sieve_dense_restore_099
    btr qword [rsi + 336], 34
    cmp ecx, 45
    jb run_sieve_dense_restore_099
    btr qword [rsi + 352], 5
    cmp ecx, 46
    jb run_sieve_dense_restore_099
    btr qword [rsi + 360], 40
    cmp ecx, 48
    jb run_sieve_dense_restore_099
    btr qword [rsi + 376], 11
    cmp ecx, 49
    jb run_sieve_dense_restore_099
    btr qword [rsi + 384], 46
    cmp ecx, 51
    jb run_sieve_dense_restore_099
    btr qword [rsi + 400], 17
    cmp ecx, 52
    jb run_sieve_dense_restore_099
    btr qword [rsi + 408], 52
    cmp ecx, 54
    jb run_sieve_dense_restore_099
    btr qword [rsi + 424], 23
    cmp ecx, 55
    jb run_sieve_dense_restore_099
    btr qword [rsi + 432], 58
    cmp ecx, 57
    jb run_sieve_dense_restore_099
    btr qword [rsi + 448], 29
    cmp ecx, 59
    jb run_sieve_dense_restore_099
    btr qword [rsi + 464], 0
    cmp ecx, 60
    jb run_sieve_dense_restore_099
    btr qword [rsi + 472], 35
    cmp ecx, 62
    jb run_sieve_dense_restore_099
    btr qword [rsi + 488], 6
    cmp ecx, 63
    jb run_sieve_dense_restore_099
    btr qword [rsi + 496], 41
    cmp ecx, 65
    jb run_sieve_dense_restore_099
    btr qword [rsi + 512], 12
    cmp ecx, 66
    jb run_sieve_dense_restore_099
    btr qword [rsi + 520], 47
    cmp ecx, 68
    jb run_sieve_dense_restore_099
    btr qword [rsi + 536], 18
    cmp ecx, 69
    jb run_sieve_dense_restore_099
    btr qword [rsi + 544], 53
    cmp ecx, 71
    jb run_sieve_dense_restore_099
    btr qword [rsi + 560], 24
    cmp ecx, 72
    jb run_sieve_dense_restore_099
    btr qword [rsi + 568], 59
    cmp ecx, 74
    jb run_sieve_dense_restore_099
    btr qword [rsi + 584], 30
    cmp ecx, 76
    jb run_sieve_dense_restore_099
    btr qword [rsi + 600], 1
    cmp ecx, 77
    jb run_sieve_dense_restore_099
    btr qword [rsi + 608], 36
    cmp ecx, 79
    jb run_sieve_dense_restore_099
    btr qword [rsi + 624], 7
    cmp ecx, 80
    jb run_sieve_dense_restore_099
    btr qword [rsi + 632], 42
    cmp ecx, 82
    jb run_sieve_dense_restore_099
    btr qword [rsi + 648], 13
    cmp ecx, 83
    jb run_sieve_dense_restore_099
    btr qword [rsi + 656], 48
    cmp ecx, 85
    jb run_sieve_dense_restore_099
    btr qword [rsi + 672], 19
    cmp ecx, 86
    jb run_sieve_dense_restore_099
    btr qword [rsi + 680], 54
    cmp ecx, 88
    jb run_sieve_dense_restore_099
    btr qword [rsi + 696], 25
    cmp ecx, 89
    jb run_sieve_dense_restore_099
    btr qword [rsi + 704], 60
    cmp ecx, 91
    jb run_sieve_dense_restore_099
    btr qword [rsi + 720], 31
    cmp ecx, 93
    jb run_sieve_dense_restore_099
    btr qword [rsi + 736], 2
    cmp ecx, 94
    jb run_sieve_dense_restore_099
    btr qword [rsi + 744], 37
    cmp ecx, 96
    jb run_sieve_dense_restore_099
    btr qword [rsi + 760], 8
    cmp ecx, 97
    jb run_sieve_dense_restore_099
    btr qword [rsi + 768], 43
    cmp ecx, 99
    jb run_sieve_dense_restore_099
    btr qword [rsi + 784], 14
run_sieve_dense_restore_099:
    bts qword [r14], 49
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_101:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 101
    jb run_sieve_dense_tail_101
align 16
run_sieve_dense_loop_101:
    btr qword [rsi], 50
    btr qword [rsi + 16], 23
    btr qword [rsi + 24], 60
    btr qword [rsi + 40], 33
    btr qword [rsi + 56], 6
    btr qword [rsi + 64], 43
    btr qword [rsi + 80], 16
    btr qword [rsi + 88], 53
    btr qword [rsi + 104], 26
    btr qword [rsi + 112], 63
    btr qword [rsi + 128], 36
    btr qword [rsi + 144], 9
    btr qword [rsi + 152], 46
    btr qword [rsi + 168], 19
    btr qword [rsi + 176], 56
    btr qword [rsi + 192], 29
    btr qword [rsi + 208], 2
    btr qword [rsi + 216], 39
    btr qword [rsi + 232], 12
    btr qword [rsi + 240], 49
    btr qword [rsi + 256], 22
    btr qword [rsi + 264], 59
    btr qword [rsi + 280], 32
    btr qword [rsi + 296], 5
    btr qword [rsi + 304], 42
    btr qword [rsi + 320], 15
    btr qword [rsi + 328], 52
    btr qword [rsi + 344], 25
    btr qword [rsi + 352], 62
    btr qword [rsi + 368], 35
    btr qword [rsi + 384], 8
    btr qword [rsi + 392], 45
    btr qword [rsi + 408], 18
    btr qword [rsi + 416], 55
    btr qword [rsi + 432], 28
    btr qword [rsi + 448], 1
    btr qword [rsi + 456], 38
    btr qword [rsi + 472], 11
    btr qword [rsi + 480], 48
    btr qword [rsi + 496], 21
    btr qword [rsi + 504], 58
    btr qword [rsi + 520], 31
    btr qword [rsi + 536], 4
    btr qword [rsi + 544], 41
    btr qword [rsi + 560], 14
    btr qword [rsi + 568], 51
    btr qword [rsi + 584], 24
    btr qword [rsi + 592], 61
    btr qword [rsi + 608], 34
    btr qword [rsi + 624], 7
    btr qword [rsi + 632], 44
    btr qword [rsi + 648], 17
    btr qword [rsi + 656], 54
    btr qword [rsi + 672], 27
    btr qword [rsi + 688], 0
    btr qword [rsi + 696], 37
    btr qword [rsi + 712], 10
    btr qword [rsi + 720], 47
    btr qword [rsi + 736], 20
    btr qword [rsi + 744], 57
    btr qword [rsi + 760], 30
    btr qword [rsi + 776], 3
    btr qword [rsi + 784], 40
    btr qword [rsi + 800], 13
    add rsi, 808
    sub ecx, 101
    cmp ecx, 101
    jae run_sieve_dense_loop_101
run_sieve_dense_tail_101:
    test ecx, ecx
    jz run_sieve_dense_restore_101
    cmp ecx, 1
    jb run_sieve_dense_restore_101
    btr qword [rsi], 50
    cmp ecx, 3
    jb run_sieve_dense_restore_101
    btr qword [rsi + 16], 23
    cmp ecx, 4
    jb run_sieve_dense_restore_101
    btr qword [rsi + 24], 60
    cmp ecx, 6
    jb run_sieve_dense_restore_101
    btr qword [rsi + 40], 33
    cmp ecx, 8
    jb run_sieve_dense_restore_101
    btr qword [rsi + 56], 6
    cmp ecx, 9
    jb run_sieve_dense_restore_101
    btr qword [rsi + 64], 43
    cmp ecx, 11
    jb run_sieve_dense_restore_101
    btr qword [rsi + 80], 16
    cmp ecx, 12
    jb run_sieve_dense_restore_101
    btr qword [rsi + 88], 53
    cmp ecx, 14
    jb run_sieve_dense_restore_101
    btr qword [rsi + 104], 26
    cmp ecx, 15
    jb run_sieve_dense_restore_101
    btr qword [rsi + 112], 63
    cmp ecx, 17
    jb run_sieve_dense_restore_101
    btr qword [rsi + 128], 36
    cmp ecx, 19
    jb run_sieve_dense_restore_101
    btr qword [rsi + 144], 9
    cmp ecx, 20
    jb run_sieve_dense_restore_101
    btr qword [rsi + 152], 46
    cmp ecx, 22
    jb run_sieve_dense_restore_101
    btr qword [rsi + 168], 19
    cmp ecx, 23
    jb run_sieve_dense_restore_101
    btr qword [rsi + 176], 56
    cmp ecx, 25
    jb run_sieve_dense_restore_101
    btr qword [rsi + 192], 29
    cmp ecx, 27
    jb run_sieve_dense_restore_101
    btr qword [rsi + 208], 2
    cmp ecx, 28
    jb run_sieve_dense_restore_101
    btr qword [rsi + 216], 39
    cmp ecx, 30
    jb run_sieve_dense_restore_101
    btr qword [rsi + 232], 12
    cmp ecx, 31
    jb run_sieve_dense_restore_101
    btr qword [rsi + 240], 49
    cmp ecx, 33
    jb run_sieve_dense_restore_101
    btr qword [rsi + 256], 22
    cmp ecx, 34
    jb run_sieve_dense_restore_101
    btr qword [rsi + 264], 59
    cmp ecx, 36
    jb run_sieve_dense_restore_101
    btr qword [rsi + 280], 32
    cmp ecx, 38
    jb run_sieve_dense_restore_101
    btr qword [rsi + 296], 5
    cmp ecx, 39
    jb run_sieve_dense_restore_101
    btr qword [rsi + 304], 42
    cmp ecx, 41
    jb run_sieve_dense_restore_101
    btr qword [rsi + 320], 15
    cmp ecx, 42
    jb run_sieve_dense_restore_101
    btr qword [rsi + 328], 52
    cmp ecx, 44
    jb run_sieve_dense_restore_101
    btr qword [rsi + 344], 25
    cmp ecx, 45
    jb run_sieve_dense_restore_101
    btr qword [rsi + 352], 62
    cmp ecx, 47
    jb run_sieve_dense_restore_101
    btr qword [rsi + 368], 35
    cmp ecx, 49
    jb run_sieve_dense_restore_101
    btr qword [rsi + 384], 8
    cmp ecx, 50
    jb run_sieve_dense_restore_101
    btr qword [rsi + 392], 45
    cmp ecx, 52
    jb run_sieve_dense_restore_101
    btr qword [rsi + 408], 18
    cmp ecx, 53
    jb run_sieve_dense_restore_101
    btr qword [rsi + 416], 55
    cmp ecx, 55
    jb run_sieve_dense_restore_101
    btr qword [rsi + 432], 28
    cmp ecx, 57
    jb run_sieve_dense_restore_101
    btr qword [rsi + 448], 1
    cmp ecx, 58
    jb run_sieve_dense_restore_101
    btr qword [rsi + 456], 38
    cmp ecx, 60
    jb run_sieve_dense_restore_101
    btr qword [rsi + 472], 11
    cmp ecx, 61
    jb run_sieve_dense_restore_101
    btr qword [rsi + 480], 48
    cmp ecx, 63
    jb run_sieve_dense_restore_101
    btr qword [rsi + 496], 21
    cmp ecx, 64
    jb run_sieve_dense_restore_101
    btr qword [rsi + 504], 58
    cmp ecx, 66
    jb run_sieve_dense_restore_101
    btr qword [rsi + 520], 31
    cmp ecx, 68
    jb run_sieve_dense_restore_101
    btr qword [rsi + 536], 4
    cmp ecx, 69
    jb run_sieve_dense_restore_101
    btr qword [rsi + 544], 41
    cmp ecx, 71
    jb run_sieve_dense_restore_101
    btr qword [rsi + 560], 14
    cmp ecx, 72
    jb run_sieve_dense_restore_101
    btr qword [rsi + 568], 51
    cmp ecx, 74
    jb run_sieve_dense_restore_101
    btr qword [rsi + 584], 24
    cmp ecx, 75
    jb run_sieve_dense_restore_101
    btr qword [rsi + 592], 61
    cmp ecx, 77
    jb run_sieve_dense_restore_101
    btr qword [rsi + 608], 34
    cmp ecx, 79
    jb run_sieve_dense_restore_101
    btr qword [rsi + 624], 7
    cmp ecx, 80
    jb run_sieve_dense_restore_101
    btr qword [rsi + 632], 44
    cmp ecx, 82
    jb run_sieve_dense_restore_101
    btr qword [rsi + 648], 17
    cmp ecx, 83
    jb run_sieve_dense_restore_101
    btr qword [rsi + 656], 54
    cmp ecx, 85
    jb run_sieve_dense_restore_101
    btr qword [rsi + 672], 27
    cmp ecx, 87
    jb run_sieve_dense_restore_101
    btr qword [rsi + 688], 0
    cmp ecx, 88
    jb run_sieve_dense_restore_101
    btr qword [rsi + 696], 37
    cmp ecx, 90
    jb run_sieve_dense_restore_101
    btr qword [rsi + 712], 10
    cmp ecx, 91
    jb run_sieve_dense_restore_101
    btr qword [rsi + 720], 47
    cmp ecx, 93
    jb run_sieve_dense_restore_101
    btr qword [rsi + 736], 20
    cmp ecx, 94
    jb run_sieve_dense_restore_101
    btr qword [rsi + 744], 57
    cmp ecx, 96
    jb run_sieve_dense_restore_101
    btr qword [rsi + 760], 30
    cmp ecx, 98
    jb run_sieve_dense_restore_101
    btr qword [rsi + 776], 3
    cmp ecx, 99
    jb run_sieve_dense_restore_101
    btr qword [rsi + 784], 40
    cmp ecx, 101
    jb run_sieve_dense_restore_101
    btr qword [rsi + 800], 13
run_sieve_dense_restore_101:
    bts qword [r14], 50
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_103:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 103
    jb run_sieve_dense_tail_103
align 16
run_sieve_dense_loop_103:
    btr qword [rsi], 51
    btr qword [rsi + 16], 26
    btr qword [rsi + 32], 1
    btr qword [rsi + 40], 40
    btr qword [rsi + 56], 15
    btr qword [rsi + 64], 54
    btr qword [rsi + 80], 29
    btr qword [rsi + 96], 4
    btr qword [rsi + 104], 43
    btr qword [rsi + 120], 18
    btr qword [rsi + 128], 57
    btr qword [rsi + 144], 32
    btr qword [rsi + 160], 7
    btr qword [rsi + 168], 46
    btr qword [rsi + 184], 21
    btr qword [rsi + 192], 60
    btr qword [rsi + 208], 35
    btr qword [rsi + 224], 10
    btr qword [rsi + 232], 49
    btr qword [rsi + 248], 24
    btr qword [rsi + 256], 63
    btr qword [rsi + 272], 38
    btr qword [rsi + 288], 13
    btr qword [rsi + 296], 52
    btr qword [rsi + 312], 27
    btr qword [rsi + 328], 2
    btr qword [rsi + 336], 41
    btr qword [rsi + 352], 16
    btr qword [rsi + 360], 55
    btr qword [rsi + 376], 30
    btr qword [rsi + 392], 5
    btr qword [rsi + 400], 44
    btr qword [rsi + 416], 19
    btr qword [rsi + 424], 58
    btr qword [rsi + 440], 33
    btr qword [rsi + 456], 8
    btr qword [rsi + 464], 47
    btr qword [rsi + 480], 22
    btr qword [rsi + 488], 61
    btr qword [rsi + 504], 36
    btr qword [rsi + 520], 11
    btr qword [rsi + 528], 50
    btr qword [rsi + 544], 25
    btr qword [rsi + 560], 0
    btr qword [rsi + 568], 39
    btr qword [rsi + 584], 14
    btr qword [rsi + 592], 53
    btr qword [rsi + 608], 28
    btr qword [rsi + 624], 3
    btr qword [rsi + 632], 42
    btr qword [rsi + 648], 17
    btr qword [rsi + 656], 56
    btr qword [rsi + 672], 31
    btr qword [rsi + 688], 6
    btr qword [rsi + 696], 45
    btr qword [rsi + 712], 20
    btr qword [rsi + 720], 59
    btr qword [rsi + 736], 34
    btr qword [rsi + 752], 9
    btr qword [rsi + 760], 48
    btr qword [rsi + 776], 23
    btr qword [rsi + 784], 62
    btr qword [rsi + 800], 37
    btr qword [rsi + 816], 12
    add rsi, 824
    sub ecx, 103
    cmp ecx, 103
    jae run_sieve_dense_loop_103
run_sieve_dense_tail_103:
    test ecx, ecx
    jz run_sieve_dense_restore_103
    cmp ecx, 1
    jb run_sieve_dense_restore_103
    btr qword [rsi], 51
    cmp ecx, 3
    jb run_sieve_dense_restore_103
    btr qword [rsi + 16], 26
    cmp ecx, 5
    jb run_sieve_dense_restore_103
    btr qword [rsi + 32], 1
    cmp ecx, 6
    jb run_sieve_dense_restore_103
    btr qword [rsi + 40], 40
    cmp ecx, 8
    jb run_sieve_dense_restore_103
    btr qword [rsi + 56], 15
    cmp ecx, 9
    jb run_sieve_dense_restore_103
    btr qword [rsi + 64], 54
    cmp ecx, 11
    jb run_sieve_dense_restore_103
    btr qword [rsi + 80], 29
    cmp ecx, 13
    jb run_sieve_dense_restore_103
    btr qword [rsi + 96], 4
    cmp ecx, 14
    jb run_sieve_dense_restore_103
    btr qword [rsi + 104], 43
    cmp ecx, 16
    jb run_sieve_dense_restore_103
    btr qword [rsi + 120], 18
    cmp ecx, 17
    jb run_sieve_dense_restore_103
    btr qword [rsi + 128], 57
    cmp ecx, 19
    jb run_sieve_dense_restore_103
    btr qword [rsi + 144], 32
    cmp ecx, 21
    jb run_sieve_dense_restore_103
    btr qword [rsi + 160], 7
    cmp ecx, 22
    jb run_sieve_dense_restore_103
    btr qword [rsi + 168], 46
    cmp ecx, 24
    jb run_sieve_dense_restore_103
    btr qword [rsi + 184], 21
    cmp ecx, 25
    jb run_sieve_dense_restore_103
    btr qword [rsi + 192], 60
    cmp ecx, 27
    jb run_sieve_dense_restore_103
    btr qword [rsi + 208], 35
    cmp ecx, 29
    jb run_sieve_dense_restore_103
    btr qword [rsi + 224], 10
    cmp ecx, 30
    jb run_sieve_dense_restore_103
    btr qword [rsi + 232], 49
    cmp ecx, 32
    jb run_sieve_dense_restore_103
    btr qword [rsi + 248], 24
    cmp ecx, 33
    jb run_sieve_dense_restore_103
    btr qword [rsi + 256], 63
    cmp ecx, 35
    jb run_sieve_dense_restore_103
    btr qword [rsi + 272], 38
    cmp ecx, 37
    jb run_sieve_dense_restore_103
    btr qword [rsi + 288], 13
    cmp ecx, 38
    jb run_sieve_dense_restore_103
    btr qword [rsi + 296], 52
    cmp ecx, 40
    jb run_sieve_dense_restore_103
    btr qword [rsi + 312], 27
    cmp ecx, 42
    jb run_sieve_dense_restore_103
    btr qword [rsi + 328], 2
    cmp ecx, 43
    jb run_sieve_dense_restore_103
    btr qword [rsi + 336], 41
    cmp ecx, 45
    jb run_sieve_dense_restore_103
    btr qword [rsi + 352], 16
    cmp ecx, 46
    jb run_sieve_dense_restore_103
    btr qword [rsi + 360], 55
    cmp ecx, 48
    jb run_sieve_dense_restore_103
    btr qword [rsi + 376], 30
    cmp ecx, 50
    jb run_sieve_dense_restore_103
    btr qword [rsi + 392], 5
    cmp ecx, 51
    jb run_sieve_dense_restore_103
    btr qword [rsi + 400], 44
    cmp ecx, 53
    jb run_sieve_dense_restore_103
    btr qword [rsi + 416], 19
    cmp ecx, 54
    jb run_sieve_dense_restore_103
    btr qword [rsi + 424], 58
    cmp ecx, 56
    jb run_sieve_dense_restore_103
    btr qword [rsi + 440], 33
    cmp ecx, 58
    jb run_sieve_dense_restore_103
    btr qword [rsi + 456], 8
    cmp ecx, 59
    jb run_sieve_dense_restore_103
    btr qword [rsi + 464], 47
    cmp ecx, 61
    jb run_sieve_dense_restore_103
    btr qword [rsi + 480], 22
    cmp ecx, 62
    jb run_sieve_dense_restore_103
    btr qword [rsi + 488], 61
    cmp ecx, 64
    jb run_sieve_dense_restore_103
    btr qword [rsi + 504], 36
    cmp ecx, 66
    jb run_sieve_dense_restore_103
    btr qword [rsi + 520], 11
    cmp ecx, 67
    jb run_sieve_dense_restore_103
    btr qword [rsi + 528], 50
    cmp ecx, 69
    jb run_sieve_dense_restore_103
    btr qword [rsi + 544], 25
    cmp ecx, 71
    jb run_sieve_dense_restore_103
    btr qword [rsi + 560], 0
    cmp ecx, 72
    jb run_sieve_dense_restore_103
    btr qword [rsi + 568], 39
    cmp ecx, 74
    jb run_sieve_dense_restore_103
    btr qword [rsi + 584], 14
    cmp ecx, 75
    jb run_sieve_dense_restore_103
    btr qword [rsi + 592], 53
    cmp ecx, 77
    jb run_sieve_dense_restore_103
    btr qword [rsi + 608], 28
    cmp ecx, 79
    jb run_sieve_dense_restore_103
    btr qword [rsi + 624], 3
    cmp ecx, 80
    jb run_sieve_dense_restore_103
    btr qword [rsi + 632], 42
    cmp ecx, 82
    jb run_sieve_dense_restore_103
    btr qword [rsi + 648], 17
    cmp ecx, 83
    jb run_sieve_dense_restore_103
    btr qword [rsi + 656], 56
    cmp ecx, 85
    jb run_sieve_dense_restore_103
    btr qword [rsi + 672], 31
    cmp ecx, 87
    jb run_sieve_dense_restore_103
    btr qword [rsi + 688], 6
    cmp ecx, 88
    jb run_sieve_dense_restore_103
    btr qword [rsi + 696], 45
    cmp ecx, 90
    jb run_sieve_dense_restore_103
    btr qword [rsi + 712], 20
    cmp ecx, 91
    jb run_sieve_dense_restore_103
    btr qword [rsi + 720], 59
    cmp ecx, 93
    jb run_sieve_dense_restore_103
    btr qword [rsi + 736], 34
    cmp ecx, 95
    jb run_sieve_dense_restore_103
    btr qword [rsi + 752], 9
    cmp ecx, 96
    jb run_sieve_dense_restore_103
    btr qword [rsi + 760], 48
    cmp ecx, 98
    jb run_sieve_dense_restore_103
    btr qword [rsi + 776], 23
    cmp ecx, 99
    jb run_sieve_dense_restore_103
    btr qword [rsi + 784], 62
    cmp ecx, 101
    jb run_sieve_dense_restore_103
    btr qword [rsi + 800], 37
    cmp ecx, 103
    jb run_sieve_dense_restore_103
    btr qword [rsi + 816], 12
run_sieve_dense_restore_103:
    bts qword [r14], 51
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_105:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 105
    jb run_sieve_dense_tail_105
align 16
run_sieve_dense_loop_105:
    btr qword [rsi], 52
    btr qword [rsi + 16], 29
    btr qword [rsi + 32], 6
    btr qword [rsi + 40], 47
    btr qword [rsi + 56], 24
    btr qword [rsi + 72], 1
    btr qword [rsi + 80], 42
    btr qword [rsi + 96], 19
    btr qword [rsi + 104], 60
    btr qword [rsi + 120], 37
    btr qword [rsi + 136], 14
    btr qword [rsi + 144], 55
    btr qword [rsi + 160], 32
    btr qword [rsi + 176], 9
    btr qword [rsi + 184], 50
    btr qword [rsi + 200], 27
    btr qword [rsi + 216], 4
    btr qword [rsi + 224], 45
    btr qword [rsi + 240], 22
    btr qword [rsi + 248], 63
    btr qword [rsi + 264], 40
    btr qword [rsi + 280], 17
    btr qword [rsi + 288], 58
    btr qword [rsi + 304], 35
    btr qword [rsi + 320], 12
    btr qword [rsi + 328], 53
    btr qword [rsi + 344], 30
    btr qword [rsi + 360], 7
    btr qword [rsi + 368], 48
    btr qword [rsi + 384], 25
    btr qword [rsi + 400], 2
    btr qword [rsi + 408], 43
    btr qword [rsi + 424], 20
    btr qword [rsi + 432], 61
    btr qword [rsi + 448], 38
    btr qword [rsi + 464], 15
    btr qword [rsi + 472], 56
    btr qword [rsi + 488], 33
    btr qword [rsi + 504], 10
    btr qword [rsi + 512], 51
    btr qword [rsi + 528], 28
    btr qword [rsi + 544], 5
    btr qword [rsi + 552], 46
    btr qword [rsi + 568], 23
    btr qword [rsi + 584], 0
    btr qword [rsi + 592], 41
    btr qword [rsi + 608], 18
    btr qword [rsi + 616], 59
    btr qword [rsi + 632], 36
    btr qword [rsi + 648], 13
    btr qword [rsi + 656], 54
    btr qword [rsi + 672], 31
    btr qword [rsi + 688], 8
    btr qword [rsi + 696], 49
    btr qword [rsi + 712], 26
    btr qword [rsi + 728], 3
    btr qword [rsi + 736], 44
    btr qword [rsi + 752], 21
    btr qword [rsi + 760], 62
    btr qword [rsi + 776], 39
    btr qword [rsi + 792], 16
    btr qword [rsi + 800], 57
    btr qword [rsi + 816], 34
    btr qword [rsi + 832], 11
    add rsi, 840
    sub ecx, 105
    cmp ecx, 105
    jae run_sieve_dense_loop_105
run_sieve_dense_tail_105:
    test ecx, ecx
    jz run_sieve_dense_restore_105
    cmp ecx, 1
    jb run_sieve_dense_restore_105
    btr qword [rsi], 52
    cmp ecx, 3
    jb run_sieve_dense_restore_105
    btr qword [rsi + 16], 29
    cmp ecx, 5
    jb run_sieve_dense_restore_105
    btr qword [rsi + 32], 6
    cmp ecx, 6
    jb run_sieve_dense_restore_105
    btr qword [rsi + 40], 47
    cmp ecx, 8
    jb run_sieve_dense_restore_105
    btr qword [rsi + 56], 24
    cmp ecx, 10
    jb run_sieve_dense_restore_105
    btr qword [rsi + 72], 1
    cmp ecx, 11
    jb run_sieve_dense_restore_105
    btr qword [rsi + 80], 42
    cmp ecx, 13
    jb run_sieve_dense_restore_105
    btr qword [rsi + 96], 19
    cmp ecx, 14
    jb run_sieve_dense_restore_105
    btr qword [rsi + 104], 60
    cmp ecx, 16
    jb run_sieve_dense_restore_105
    btr qword [rsi + 120], 37
    cmp ecx, 18
    jb run_sieve_dense_restore_105
    btr qword [rsi + 136], 14
    cmp ecx, 19
    jb run_sieve_dense_restore_105
    btr qword [rsi + 144], 55
    cmp ecx, 21
    jb run_sieve_dense_restore_105
    btr qword [rsi + 160], 32
    cmp ecx, 23
    jb run_sieve_dense_restore_105
    btr qword [rsi + 176], 9
    cmp ecx, 24
    jb run_sieve_dense_restore_105
    btr qword [rsi + 184], 50
    cmp ecx, 26
    jb run_sieve_dense_restore_105
    btr qword [rsi + 200], 27
    cmp ecx, 28
    jb run_sieve_dense_restore_105
    btr qword [rsi + 216], 4
    cmp ecx, 29
    jb run_sieve_dense_restore_105
    btr qword [rsi + 224], 45
    cmp ecx, 31
    jb run_sieve_dense_restore_105
    btr qword [rsi + 240], 22
    cmp ecx, 32
    jb run_sieve_dense_restore_105
    btr qword [rsi + 248], 63
    cmp ecx, 34
    jb run_sieve_dense_restore_105
    btr qword [rsi + 264], 40
    cmp ecx, 36
    jb run_sieve_dense_restore_105
    btr qword [rsi + 280], 17
    cmp ecx, 37
    jb run_sieve_dense_restore_105
    btr qword [rsi + 288], 58
    cmp ecx, 39
    jb run_sieve_dense_restore_105
    btr qword [rsi + 304], 35
    cmp ecx, 41
    jb run_sieve_dense_restore_105
    btr qword [rsi + 320], 12
    cmp ecx, 42
    jb run_sieve_dense_restore_105
    btr qword [rsi + 328], 53
    cmp ecx, 44
    jb run_sieve_dense_restore_105
    btr qword [rsi + 344], 30
    cmp ecx, 46
    jb run_sieve_dense_restore_105
    btr qword [rsi + 360], 7
    cmp ecx, 47
    jb run_sieve_dense_restore_105
    btr qword [rsi + 368], 48
    cmp ecx, 49
    jb run_sieve_dense_restore_105
    btr qword [rsi + 384], 25
    cmp ecx, 51
    jb run_sieve_dense_restore_105
    btr qword [rsi + 400], 2
    cmp ecx, 52
    jb run_sieve_dense_restore_105
    btr qword [rsi + 408], 43
    cmp ecx, 54
    jb run_sieve_dense_restore_105
    btr qword [rsi + 424], 20
    cmp ecx, 55
    jb run_sieve_dense_restore_105
    btr qword [rsi + 432], 61
    cmp ecx, 57
    jb run_sieve_dense_restore_105
    btr qword [rsi + 448], 38
    cmp ecx, 59
    jb run_sieve_dense_restore_105
    btr qword [rsi + 464], 15
    cmp ecx, 60
    jb run_sieve_dense_restore_105
    btr qword [rsi + 472], 56
    cmp ecx, 62
    jb run_sieve_dense_restore_105
    btr qword [rsi + 488], 33
    cmp ecx, 64
    jb run_sieve_dense_restore_105
    btr qword [rsi + 504], 10
    cmp ecx, 65
    jb run_sieve_dense_restore_105
    btr qword [rsi + 512], 51
    cmp ecx, 67
    jb run_sieve_dense_restore_105
    btr qword [rsi + 528], 28
    cmp ecx, 69
    jb run_sieve_dense_restore_105
    btr qword [rsi + 544], 5
    cmp ecx, 70
    jb run_sieve_dense_restore_105
    btr qword [rsi + 552], 46
    cmp ecx, 72
    jb run_sieve_dense_restore_105
    btr qword [rsi + 568], 23
    cmp ecx, 74
    jb run_sieve_dense_restore_105
    btr qword [rsi + 584], 0
    cmp ecx, 75
    jb run_sieve_dense_restore_105
    btr qword [rsi + 592], 41
    cmp ecx, 77
    jb run_sieve_dense_restore_105
    btr qword [rsi + 608], 18
    cmp ecx, 78
    jb run_sieve_dense_restore_105
    btr qword [rsi + 616], 59
    cmp ecx, 80
    jb run_sieve_dense_restore_105
    btr qword [rsi + 632], 36
    cmp ecx, 82
    jb run_sieve_dense_restore_105
    btr qword [rsi + 648], 13
    cmp ecx, 83
    jb run_sieve_dense_restore_105
    btr qword [rsi + 656], 54
    cmp ecx, 85
    jb run_sieve_dense_restore_105
    btr qword [rsi + 672], 31
    cmp ecx, 87
    jb run_sieve_dense_restore_105
    btr qword [rsi + 688], 8
    cmp ecx, 88
    jb run_sieve_dense_restore_105
    btr qword [rsi + 696], 49
    cmp ecx, 90
    jb run_sieve_dense_restore_105
    btr qword [rsi + 712], 26
    cmp ecx, 92
    jb run_sieve_dense_restore_105
    btr qword [rsi + 728], 3
    cmp ecx, 93
    jb run_sieve_dense_restore_105
    btr qword [rsi + 736], 44
    cmp ecx, 95
    jb run_sieve_dense_restore_105
    btr qword [rsi + 752], 21
    cmp ecx, 96
    jb run_sieve_dense_restore_105
    btr qword [rsi + 760], 62
    cmp ecx, 98
    jb run_sieve_dense_restore_105
    btr qword [rsi + 776], 39
    cmp ecx, 100
    jb run_sieve_dense_restore_105
    btr qword [rsi + 792], 16
    cmp ecx, 101
    jb run_sieve_dense_restore_105
    btr qword [rsi + 800], 57
    cmp ecx, 103
    jb run_sieve_dense_restore_105
    btr qword [rsi + 816], 34
    cmp ecx, 105
    jb run_sieve_dense_restore_105
    btr qword [rsi + 832], 11
run_sieve_dense_restore_105:
    bts qword [r14], 52
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_107:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 107
    jb run_sieve_dense_tail_107
align 16
run_sieve_dense_loop_107:
    btr qword [rsi], 53
    btr qword [rsi + 16], 32
    btr qword [rsi + 32], 11
    btr qword [rsi + 40], 54
    btr qword [rsi + 56], 33
    btr qword [rsi + 72], 12
    btr qword [rsi + 80], 55
    btr qword [rsi + 96], 34
    btr qword [rsi + 112], 13
    btr qword [rsi + 120], 56
    btr qword [rsi + 136], 35
    btr qword [rsi + 152], 14
    btr qword [rsi + 160], 57
    btr qword [rsi + 176], 36
    btr qword [rsi + 192], 15
    btr qword [rsi + 200], 58
    btr qword [rsi + 216], 37
    btr qword [rsi + 232], 16
    btr qword [rsi + 240], 59
    btr qword [rsi + 256], 38
    btr qword [rsi + 272], 17
    btr qword [rsi + 280], 60
    btr qword [rsi + 296], 39
    btr qword [rsi + 312], 18
    btr qword [rsi + 320], 61
    btr qword [rsi + 336], 40
    btr qword [rsi + 352], 19
    btr qword [rsi + 360], 62
    btr qword [rsi + 376], 41
    btr qword [rsi + 392], 20
    btr qword [rsi + 400], 63
    btr qword [rsi + 416], 42
    btr qword [rsi + 432], 21
    btr qword [rsi + 448], 0
    btr qword [rsi + 456], 43
    btr qword [rsi + 472], 22
    btr qword [rsi + 488], 1
    btr qword [rsi + 496], 44
    btr qword [rsi + 512], 23
    btr qword [rsi + 528], 2
    btr qword [rsi + 536], 45
    btr qword [rsi + 552], 24
    btr qword [rsi + 568], 3
    btr qword [rsi + 576], 46
    btr qword [rsi + 592], 25
    btr qword [rsi + 608], 4
    btr qword [rsi + 616], 47
    btr qword [rsi + 632], 26
    btr qword [rsi + 648], 5
    btr qword [rsi + 656], 48
    btr qword [rsi + 672], 27
    btr qword [rsi + 688], 6
    btr qword [rsi + 696], 49
    btr qword [rsi + 712], 28
    btr qword [rsi + 728], 7
    btr qword [rsi + 736], 50
    btr qword [rsi + 752], 29
    btr qword [rsi + 768], 8
    btr qword [rsi + 776], 51
    btr qword [rsi + 792], 30
    btr qword [rsi + 808], 9
    btr qword [rsi + 816], 52
    btr qword [rsi + 832], 31
    btr qword [rsi + 848], 10
    add rsi, 856
    sub ecx, 107
    cmp ecx, 107
    jae run_sieve_dense_loop_107
run_sieve_dense_tail_107:
    test ecx, ecx
    jz run_sieve_dense_restore_107
    cmp ecx, 1
    jb run_sieve_dense_restore_107
    btr qword [rsi], 53
    cmp ecx, 3
    jb run_sieve_dense_restore_107
    btr qword [rsi + 16], 32
    cmp ecx, 5
    jb run_sieve_dense_restore_107
    btr qword [rsi + 32], 11
    cmp ecx, 6
    jb run_sieve_dense_restore_107
    btr qword [rsi + 40], 54
    cmp ecx, 8
    jb run_sieve_dense_restore_107
    btr qword [rsi + 56], 33
    cmp ecx, 10
    jb run_sieve_dense_restore_107
    btr qword [rsi + 72], 12
    cmp ecx, 11
    jb run_sieve_dense_restore_107
    btr qword [rsi + 80], 55
    cmp ecx, 13
    jb run_sieve_dense_restore_107
    btr qword [rsi + 96], 34
    cmp ecx, 15
    jb run_sieve_dense_restore_107
    btr qword [rsi + 112], 13
    cmp ecx, 16
    jb run_sieve_dense_restore_107
    btr qword [rsi + 120], 56
    cmp ecx, 18
    jb run_sieve_dense_restore_107
    btr qword [rsi + 136], 35
    cmp ecx, 20
    jb run_sieve_dense_restore_107
    btr qword [rsi + 152], 14
    cmp ecx, 21
    jb run_sieve_dense_restore_107
    btr qword [rsi + 160], 57
    cmp ecx, 23
    jb run_sieve_dense_restore_107
    btr qword [rsi + 176], 36
    cmp ecx, 25
    jb run_sieve_dense_restore_107
    btr qword [rsi + 192], 15
    cmp ecx, 26
    jb run_sieve_dense_restore_107
    btr qword [rsi + 200], 58
    cmp ecx, 28
    jb run_sieve_dense_restore_107
    btr qword [rsi + 216], 37
    cmp ecx, 30
    jb run_sieve_dense_restore_107
    btr qword [rsi + 232], 16
    cmp ecx, 31
    jb run_sieve_dense_restore_107
    btr qword [rsi + 240], 59
    cmp ecx, 33
    jb run_sieve_dense_restore_107
    btr qword [rsi + 256], 38
    cmp ecx, 35
    jb run_sieve_dense_restore_107
    btr qword [rsi + 272], 17
    cmp ecx, 36
    jb run_sieve_dense_restore_107
    btr qword [rsi + 280], 60
    cmp ecx, 38
    jb run_sieve_dense_restore_107
    btr qword [rsi + 296], 39
    cmp ecx, 40
    jb run_sieve_dense_restore_107
    btr qword [rsi + 312], 18
    cmp ecx, 41
    jb run_sieve_dense_restore_107
    btr qword [rsi + 320], 61
    cmp ecx, 43
    jb run_sieve_dense_restore_107
    btr qword [rsi + 336], 40
    cmp ecx, 45
    jb run_sieve_dense_restore_107
    btr qword [rsi + 352], 19
    cmp ecx, 46
    jb run_sieve_dense_restore_107
    btr qword [rsi + 360], 62
    cmp ecx, 48
    jb run_sieve_dense_restore_107
    btr qword [rsi + 376], 41
    cmp ecx, 50
    jb run_sieve_dense_restore_107
    btr qword [rsi + 392], 20
    cmp ecx, 51
    jb run_sieve_dense_restore_107
    btr qword [rsi + 400], 63
    cmp ecx, 53
    jb run_sieve_dense_restore_107
    btr qword [rsi + 416], 42
    cmp ecx, 55
    jb run_sieve_dense_restore_107
    btr qword [rsi + 432], 21
    cmp ecx, 57
    jb run_sieve_dense_restore_107
    btr qword [rsi + 448], 0
    cmp ecx, 58
    jb run_sieve_dense_restore_107
    btr qword [rsi + 456], 43
    cmp ecx, 60
    jb run_sieve_dense_restore_107
    btr qword [rsi + 472], 22
    cmp ecx, 62
    jb run_sieve_dense_restore_107
    btr qword [rsi + 488], 1
    cmp ecx, 63
    jb run_sieve_dense_restore_107
    btr qword [rsi + 496], 44
    cmp ecx, 65
    jb run_sieve_dense_restore_107
    btr qword [rsi + 512], 23
    cmp ecx, 67
    jb run_sieve_dense_restore_107
    btr qword [rsi + 528], 2
    cmp ecx, 68
    jb run_sieve_dense_restore_107
    btr qword [rsi + 536], 45
    cmp ecx, 70
    jb run_sieve_dense_restore_107
    btr qword [rsi + 552], 24
    cmp ecx, 72
    jb run_sieve_dense_restore_107
    btr qword [rsi + 568], 3
    cmp ecx, 73
    jb run_sieve_dense_restore_107
    btr qword [rsi + 576], 46
    cmp ecx, 75
    jb run_sieve_dense_restore_107
    btr qword [rsi + 592], 25
    cmp ecx, 77
    jb run_sieve_dense_restore_107
    btr qword [rsi + 608], 4
    cmp ecx, 78
    jb run_sieve_dense_restore_107
    btr qword [rsi + 616], 47
    cmp ecx, 80
    jb run_sieve_dense_restore_107
    btr qword [rsi + 632], 26
    cmp ecx, 82
    jb run_sieve_dense_restore_107
    btr qword [rsi + 648], 5
    cmp ecx, 83
    jb run_sieve_dense_restore_107
    btr qword [rsi + 656], 48
    cmp ecx, 85
    jb run_sieve_dense_restore_107
    btr qword [rsi + 672], 27
    cmp ecx, 87
    jb run_sieve_dense_restore_107
    btr qword [rsi + 688], 6
    cmp ecx, 88
    jb run_sieve_dense_restore_107
    btr qword [rsi + 696], 49
    cmp ecx, 90
    jb run_sieve_dense_restore_107
    btr qword [rsi + 712], 28
    cmp ecx, 92
    jb run_sieve_dense_restore_107
    btr qword [rsi + 728], 7
    cmp ecx, 93
    jb run_sieve_dense_restore_107
    btr qword [rsi + 736], 50
    cmp ecx, 95
    jb run_sieve_dense_restore_107
    btr qword [rsi + 752], 29
    cmp ecx, 97
    jb run_sieve_dense_restore_107
    btr qword [rsi + 768], 8
    cmp ecx, 98
    jb run_sieve_dense_restore_107
    btr qword [rsi + 776], 51
    cmp ecx, 100
    jb run_sieve_dense_restore_107
    btr qword [rsi + 792], 30
    cmp ecx, 102
    jb run_sieve_dense_restore_107
    btr qword [rsi + 808], 9
    cmp ecx, 103
    jb run_sieve_dense_restore_107
    btr qword [rsi + 816], 52
    cmp ecx, 105
    jb run_sieve_dense_restore_107
    btr qword [rsi + 832], 31
    cmp ecx, 107
    jb run_sieve_dense_restore_107
    btr qword [rsi + 848], 10
run_sieve_dense_restore_107:
    bts qword [r14], 53
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_109:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 109
    jb run_sieve_dense_tail_109
align 16
run_sieve_dense_loop_109:
    btr qword [rsi], 54
    btr qword [rsi + 16], 35
    btr qword [rsi + 32], 16
    btr qword [rsi + 40], 61
    btr qword [rsi + 56], 42
    btr qword [rsi + 72], 23
    btr qword [rsi + 88], 4
    btr qword [rsi + 96], 49
    btr qword [rsi + 112], 30
    btr qword [rsi + 128], 11
    btr qword [rsi + 136], 56
    btr qword [rsi + 152], 37
    btr qword [rsi + 168], 18
    btr qword [rsi + 176], 63
    btr qword [rsi + 192], 44
    btr qword [rsi + 208], 25
    btr qword [rsi + 224], 6
    btr qword [rsi + 232], 51
    btr qword [rsi + 248], 32
    btr qword [rsi + 264], 13
    btr qword [rsi + 272], 58
    btr qword [rsi + 288], 39
    btr qword [rsi + 304], 20
    btr qword [rsi + 320], 1
    btr qword [rsi + 328], 46
    btr qword [rsi + 344], 27
    btr qword [rsi + 360], 8
    btr qword [rsi + 368], 53
    btr qword [rsi + 384], 34
    btr qword [rsi + 400], 15
    btr qword [rsi + 408], 60
    btr qword [rsi + 424], 41
    btr qword [rsi + 440], 22
    btr qword [rsi + 456], 3
    btr qword [rsi + 464], 48
    btr qword [rsi + 480], 29
    btr qword [rsi + 496], 10
    btr qword [rsi + 504], 55
    btr qword [rsi + 520], 36
    btr qword [rsi + 536], 17
    btr qword [rsi + 544], 62
    btr qword [rsi + 560], 43
    btr qword [rsi + 576], 24
    btr qword [rsi + 592], 5
    btr qword [rsi + 600], 50
    btr qword [rsi + 616], 31
    btr qword [rsi + 632], 12
    btr qword [rsi + 640], 57
    btr qword [rsi + 656], 38
    btr qword [rsi + 672], 19
    btr qword [rsi + 688], 0
    btr qword [rsi + 696], 45
    btr qword [rsi + 712], 26
    btr qword [rsi + 728], 7
    btr qword [rsi + 736], 52
    btr qword [rsi + 752], 33
    btr qword [rsi + 768], 14
    btr qword [rsi + 776], 59
    btr qword [rsi + 792], 40
    btr qword [rsi + 808], 21
    btr qword [rsi + 824], 2
    btr qword [rsi + 832], 47
    btr qword [rsi + 848], 28
    btr qword [rsi + 864], 9
    add rsi, 872
    sub ecx, 109
    cmp ecx, 109
    jae run_sieve_dense_loop_109
run_sieve_dense_tail_109:
    test ecx, ecx
    jz run_sieve_dense_restore_109
    cmp ecx, 1
    jb run_sieve_dense_restore_109
    btr qword [rsi], 54
    cmp ecx, 3
    jb run_sieve_dense_restore_109
    btr qword [rsi + 16], 35
    cmp ecx, 5
    jb run_sieve_dense_restore_109
    btr qword [rsi + 32], 16
    cmp ecx, 6
    jb run_sieve_dense_restore_109
    btr qword [rsi + 40], 61
    cmp ecx, 8
    jb run_sieve_dense_restore_109
    btr qword [rsi + 56], 42
    cmp ecx, 10
    jb run_sieve_dense_restore_109
    btr qword [rsi + 72], 23
    cmp ecx, 12
    jb run_sieve_dense_restore_109
    btr qword [rsi + 88], 4
    cmp ecx, 13
    jb run_sieve_dense_restore_109
    btr qword [rsi + 96], 49
    cmp ecx, 15
    jb run_sieve_dense_restore_109
    btr qword [rsi + 112], 30
    cmp ecx, 17
    jb run_sieve_dense_restore_109
    btr qword [rsi + 128], 11
    cmp ecx, 18
    jb run_sieve_dense_restore_109
    btr qword [rsi + 136], 56
    cmp ecx, 20
    jb run_sieve_dense_restore_109
    btr qword [rsi + 152], 37
    cmp ecx, 22
    jb run_sieve_dense_restore_109
    btr qword [rsi + 168], 18
    cmp ecx, 23
    jb run_sieve_dense_restore_109
    btr qword [rsi + 176], 63
    cmp ecx, 25
    jb run_sieve_dense_restore_109
    btr qword [rsi + 192], 44
    cmp ecx, 27
    jb run_sieve_dense_restore_109
    btr qword [rsi + 208], 25
    cmp ecx, 29
    jb run_sieve_dense_restore_109
    btr qword [rsi + 224], 6
    cmp ecx, 30
    jb run_sieve_dense_restore_109
    btr qword [rsi + 232], 51
    cmp ecx, 32
    jb run_sieve_dense_restore_109
    btr qword [rsi + 248], 32
    cmp ecx, 34
    jb run_sieve_dense_restore_109
    btr qword [rsi + 264], 13
    cmp ecx, 35
    jb run_sieve_dense_restore_109
    btr qword [rsi + 272], 58
    cmp ecx, 37
    jb run_sieve_dense_restore_109
    btr qword [rsi + 288], 39
    cmp ecx, 39
    jb run_sieve_dense_restore_109
    btr qword [rsi + 304], 20
    cmp ecx, 41
    jb run_sieve_dense_restore_109
    btr qword [rsi + 320], 1
    cmp ecx, 42
    jb run_sieve_dense_restore_109
    btr qword [rsi + 328], 46
    cmp ecx, 44
    jb run_sieve_dense_restore_109
    btr qword [rsi + 344], 27
    cmp ecx, 46
    jb run_sieve_dense_restore_109
    btr qword [rsi + 360], 8
    cmp ecx, 47
    jb run_sieve_dense_restore_109
    btr qword [rsi + 368], 53
    cmp ecx, 49
    jb run_sieve_dense_restore_109
    btr qword [rsi + 384], 34
    cmp ecx, 51
    jb run_sieve_dense_restore_109
    btr qword [rsi + 400], 15
    cmp ecx, 52
    jb run_sieve_dense_restore_109
    btr qword [rsi + 408], 60
    cmp ecx, 54
    jb run_sieve_dense_restore_109
    btr qword [rsi + 424], 41
    cmp ecx, 56
    jb run_sieve_dense_restore_109
    btr qword [rsi + 440], 22
    cmp ecx, 58
    jb run_sieve_dense_restore_109
    btr qword [rsi + 456], 3
    cmp ecx, 59
    jb run_sieve_dense_restore_109
    btr qword [rsi + 464], 48
    cmp ecx, 61
    jb run_sieve_dense_restore_109
    btr qword [rsi + 480], 29
    cmp ecx, 63
    jb run_sieve_dense_restore_109
    btr qword [rsi + 496], 10
    cmp ecx, 64
    jb run_sieve_dense_restore_109
    btr qword [rsi + 504], 55
    cmp ecx, 66
    jb run_sieve_dense_restore_109
    btr qword [rsi + 520], 36
    cmp ecx, 68
    jb run_sieve_dense_restore_109
    btr qword [rsi + 536], 17
    cmp ecx, 69
    jb run_sieve_dense_restore_109
    btr qword [rsi + 544], 62
    cmp ecx, 71
    jb run_sieve_dense_restore_109
    btr qword [rsi + 560], 43
    cmp ecx, 73
    jb run_sieve_dense_restore_109
    btr qword [rsi + 576], 24
    cmp ecx, 75
    jb run_sieve_dense_restore_109
    btr qword [rsi + 592], 5
    cmp ecx, 76
    jb run_sieve_dense_restore_109
    btr qword [rsi + 600], 50
    cmp ecx, 78
    jb run_sieve_dense_restore_109
    btr qword [rsi + 616], 31
    cmp ecx, 80
    jb run_sieve_dense_restore_109
    btr qword [rsi + 632], 12
    cmp ecx, 81
    jb run_sieve_dense_restore_109
    btr qword [rsi + 640], 57
    cmp ecx, 83
    jb run_sieve_dense_restore_109
    btr qword [rsi + 656], 38
    cmp ecx, 85
    jb run_sieve_dense_restore_109
    btr qword [rsi + 672], 19
    cmp ecx, 87
    jb run_sieve_dense_restore_109
    btr qword [rsi + 688], 0
    cmp ecx, 88
    jb run_sieve_dense_restore_109
    btr qword [rsi + 696], 45
    cmp ecx, 90
    jb run_sieve_dense_restore_109
    btr qword [rsi + 712], 26
    cmp ecx, 92
    jb run_sieve_dense_restore_109
    btr qword [rsi + 728], 7
    cmp ecx, 93
    jb run_sieve_dense_restore_109
    btr qword [rsi + 736], 52
    cmp ecx, 95
    jb run_sieve_dense_restore_109
    btr qword [rsi + 752], 33
    cmp ecx, 97
    jb run_sieve_dense_restore_109
    btr qword [rsi + 768], 14
    cmp ecx, 98
    jb run_sieve_dense_restore_109
    btr qword [rsi + 776], 59
    cmp ecx, 100
    jb run_sieve_dense_restore_109
    btr qword [rsi + 792], 40
    cmp ecx, 102
    jb run_sieve_dense_restore_109
    btr qword [rsi + 808], 21
    cmp ecx, 104
    jb run_sieve_dense_restore_109
    btr qword [rsi + 824], 2
    cmp ecx, 105
    jb run_sieve_dense_restore_109
    btr qword [rsi + 832], 47
    cmp ecx, 107
    jb run_sieve_dense_restore_109
    btr qword [rsi + 848], 28
    cmp ecx, 109
    jb run_sieve_dense_restore_109
    btr qword [rsi + 864], 9
run_sieve_dense_restore_109:
    bts qword [r14], 54
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_111:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 111
    jb run_sieve_dense_tail_111
align 16
run_sieve_dense_loop_111:
    btr qword [rsi], 55
    btr qword [rsi + 16], 38
    btr qword [rsi + 32], 21
    btr qword [rsi + 48], 4
    btr qword [rsi + 56], 51
    btr qword [rsi + 72], 34
    btr qword [rsi + 88], 17
    btr qword [rsi + 104], 0
    btr qword [rsi + 112], 47
    btr qword [rsi + 128], 30
    btr qword [rsi + 144], 13
    btr qword [rsi + 152], 60
    btr qword [rsi + 168], 43
    btr qword [rsi + 184], 26
    btr qword [rsi + 200], 9
    btr qword [rsi + 208], 56
    btr qword [rsi + 224], 39
    btr qword [rsi + 240], 22
    btr qword [rsi + 256], 5
    btr qword [rsi + 264], 52
    btr qword [rsi + 280], 35
    btr qword [rsi + 296], 18
    btr qword [rsi + 312], 1
    btr qword [rsi + 320], 48
    btr qword [rsi + 336], 31
    btr qword [rsi + 352], 14
    btr qword [rsi + 360], 61
    btr qword [rsi + 376], 44
    btr qword [rsi + 392], 27
    btr qword [rsi + 408], 10
    btr qword [rsi + 416], 57
    btr qword [rsi + 432], 40
    btr qword [rsi + 448], 23
    btr qword [rsi + 464], 6
    btr qword [rsi + 472], 53
    btr qword [rsi + 488], 36
    btr qword [rsi + 504], 19
    btr qword [rsi + 520], 2
    btr qword [rsi + 528], 49
    btr qword [rsi + 544], 32
    btr qword [rsi + 560], 15
    btr qword [rsi + 568], 62
    btr qword [rsi + 584], 45
    btr qword [rsi + 600], 28
    btr qword [rsi + 616], 11
    btr qword [rsi + 624], 58
    btr qword [rsi + 640], 41
    btr qword [rsi + 656], 24
    btr qword [rsi + 672], 7
    btr qword [rsi + 680], 54
    btr qword [rsi + 696], 37
    btr qword [rsi + 712], 20
    btr qword [rsi + 728], 3
    btr qword [rsi + 736], 50
    btr qword [rsi + 752], 33
    btr qword [rsi + 768], 16
    btr qword [rsi + 776], 63
    btr qword [rsi + 792], 46
    btr qword [rsi + 808], 29
    btr qword [rsi + 824], 12
    btr qword [rsi + 832], 59
    btr qword [rsi + 848], 42
    btr qword [rsi + 864], 25
    btr qword [rsi + 880], 8
    add rsi, 888
    sub ecx, 111
    cmp ecx, 111
    jae run_sieve_dense_loop_111
run_sieve_dense_tail_111:
    test ecx, ecx
    jz run_sieve_dense_restore_111
    cmp ecx, 1
    jb run_sieve_dense_restore_111
    btr qword [rsi], 55
    cmp ecx, 3
    jb run_sieve_dense_restore_111
    btr qword [rsi + 16], 38
    cmp ecx, 5
    jb run_sieve_dense_restore_111
    btr qword [rsi + 32], 21
    cmp ecx, 7
    jb run_sieve_dense_restore_111
    btr qword [rsi + 48], 4
    cmp ecx, 8
    jb run_sieve_dense_restore_111
    btr qword [rsi + 56], 51
    cmp ecx, 10
    jb run_sieve_dense_restore_111
    btr qword [rsi + 72], 34
    cmp ecx, 12
    jb run_sieve_dense_restore_111
    btr qword [rsi + 88], 17
    cmp ecx, 14
    jb run_sieve_dense_restore_111
    btr qword [rsi + 104], 0
    cmp ecx, 15
    jb run_sieve_dense_restore_111
    btr qword [rsi + 112], 47
    cmp ecx, 17
    jb run_sieve_dense_restore_111
    btr qword [rsi + 128], 30
    cmp ecx, 19
    jb run_sieve_dense_restore_111
    btr qword [rsi + 144], 13
    cmp ecx, 20
    jb run_sieve_dense_restore_111
    btr qword [rsi + 152], 60
    cmp ecx, 22
    jb run_sieve_dense_restore_111
    btr qword [rsi + 168], 43
    cmp ecx, 24
    jb run_sieve_dense_restore_111
    btr qword [rsi + 184], 26
    cmp ecx, 26
    jb run_sieve_dense_restore_111
    btr qword [rsi + 200], 9
    cmp ecx, 27
    jb run_sieve_dense_restore_111
    btr qword [rsi + 208], 56
    cmp ecx, 29
    jb run_sieve_dense_restore_111
    btr qword [rsi + 224], 39
    cmp ecx, 31
    jb run_sieve_dense_restore_111
    btr qword [rsi + 240], 22
    cmp ecx, 33
    jb run_sieve_dense_restore_111
    btr qword [rsi + 256], 5
    cmp ecx, 34
    jb run_sieve_dense_restore_111
    btr qword [rsi + 264], 52
    cmp ecx, 36
    jb run_sieve_dense_restore_111
    btr qword [rsi + 280], 35
    cmp ecx, 38
    jb run_sieve_dense_restore_111
    btr qword [rsi + 296], 18
    cmp ecx, 40
    jb run_sieve_dense_restore_111
    btr qword [rsi + 312], 1
    cmp ecx, 41
    jb run_sieve_dense_restore_111
    btr qword [rsi + 320], 48
    cmp ecx, 43
    jb run_sieve_dense_restore_111
    btr qword [rsi + 336], 31
    cmp ecx, 45
    jb run_sieve_dense_restore_111
    btr qword [rsi + 352], 14
    cmp ecx, 46
    jb run_sieve_dense_restore_111
    btr qword [rsi + 360], 61
    cmp ecx, 48
    jb run_sieve_dense_restore_111
    btr qword [rsi + 376], 44
    cmp ecx, 50
    jb run_sieve_dense_restore_111
    btr qword [rsi + 392], 27
    cmp ecx, 52
    jb run_sieve_dense_restore_111
    btr qword [rsi + 408], 10
    cmp ecx, 53
    jb run_sieve_dense_restore_111
    btr qword [rsi + 416], 57
    cmp ecx, 55
    jb run_sieve_dense_restore_111
    btr qword [rsi + 432], 40
    cmp ecx, 57
    jb run_sieve_dense_restore_111
    btr qword [rsi + 448], 23
    cmp ecx, 59
    jb run_sieve_dense_restore_111
    btr qword [rsi + 464], 6
    cmp ecx, 60
    jb run_sieve_dense_restore_111
    btr qword [rsi + 472], 53
    cmp ecx, 62
    jb run_sieve_dense_restore_111
    btr qword [rsi + 488], 36
    cmp ecx, 64
    jb run_sieve_dense_restore_111
    btr qword [rsi + 504], 19
    cmp ecx, 66
    jb run_sieve_dense_restore_111
    btr qword [rsi + 520], 2
    cmp ecx, 67
    jb run_sieve_dense_restore_111
    btr qword [rsi + 528], 49
    cmp ecx, 69
    jb run_sieve_dense_restore_111
    btr qword [rsi + 544], 32
    cmp ecx, 71
    jb run_sieve_dense_restore_111
    btr qword [rsi + 560], 15
    cmp ecx, 72
    jb run_sieve_dense_restore_111
    btr qword [rsi + 568], 62
    cmp ecx, 74
    jb run_sieve_dense_restore_111
    btr qword [rsi + 584], 45
    cmp ecx, 76
    jb run_sieve_dense_restore_111
    btr qword [rsi + 600], 28
    cmp ecx, 78
    jb run_sieve_dense_restore_111
    btr qword [rsi + 616], 11
    cmp ecx, 79
    jb run_sieve_dense_restore_111
    btr qword [rsi + 624], 58
    cmp ecx, 81
    jb run_sieve_dense_restore_111
    btr qword [rsi + 640], 41
    cmp ecx, 83
    jb run_sieve_dense_restore_111
    btr qword [rsi + 656], 24
    cmp ecx, 85
    jb run_sieve_dense_restore_111
    btr qword [rsi + 672], 7
    cmp ecx, 86
    jb run_sieve_dense_restore_111
    btr qword [rsi + 680], 54
    cmp ecx, 88
    jb run_sieve_dense_restore_111
    btr qword [rsi + 696], 37
    cmp ecx, 90
    jb run_sieve_dense_restore_111
    btr qword [rsi + 712], 20
    cmp ecx, 92
    jb run_sieve_dense_restore_111
    btr qword [rsi + 728], 3
    cmp ecx, 93
    jb run_sieve_dense_restore_111
    btr qword [rsi + 736], 50
    cmp ecx, 95
    jb run_sieve_dense_restore_111
    btr qword [rsi + 752], 33
    cmp ecx, 97
    jb run_sieve_dense_restore_111
    btr qword [rsi + 768], 16
    cmp ecx, 98
    jb run_sieve_dense_restore_111
    btr qword [rsi + 776], 63
    cmp ecx, 100
    jb run_sieve_dense_restore_111
    btr qword [rsi + 792], 46
    cmp ecx, 102
    jb run_sieve_dense_restore_111
    btr qword [rsi + 808], 29
    cmp ecx, 104
    jb run_sieve_dense_restore_111
    btr qword [rsi + 824], 12
    cmp ecx, 105
    jb run_sieve_dense_restore_111
    btr qword [rsi + 832], 59
    cmp ecx, 107
    jb run_sieve_dense_restore_111
    btr qword [rsi + 848], 42
    cmp ecx, 109
    jb run_sieve_dense_restore_111
    btr qword [rsi + 864], 25
    cmp ecx, 111
    jb run_sieve_dense_restore_111
    btr qword [rsi + 880], 8
run_sieve_dense_restore_111:
    bts qword [r14], 55
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_113:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 113
    jb run_sieve_dense_tail_113
align 16
run_sieve_dense_loop_113:
    btr qword [rsi], 56
    btr qword [rsi + 16], 41
    btr qword [rsi + 32], 26
    btr qword [rsi + 48], 11
    btr qword [rsi + 56], 60
    btr qword [rsi + 72], 45
    btr qword [rsi + 88], 30
    btr qword [rsi + 104], 15
    btr qword [rsi + 120], 0
    btr qword [rsi + 128], 49
    btr qword [rsi + 144], 34
    btr qword [rsi + 160], 19
    btr qword [rsi + 176], 4
    btr qword [rsi + 184], 53
    btr qword [rsi + 200], 38
    btr qword [rsi + 216], 23
    btr qword [rsi + 232], 8
    btr qword [rsi + 240], 57
    btr qword [rsi + 256], 42
    btr qword [rsi + 272], 27
    btr qword [rsi + 288], 12
    btr qword [rsi + 296], 61
    btr qword [rsi + 312], 46
    btr qword [rsi + 328], 31
    btr qword [rsi + 344], 16
    btr qword [rsi + 360], 1
    btr qword [rsi + 368], 50
    btr qword [rsi + 384], 35
    btr qword [rsi + 400], 20
    btr qword [rsi + 416], 5
    btr qword [rsi + 424], 54
    btr qword [rsi + 440], 39
    btr qword [rsi + 456], 24
    btr qword [rsi + 472], 9
    btr qword [rsi + 480], 58
    btr qword [rsi + 496], 43
    btr qword [rsi + 512], 28
    btr qword [rsi + 528], 13
    btr qword [rsi + 536], 62
    btr qword [rsi + 552], 47
    btr qword [rsi + 568], 32
    btr qword [rsi + 584], 17
    btr qword [rsi + 600], 2
    btr qword [rsi + 608], 51
    btr qword [rsi + 624], 36
    btr qword [rsi + 640], 21
    btr qword [rsi + 656], 6
    btr qword [rsi + 664], 55
    btr qword [rsi + 680], 40
    btr qword [rsi + 696], 25
    btr qword [rsi + 712], 10
    btr qword [rsi + 720], 59
    btr qword [rsi + 736], 44
    btr qword [rsi + 752], 29
    btr qword [rsi + 768], 14
    btr qword [rsi + 776], 63
    btr qword [rsi + 792], 48
    btr qword [rsi + 808], 33
    btr qword [rsi + 824], 18
    btr qword [rsi + 840], 3
    btr qword [rsi + 848], 52
    btr qword [rsi + 864], 37
    btr qword [rsi + 880], 22
    btr qword [rsi + 896], 7
    add rsi, 904
    sub ecx, 113
    cmp ecx, 113
    jae run_sieve_dense_loop_113
run_sieve_dense_tail_113:
    test ecx, ecx
    jz run_sieve_dense_restore_113
    cmp ecx, 1
    jb run_sieve_dense_restore_113
    btr qword [rsi], 56
    cmp ecx, 3
    jb run_sieve_dense_restore_113
    btr qword [rsi + 16], 41
    cmp ecx, 5
    jb run_sieve_dense_restore_113
    btr qword [rsi + 32], 26
    cmp ecx, 7
    jb run_sieve_dense_restore_113
    btr qword [rsi + 48], 11
    cmp ecx, 8
    jb run_sieve_dense_restore_113
    btr qword [rsi + 56], 60
    cmp ecx, 10
    jb run_sieve_dense_restore_113
    btr qword [rsi + 72], 45
    cmp ecx, 12
    jb run_sieve_dense_restore_113
    btr qword [rsi + 88], 30
    cmp ecx, 14
    jb run_sieve_dense_restore_113
    btr qword [rsi + 104], 15
    cmp ecx, 16
    jb run_sieve_dense_restore_113
    btr qword [rsi + 120], 0
    cmp ecx, 17
    jb run_sieve_dense_restore_113
    btr qword [rsi + 128], 49
    cmp ecx, 19
    jb run_sieve_dense_restore_113
    btr qword [rsi + 144], 34
    cmp ecx, 21
    jb run_sieve_dense_restore_113
    btr qword [rsi + 160], 19
    cmp ecx, 23
    jb run_sieve_dense_restore_113
    btr qword [rsi + 176], 4
    cmp ecx, 24
    jb run_sieve_dense_restore_113
    btr qword [rsi + 184], 53
    cmp ecx, 26
    jb run_sieve_dense_restore_113
    btr qword [rsi + 200], 38
    cmp ecx, 28
    jb run_sieve_dense_restore_113
    btr qword [rsi + 216], 23
    cmp ecx, 30
    jb run_sieve_dense_restore_113
    btr qword [rsi + 232], 8
    cmp ecx, 31
    jb run_sieve_dense_restore_113
    btr qword [rsi + 240], 57
    cmp ecx, 33
    jb run_sieve_dense_restore_113
    btr qword [rsi + 256], 42
    cmp ecx, 35
    jb run_sieve_dense_restore_113
    btr qword [rsi + 272], 27
    cmp ecx, 37
    jb run_sieve_dense_restore_113
    btr qword [rsi + 288], 12
    cmp ecx, 38
    jb run_sieve_dense_restore_113
    btr qword [rsi + 296], 61
    cmp ecx, 40
    jb run_sieve_dense_restore_113
    btr qword [rsi + 312], 46
    cmp ecx, 42
    jb run_sieve_dense_restore_113
    btr qword [rsi + 328], 31
    cmp ecx, 44
    jb run_sieve_dense_restore_113
    btr qword [rsi + 344], 16
    cmp ecx, 46
    jb run_sieve_dense_restore_113
    btr qword [rsi + 360], 1
    cmp ecx, 47
    jb run_sieve_dense_restore_113
    btr qword [rsi + 368], 50
    cmp ecx, 49
    jb run_sieve_dense_restore_113
    btr qword [rsi + 384], 35
    cmp ecx, 51
    jb run_sieve_dense_restore_113
    btr qword [rsi + 400], 20
    cmp ecx, 53
    jb run_sieve_dense_restore_113
    btr qword [rsi + 416], 5
    cmp ecx, 54
    jb run_sieve_dense_restore_113
    btr qword [rsi + 424], 54
    cmp ecx, 56
    jb run_sieve_dense_restore_113
    btr qword [rsi + 440], 39
    cmp ecx, 58
    jb run_sieve_dense_restore_113
    btr qword [rsi + 456], 24
    cmp ecx, 60
    jb run_sieve_dense_restore_113
    btr qword [rsi + 472], 9
    cmp ecx, 61
    jb run_sieve_dense_restore_113
    btr qword [rsi + 480], 58
    cmp ecx, 63
    jb run_sieve_dense_restore_113
    btr qword [rsi + 496], 43
    cmp ecx, 65
    jb run_sieve_dense_restore_113
    btr qword [rsi + 512], 28
    cmp ecx, 67
    jb run_sieve_dense_restore_113
    btr qword [rsi + 528], 13
    cmp ecx, 68
    jb run_sieve_dense_restore_113
    btr qword [rsi + 536], 62
    cmp ecx, 70
    jb run_sieve_dense_restore_113
    btr qword [rsi + 552], 47
    cmp ecx, 72
    jb run_sieve_dense_restore_113
    btr qword [rsi + 568], 32
    cmp ecx, 74
    jb run_sieve_dense_restore_113
    btr qword [rsi + 584], 17
    cmp ecx, 76
    jb run_sieve_dense_restore_113
    btr qword [rsi + 600], 2
    cmp ecx, 77
    jb run_sieve_dense_restore_113
    btr qword [rsi + 608], 51
    cmp ecx, 79
    jb run_sieve_dense_restore_113
    btr qword [rsi + 624], 36
    cmp ecx, 81
    jb run_sieve_dense_restore_113
    btr qword [rsi + 640], 21
    cmp ecx, 83
    jb run_sieve_dense_restore_113
    btr qword [rsi + 656], 6
    cmp ecx, 84
    jb run_sieve_dense_restore_113
    btr qword [rsi + 664], 55
    cmp ecx, 86
    jb run_sieve_dense_restore_113
    btr qword [rsi + 680], 40
    cmp ecx, 88
    jb run_sieve_dense_restore_113
    btr qword [rsi + 696], 25
    cmp ecx, 90
    jb run_sieve_dense_restore_113
    btr qword [rsi + 712], 10
    cmp ecx, 91
    jb run_sieve_dense_restore_113
    btr qword [rsi + 720], 59
    cmp ecx, 93
    jb run_sieve_dense_restore_113
    btr qword [rsi + 736], 44
    cmp ecx, 95
    jb run_sieve_dense_restore_113
    btr qword [rsi + 752], 29
    cmp ecx, 97
    jb run_sieve_dense_restore_113
    btr qword [rsi + 768], 14
    cmp ecx, 98
    jb run_sieve_dense_restore_113
    btr qword [rsi + 776], 63
    cmp ecx, 100
    jb run_sieve_dense_restore_113
    btr qword [rsi + 792], 48
    cmp ecx, 102
    jb run_sieve_dense_restore_113
    btr qword [rsi + 808], 33
    cmp ecx, 104
    jb run_sieve_dense_restore_113
    btr qword [rsi + 824], 18
    cmp ecx, 106
    jb run_sieve_dense_restore_113
    btr qword [rsi + 840], 3
    cmp ecx, 107
    jb run_sieve_dense_restore_113
    btr qword [rsi + 848], 52
    cmp ecx, 109
    jb run_sieve_dense_restore_113
    btr qword [rsi + 864], 37
    cmp ecx, 111
    jb run_sieve_dense_restore_113
    btr qword [rsi + 880], 22
    cmp ecx, 113
    jb run_sieve_dense_restore_113
    btr qword [rsi + 896], 7
run_sieve_dense_restore_113:
    bts qword [r14], 56
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_115:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 115
    jb run_sieve_dense_tail_115
align 16
run_sieve_dense_loop_115:
    btr qword [rsi], 57
    btr qword [rsi + 16], 44
    btr qword [rsi + 32], 31
    btr qword [rsi + 48], 18
    btr qword [rsi + 64], 5
    btr qword [rsi + 72], 56
    btr qword [rsi + 88], 43
    btr qword [rsi + 104], 30
    btr qword [rsi + 120], 17
    btr qword [rsi + 136], 4
    btr qword [rsi + 144], 55
    btr qword [rsi + 160], 42
    btr qword [rsi + 176], 29
    btr qword [rsi + 192], 16
    btr qword [rsi + 208], 3
    btr qword [rsi + 216], 54
    btr qword [rsi + 232], 41
    btr qword [rsi + 248], 28
    btr qword [rsi + 264], 15
    btr qword [rsi + 280], 2
    btr qword [rsi + 288], 53
    btr qword [rsi + 304], 40
    btr qword [rsi + 320], 27
    btr qword [rsi + 336], 14
    btr qword [rsi + 352], 1
    btr qword [rsi + 360], 52
    btr qword [rsi + 376], 39
    btr qword [rsi + 392], 26
    btr qword [rsi + 408], 13
    btr qword [rsi + 424], 0
    btr qword [rsi + 432], 51
    btr qword [rsi + 448], 38
    btr qword [rsi + 464], 25
    btr qword [rsi + 480], 12
    btr qword [rsi + 488], 63
    btr qword [rsi + 504], 50
    btr qword [rsi + 520], 37
    btr qword [rsi + 536], 24
    btr qword [rsi + 552], 11
    btr qword [rsi + 560], 62
    btr qword [rsi + 576], 49
    btr qword [rsi + 592], 36
    btr qword [rsi + 608], 23
    btr qword [rsi + 624], 10
    btr qword [rsi + 632], 61
    btr qword [rsi + 648], 48
    btr qword [rsi + 664], 35
    btr qword [rsi + 680], 22
    btr qword [rsi + 696], 9
    btr qword [rsi + 704], 60
    btr qword [rsi + 720], 47
    btr qword [rsi + 736], 34
    btr qword [rsi + 752], 21
    btr qword [rsi + 768], 8
    btr qword [rsi + 776], 59
    btr qword [rsi + 792], 46
    btr qword [rsi + 808], 33
    btr qword [rsi + 824], 20
    btr qword [rsi + 840], 7
    btr qword [rsi + 848], 58
    btr qword [rsi + 864], 45
    btr qword [rsi + 880], 32
    btr qword [rsi + 896], 19
    btr qword [rsi + 912], 6
    add rsi, 920
    sub ecx, 115
    cmp ecx, 115
    jae run_sieve_dense_loop_115
run_sieve_dense_tail_115:
    test ecx, ecx
    jz run_sieve_dense_restore_115
    cmp ecx, 1
    jb run_sieve_dense_restore_115
    btr qword [rsi], 57
    cmp ecx, 3
    jb run_sieve_dense_restore_115
    btr qword [rsi + 16], 44
    cmp ecx, 5
    jb run_sieve_dense_restore_115
    btr qword [rsi + 32], 31
    cmp ecx, 7
    jb run_sieve_dense_restore_115
    btr qword [rsi + 48], 18
    cmp ecx, 9
    jb run_sieve_dense_restore_115
    btr qword [rsi + 64], 5
    cmp ecx, 10
    jb run_sieve_dense_restore_115
    btr qword [rsi + 72], 56
    cmp ecx, 12
    jb run_sieve_dense_restore_115
    btr qword [rsi + 88], 43
    cmp ecx, 14
    jb run_sieve_dense_restore_115
    btr qword [rsi + 104], 30
    cmp ecx, 16
    jb run_sieve_dense_restore_115
    btr qword [rsi + 120], 17
    cmp ecx, 18
    jb run_sieve_dense_restore_115
    btr qword [rsi + 136], 4
    cmp ecx, 19
    jb run_sieve_dense_restore_115
    btr qword [rsi + 144], 55
    cmp ecx, 21
    jb run_sieve_dense_restore_115
    btr qword [rsi + 160], 42
    cmp ecx, 23
    jb run_sieve_dense_restore_115
    btr qword [rsi + 176], 29
    cmp ecx, 25
    jb run_sieve_dense_restore_115
    btr qword [rsi + 192], 16
    cmp ecx, 27
    jb run_sieve_dense_restore_115
    btr qword [rsi + 208], 3
    cmp ecx, 28
    jb run_sieve_dense_restore_115
    btr qword [rsi + 216], 54
    cmp ecx, 30
    jb run_sieve_dense_restore_115
    btr qword [rsi + 232], 41
    cmp ecx, 32
    jb run_sieve_dense_restore_115
    btr qword [rsi + 248], 28
    cmp ecx, 34
    jb run_sieve_dense_restore_115
    btr qword [rsi + 264], 15
    cmp ecx, 36
    jb run_sieve_dense_restore_115
    btr qword [rsi + 280], 2
    cmp ecx, 37
    jb run_sieve_dense_restore_115
    btr qword [rsi + 288], 53
    cmp ecx, 39
    jb run_sieve_dense_restore_115
    btr qword [rsi + 304], 40
    cmp ecx, 41
    jb run_sieve_dense_restore_115
    btr qword [rsi + 320], 27
    cmp ecx, 43
    jb run_sieve_dense_restore_115
    btr qword [rsi + 336], 14
    cmp ecx, 45
    jb run_sieve_dense_restore_115
    btr qword [rsi + 352], 1
    cmp ecx, 46
    jb run_sieve_dense_restore_115
    btr qword [rsi + 360], 52
    cmp ecx, 48
    jb run_sieve_dense_restore_115
    btr qword [rsi + 376], 39
    cmp ecx, 50
    jb run_sieve_dense_restore_115
    btr qword [rsi + 392], 26
    cmp ecx, 52
    jb run_sieve_dense_restore_115
    btr qword [rsi + 408], 13
    cmp ecx, 54
    jb run_sieve_dense_restore_115
    btr qword [rsi + 424], 0
    cmp ecx, 55
    jb run_sieve_dense_restore_115
    btr qword [rsi + 432], 51
    cmp ecx, 57
    jb run_sieve_dense_restore_115
    btr qword [rsi + 448], 38
    cmp ecx, 59
    jb run_sieve_dense_restore_115
    btr qword [rsi + 464], 25
    cmp ecx, 61
    jb run_sieve_dense_restore_115
    btr qword [rsi + 480], 12
    cmp ecx, 62
    jb run_sieve_dense_restore_115
    btr qword [rsi + 488], 63
    cmp ecx, 64
    jb run_sieve_dense_restore_115
    btr qword [rsi + 504], 50
    cmp ecx, 66
    jb run_sieve_dense_restore_115
    btr qword [rsi + 520], 37
    cmp ecx, 68
    jb run_sieve_dense_restore_115
    btr qword [rsi + 536], 24
    cmp ecx, 70
    jb run_sieve_dense_restore_115
    btr qword [rsi + 552], 11
    cmp ecx, 71
    jb run_sieve_dense_restore_115
    btr qword [rsi + 560], 62
    cmp ecx, 73
    jb run_sieve_dense_restore_115
    btr qword [rsi + 576], 49
    cmp ecx, 75
    jb run_sieve_dense_restore_115
    btr qword [rsi + 592], 36
    cmp ecx, 77
    jb run_sieve_dense_restore_115
    btr qword [rsi + 608], 23
    cmp ecx, 79
    jb run_sieve_dense_restore_115
    btr qword [rsi + 624], 10
    cmp ecx, 80
    jb run_sieve_dense_restore_115
    btr qword [rsi + 632], 61
    cmp ecx, 82
    jb run_sieve_dense_restore_115
    btr qword [rsi + 648], 48
    cmp ecx, 84
    jb run_sieve_dense_restore_115
    btr qword [rsi + 664], 35
    cmp ecx, 86
    jb run_sieve_dense_restore_115
    btr qword [rsi + 680], 22
    cmp ecx, 88
    jb run_sieve_dense_restore_115
    btr qword [rsi + 696], 9
    cmp ecx, 89
    jb run_sieve_dense_restore_115
    btr qword [rsi + 704], 60
    cmp ecx, 91
    jb run_sieve_dense_restore_115
    btr qword [rsi + 720], 47
    cmp ecx, 93
    jb run_sieve_dense_restore_115
    btr qword [rsi + 736], 34
    cmp ecx, 95
    jb run_sieve_dense_restore_115
    btr qword [rsi + 752], 21
    cmp ecx, 97
    jb run_sieve_dense_restore_115
    btr qword [rsi + 768], 8
    cmp ecx, 98
    jb run_sieve_dense_restore_115
    btr qword [rsi + 776], 59
    cmp ecx, 100
    jb run_sieve_dense_restore_115
    btr qword [rsi + 792], 46
    cmp ecx, 102
    jb run_sieve_dense_restore_115
    btr qword [rsi + 808], 33
    cmp ecx, 104
    jb run_sieve_dense_restore_115
    btr qword [rsi + 824], 20
    cmp ecx, 106
    jb run_sieve_dense_restore_115
    btr qword [rsi + 840], 7
    cmp ecx, 107
    jb run_sieve_dense_restore_115
    btr qword [rsi + 848], 58
    cmp ecx, 109
    jb run_sieve_dense_restore_115
    btr qword [rsi + 864], 45
    cmp ecx, 111
    jb run_sieve_dense_restore_115
    btr qword [rsi + 880], 32
    cmp ecx, 113
    jb run_sieve_dense_restore_115
    btr qword [rsi + 896], 19
    cmp ecx, 115
    jb run_sieve_dense_restore_115
    btr qword [rsi + 912], 6
run_sieve_dense_restore_115:
    bts qword [r14], 57
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_117:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 117
    jb run_sieve_dense_tail_117
align 16
run_sieve_dense_loop_117:
    btr qword [rsi], 58
    btr qword [rsi + 16], 47
    btr qword [rsi + 32], 36
    btr qword [rsi + 48], 25
    btr qword [rsi + 64], 14
    btr qword [rsi + 80], 3
    btr qword [rsi + 88], 56
    btr qword [rsi + 104], 45
    btr qword [rsi + 120], 34
    btr qword [rsi + 136], 23
    btr qword [rsi + 152], 12
    btr qword [rsi + 168], 1
    btr qword [rsi + 176], 54
    btr qword [rsi + 192], 43
    btr qword [rsi + 208], 32
    btr qword [rsi + 224], 21
    btr qword [rsi + 240], 10
    btr qword [rsi + 248], 63
    btr qword [rsi + 264], 52
    btr qword [rsi + 280], 41
    btr qword [rsi + 296], 30
    btr qword [rsi + 312], 19
    btr qword [rsi + 328], 8
    btr qword [rsi + 336], 61
    btr qword [rsi + 352], 50
    btr qword [rsi + 368], 39
    btr qword [rsi + 384], 28
    btr qword [rsi + 400], 17
    btr qword [rsi + 416], 6
    btr qword [rsi + 424], 59
    btr qword [rsi + 440], 48
    btr qword [rsi + 456], 37
    btr qword [rsi + 472], 26
    btr qword [rsi + 488], 15
    btr qword [rsi + 504], 4
    btr qword [rsi + 512], 57
    btr qword [rsi + 528], 46
    btr qword [rsi + 544], 35
    btr qword [rsi + 560], 24
    btr qword [rsi + 576], 13
    btr qword [rsi + 592], 2
    btr qword [rsi + 600], 55
    btr qword [rsi + 616], 44
    btr qword [rsi + 632], 33
    btr qword [rsi + 648], 22
    btr qword [rsi + 664], 11
    btr qword [rsi + 680], 0
    btr qword [rsi + 688], 53
    btr qword [rsi + 704], 42
    btr qword [rsi + 720], 31
    btr qword [rsi + 736], 20
    btr qword [rsi + 752], 9
    btr qword [rsi + 760], 62
    btr qword [rsi + 776], 51
    btr qword [rsi + 792], 40
    btr qword [rsi + 808], 29
    btr qword [rsi + 824], 18
    btr qword [rsi + 840], 7
    btr qword [rsi + 848], 60
    btr qword [rsi + 864], 49
    btr qword [rsi + 880], 38
    btr qword [rsi + 896], 27
    btr qword [rsi + 912], 16
    btr qword [rsi + 928], 5
    add rsi, 936
    sub ecx, 117
    cmp ecx, 117
    jae run_sieve_dense_loop_117
run_sieve_dense_tail_117:
    test ecx, ecx
    jz run_sieve_dense_restore_117
    cmp ecx, 1
    jb run_sieve_dense_restore_117
    btr qword [rsi], 58
    cmp ecx, 3
    jb run_sieve_dense_restore_117
    btr qword [rsi + 16], 47
    cmp ecx, 5
    jb run_sieve_dense_restore_117
    btr qword [rsi + 32], 36
    cmp ecx, 7
    jb run_sieve_dense_restore_117
    btr qword [rsi + 48], 25
    cmp ecx, 9
    jb run_sieve_dense_restore_117
    btr qword [rsi + 64], 14
    cmp ecx, 11
    jb run_sieve_dense_restore_117
    btr qword [rsi + 80], 3
    cmp ecx, 12
    jb run_sieve_dense_restore_117
    btr qword [rsi + 88], 56
    cmp ecx, 14
    jb run_sieve_dense_restore_117
    btr qword [rsi + 104], 45
    cmp ecx, 16
    jb run_sieve_dense_restore_117
    btr qword [rsi + 120], 34
    cmp ecx, 18
    jb run_sieve_dense_restore_117
    btr qword [rsi + 136], 23
    cmp ecx, 20
    jb run_sieve_dense_restore_117
    btr qword [rsi + 152], 12
    cmp ecx, 22
    jb run_sieve_dense_restore_117
    btr qword [rsi + 168], 1
    cmp ecx, 23
    jb run_sieve_dense_restore_117
    btr qword [rsi + 176], 54
    cmp ecx, 25
    jb run_sieve_dense_restore_117
    btr qword [rsi + 192], 43
    cmp ecx, 27
    jb run_sieve_dense_restore_117
    btr qword [rsi + 208], 32
    cmp ecx, 29
    jb run_sieve_dense_restore_117
    btr qword [rsi + 224], 21
    cmp ecx, 31
    jb run_sieve_dense_restore_117
    btr qword [rsi + 240], 10
    cmp ecx, 32
    jb run_sieve_dense_restore_117
    btr qword [rsi + 248], 63
    cmp ecx, 34
    jb run_sieve_dense_restore_117
    btr qword [rsi + 264], 52
    cmp ecx, 36
    jb run_sieve_dense_restore_117
    btr qword [rsi + 280], 41
    cmp ecx, 38
    jb run_sieve_dense_restore_117
    btr qword [rsi + 296], 30
    cmp ecx, 40
    jb run_sieve_dense_restore_117
    btr qword [rsi + 312], 19
    cmp ecx, 42
    jb run_sieve_dense_restore_117
    btr qword [rsi + 328], 8
    cmp ecx, 43
    jb run_sieve_dense_restore_117
    btr qword [rsi + 336], 61
    cmp ecx, 45
    jb run_sieve_dense_restore_117
    btr qword [rsi + 352], 50
    cmp ecx, 47
    jb run_sieve_dense_restore_117
    btr qword [rsi + 368], 39
    cmp ecx, 49
    jb run_sieve_dense_restore_117
    btr qword [rsi + 384], 28
    cmp ecx, 51
    jb run_sieve_dense_restore_117
    btr qword [rsi + 400], 17
    cmp ecx, 53
    jb run_sieve_dense_restore_117
    btr qword [rsi + 416], 6
    cmp ecx, 54
    jb run_sieve_dense_restore_117
    btr qword [rsi + 424], 59
    cmp ecx, 56
    jb run_sieve_dense_restore_117
    btr qword [rsi + 440], 48
    cmp ecx, 58
    jb run_sieve_dense_restore_117
    btr qword [rsi + 456], 37
    cmp ecx, 60
    jb run_sieve_dense_restore_117
    btr qword [rsi + 472], 26
    cmp ecx, 62
    jb run_sieve_dense_restore_117
    btr qword [rsi + 488], 15
    cmp ecx, 64
    jb run_sieve_dense_restore_117
    btr qword [rsi + 504], 4
    cmp ecx, 65
    jb run_sieve_dense_restore_117
    btr qword [rsi + 512], 57
    cmp ecx, 67
    jb run_sieve_dense_restore_117
    btr qword [rsi + 528], 46
    cmp ecx, 69
    jb run_sieve_dense_restore_117
    btr qword [rsi + 544], 35
    cmp ecx, 71
    jb run_sieve_dense_restore_117
    btr qword [rsi + 560], 24
    cmp ecx, 73
    jb run_sieve_dense_restore_117
    btr qword [rsi + 576], 13
    cmp ecx, 75
    jb run_sieve_dense_restore_117
    btr qword [rsi + 592], 2
    cmp ecx, 76
    jb run_sieve_dense_restore_117
    btr qword [rsi + 600], 55
    cmp ecx, 78
    jb run_sieve_dense_restore_117
    btr qword [rsi + 616], 44
    cmp ecx, 80
    jb run_sieve_dense_restore_117
    btr qword [rsi + 632], 33
    cmp ecx, 82
    jb run_sieve_dense_restore_117
    btr qword [rsi + 648], 22
    cmp ecx, 84
    jb run_sieve_dense_restore_117
    btr qword [rsi + 664], 11
    cmp ecx, 86
    jb run_sieve_dense_restore_117
    btr qword [rsi + 680], 0
    cmp ecx, 87
    jb run_sieve_dense_restore_117
    btr qword [rsi + 688], 53
    cmp ecx, 89
    jb run_sieve_dense_restore_117
    btr qword [rsi + 704], 42
    cmp ecx, 91
    jb run_sieve_dense_restore_117
    btr qword [rsi + 720], 31
    cmp ecx, 93
    jb run_sieve_dense_restore_117
    btr qword [rsi + 736], 20
    cmp ecx, 95
    jb run_sieve_dense_restore_117
    btr qword [rsi + 752], 9
    cmp ecx, 96
    jb run_sieve_dense_restore_117
    btr qword [rsi + 760], 62
    cmp ecx, 98
    jb run_sieve_dense_restore_117
    btr qword [rsi + 776], 51
    cmp ecx, 100
    jb run_sieve_dense_restore_117
    btr qword [rsi + 792], 40
    cmp ecx, 102
    jb run_sieve_dense_restore_117
    btr qword [rsi + 808], 29
    cmp ecx, 104
    jb run_sieve_dense_restore_117
    btr qword [rsi + 824], 18
    cmp ecx, 106
    jb run_sieve_dense_restore_117
    btr qword [rsi + 840], 7
    cmp ecx, 107
    jb run_sieve_dense_restore_117
    btr qword [rsi + 848], 60
    cmp ecx, 109
    jb run_sieve_dense_restore_117
    btr qword [rsi + 864], 49
    cmp ecx, 111
    jb run_sieve_dense_restore_117
    btr qword [rsi + 880], 38
    cmp ecx, 113
    jb run_sieve_dense_restore_117
    btr qword [rsi + 896], 27
    cmp ecx, 115
    jb run_sieve_dense_restore_117
    btr qword [rsi + 912], 16
    cmp ecx, 117
    jb run_sieve_dense_restore_117
    btr qword [rsi + 928], 5
run_sieve_dense_restore_117:
    bts qword [r14], 58
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_119:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 119
    jb run_sieve_dense_tail_119
align 16
run_sieve_dense_loop_119:
    btr qword [rsi], 59
    btr qword [rsi + 16], 50
    btr qword [rsi + 32], 41
    btr qword [rsi + 48], 32
    btr qword [rsi + 64], 23
    btr qword [rsi + 80], 14
    btr qword [rsi + 96], 5
    btr qword [rsi + 104], 60
    btr qword [rsi + 120], 51
    btr qword [rsi + 136], 42
    btr qword [rsi + 152], 33
    btr qword [rsi + 168], 24
    btr qword [rsi + 184], 15
    btr qword [rsi + 200], 6
    btr qword [rsi + 208], 61
    btr qword [rsi + 224], 52
    btr qword [rsi + 240], 43
    btr qword [rsi + 256], 34
    btr qword [rsi + 272], 25
    btr qword [rsi + 288], 16
    btr qword [rsi + 304], 7
    btr qword [rsi + 312], 62
    btr qword [rsi + 328], 53
    btr qword [rsi + 344], 44
    btr qword [rsi + 360], 35
    btr qword [rsi + 376], 26
    btr qword [rsi + 392], 17
    btr qword [rsi + 408], 8
    btr qword [rsi + 416], 63
    btr qword [rsi + 432], 54
    btr qword [rsi + 448], 45
    btr qword [rsi + 464], 36
    btr qword [rsi + 480], 27
    btr qword [rsi + 496], 18
    btr qword [rsi + 512], 9
    btr qword [rsi + 528], 0
    btr qword [rsi + 536], 55
    btr qword [rsi + 552], 46
    btr qword [rsi + 568], 37
    btr qword [rsi + 584], 28
    btr qword [rsi + 600], 19
    btr qword [rsi + 616], 10
    btr qword [rsi + 632], 1
    btr qword [rsi + 640], 56
    btr qword [rsi + 656], 47
    btr qword [rsi + 672], 38
    btr qword [rsi + 688], 29
    btr qword [rsi + 704], 20
    btr qword [rsi + 720], 11
    btr qword [rsi + 736], 2
    btr qword [rsi + 744], 57
    btr qword [rsi + 760], 48
    btr qword [rsi + 776], 39
    btr qword [rsi + 792], 30
    btr qword [rsi + 808], 21
    btr qword [rsi + 824], 12
    btr qword [rsi + 840], 3
    btr qword [rsi + 848], 58
    btr qword [rsi + 864], 49
    btr qword [rsi + 880], 40
    btr qword [rsi + 896], 31
    btr qword [rsi + 912], 22
    btr qword [rsi + 928], 13
    btr qword [rsi + 944], 4
    add rsi, 952
    sub ecx, 119
    cmp ecx, 119
    jae run_sieve_dense_loop_119
run_sieve_dense_tail_119:
    test ecx, ecx
    jz run_sieve_dense_restore_119
    cmp ecx, 1
    jb run_sieve_dense_restore_119
    btr qword [rsi], 59
    cmp ecx, 3
    jb run_sieve_dense_restore_119
    btr qword [rsi + 16], 50
    cmp ecx, 5
    jb run_sieve_dense_restore_119
    btr qword [rsi + 32], 41
    cmp ecx, 7
    jb run_sieve_dense_restore_119
    btr qword [rsi + 48], 32
    cmp ecx, 9
    jb run_sieve_dense_restore_119
    btr qword [rsi + 64], 23
    cmp ecx, 11
    jb run_sieve_dense_restore_119
    btr qword [rsi + 80], 14
    cmp ecx, 13
    jb run_sieve_dense_restore_119
    btr qword [rsi + 96], 5
    cmp ecx, 14
    jb run_sieve_dense_restore_119
    btr qword [rsi + 104], 60
    cmp ecx, 16
    jb run_sieve_dense_restore_119
    btr qword [rsi + 120], 51
    cmp ecx, 18
    jb run_sieve_dense_restore_119
    btr qword [rsi + 136], 42
    cmp ecx, 20
    jb run_sieve_dense_restore_119
    btr qword [rsi + 152], 33
    cmp ecx, 22
    jb run_sieve_dense_restore_119
    btr qword [rsi + 168], 24
    cmp ecx, 24
    jb run_sieve_dense_restore_119
    btr qword [rsi + 184], 15
    cmp ecx, 26
    jb run_sieve_dense_restore_119
    btr qword [rsi + 200], 6
    cmp ecx, 27
    jb run_sieve_dense_restore_119
    btr qword [rsi + 208], 61
    cmp ecx, 29
    jb run_sieve_dense_restore_119
    btr qword [rsi + 224], 52
    cmp ecx, 31
    jb run_sieve_dense_restore_119
    btr qword [rsi + 240], 43
    cmp ecx, 33
    jb run_sieve_dense_restore_119
    btr qword [rsi + 256], 34
    cmp ecx, 35
    jb run_sieve_dense_restore_119
    btr qword [rsi + 272], 25
    cmp ecx, 37
    jb run_sieve_dense_restore_119
    btr qword [rsi + 288], 16
    cmp ecx, 39
    jb run_sieve_dense_restore_119
    btr qword [rsi + 304], 7
    cmp ecx, 40
    jb run_sieve_dense_restore_119
    btr qword [rsi + 312], 62
    cmp ecx, 42
    jb run_sieve_dense_restore_119
    btr qword [rsi + 328], 53
    cmp ecx, 44
    jb run_sieve_dense_restore_119
    btr qword [rsi + 344], 44
    cmp ecx, 46
    jb run_sieve_dense_restore_119
    btr qword [rsi + 360], 35
    cmp ecx, 48
    jb run_sieve_dense_restore_119
    btr qword [rsi + 376], 26
    cmp ecx, 50
    jb run_sieve_dense_restore_119
    btr qword [rsi + 392], 17
    cmp ecx, 52
    jb run_sieve_dense_restore_119
    btr qword [rsi + 408], 8
    cmp ecx, 53
    jb run_sieve_dense_restore_119
    btr qword [rsi + 416], 63
    cmp ecx, 55
    jb run_sieve_dense_restore_119
    btr qword [rsi + 432], 54
    cmp ecx, 57
    jb run_sieve_dense_restore_119
    btr qword [rsi + 448], 45
    cmp ecx, 59
    jb run_sieve_dense_restore_119
    btr qword [rsi + 464], 36
    cmp ecx, 61
    jb run_sieve_dense_restore_119
    btr qword [rsi + 480], 27
    cmp ecx, 63
    jb run_sieve_dense_restore_119
    btr qword [rsi + 496], 18
    cmp ecx, 65
    jb run_sieve_dense_restore_119
    btr qword [rsi + 512], 9
    cmp ecx, 67
    jb run_sieve_dense_restore_119
    btr qword [rsi + 528], 0
    cmp ecx, 68
    jb run_sieve_dense_restore_119
    btr qword [rsi + 536], 55
    cmp ecx, 70
    jb run_sieve_dense_restore_119
    btr qword [rsi + 552], 46
    cmp ecx, 72
    jb run_sieve_dense_restore_119
    btr qword [rsi + 568], 37
    cmp ecx, 74
    jb run_sieve_dense_restore_119
    btr qword [rsi + 584], 28
    cmp ecx, 76
    jb run_sieve_dense_restore_119
    btr qword [rsi + 600], 19
    cmp ecx, 78
    jb run_sieve_dense_restore_119
    btr qword [rsi + 616], 10
    cmp ecx, 80
    jb run_sieve_dense_restore_119
    btr qword [rsi + 632], 1
    cmp ecx, 81
    jb run_sieve_dense_restore_119
    btr qword [rsi + 640], 56
    cmp ecx, 83
    jb run_sieve_dense_restore_119
    btr qword [rsi + 656], 47
    cmp ecx, 85
    jb run_sieve_dense_restore_119
    btr qword [rsi + 672], 38
    cmp ecx, 87
    jb run_sieve_dense_restore_119
    btr qword [rsi + 688], 29
    cmp ecx, 89
    jb run_sieve_dense_restore_119
    btr qword [rsi + 704], 20
    cmp ecx, 91
    jb run_sieve_dense_restore_119
    btr qword [rsi + 720], 11
    cmp ecx, 93
    jb run_sieve_dense_restore_119
    btr qword [rsi + 736], 2
    cmp ecx, 94
    jb run_sieve_dense_restore_119
    btr qword [rsi + 744], 57
    cmp ecx, 96
    jb run_sieve_dense_restore_119
    btr qword [rsi + 760], 48
    cmp ecx, 98
    jb run_sieve_dense_restore_119
    btr qword [rsi + 776], 39
    cmp ecx, 100
    jb run_sieve_dense_restore_119
    btr qword [rsi + 792], 30
    cmp ecx, 102
    jb run_sieve_dense_restore_119
    btr qword [rsi + 808], 21
    cmp ecx, 104
    jb run_sieve_dense_restore_119
    btr qword [rsi + 824], 12
    cmp ecx, 106
    jb run_sieve_dense_restore_119
    btr qword [rsi + 840], 3
    cmp ecx, 107
    jb run_sieve_dense_restore_119
    btr qword [rsi + 848], 58
    cmp ecx, 109
    jb run_sieve_dense_restore_119
    btr qword [rsi + 864], 49
    cmp ecx, 111
    jb run_sieve_dense_restore_119
    btr qword [rsi + 880], 40
    cmp ecx, 113
    jb run_sieve_dense_restore_119
    btr qword [rsi + 896], 31
    cmp ecx, 115
    jb run_sieve_dense_restore_119
    btr qword [rsi + 912], 22
    cmp ecx, 117
    jb run_sieve_dense_restore_119
    btr qword [rsi + 928], 13
    cmp ecx, 119
    jb run_sieve_dense_restore_119
    btr qword [rsi + 944], 4
run_sieve_dense_restore_119:
    bts qword [r14], 59
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_121:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 121
    jb run_sieve_dense_tail_121
align 16
run_sieve_dense_loop_121:
    btr qword [rsi], 60
    btr qword [rsi + 16], 53
    btr qword [rsi + 32], 46
    btr qword [rsi + 48], 39
    btr qword [rsi + 64], 32
    btr qword [rsi + 80], 25
    btr qword [rsi + 96], 18
    btr qword [rsi + 112], 11
    btr qword [rsi + 128], 4
    btr qword [rsi + 136], 61
    btr qword [rsi + 152], 54
    btr qword [rsi + 168], 47
    btr qword [rsi + 184], 40
    btr qword [rsi + 200], 33
    btr qword [rsi + 216], 26
    btr qword [rsi + 232], 19
    btr qword [rsi + 248], 12
    btr qword [rsi + 264], 5
    btr qword [rsi + 272], 62
    btr qword [rsi + 288], 55
    btr qword [rsi + 304], 48
    btr qword [rsi + 320], 41
    btr qword [rsi + 336], 34
    btr qword [rsi + 352], 27
    btr qword [rsi + 368], 20
    btr qword [rsi + 384], 13
    btr qword [rsi + 400], 6
    btr qword [rsi + 408], 63
    btr qword [rsi + 424], 56
    btr qword [rsi + 440], 49
    btr qword [rsi + 456], 42
    btr qword [rsi + 472], 35
    btr qword [rsi + 488], 28
    btr qword [rsi + 504], 21
    btr qword [rsi + 520], 14
    btr qword [rsi + 536], 7
    btr qword [rsi + 552], 0
    btr qword [rsi + 560], 57
    btr qword [rsi + 576], 50
    btr qword [rsi + 592], 43
    btr qword [rsi + 608], 36
    btr qword [rsi + 624], 29
    btr qword [rsi + 640], 22
    btr qword [rsi + 656], 15
    btr qword [rsi + 672], 8
    btr qword [rsi + 688], 1
    btr qword [rsi + 696], 58
    btr qword [rsi + 712], 51
    btr qword [rsi + 728], 44
    btr qword [rsi + 744], 37
    btr qword [rsi + 760], 30
    btr qword [rsi + 776], 23
    btr qword [rsi + 792], 16
    btr qword [rsi + 808], 9
    btr qword [rsi + 824], 2
    btr qword [rsi + 832], 59
    btr qword [rsi + 848], 52
    btr qword [rsi + 864], 45
    btr qword [rsi + 880], 38
    btr qword [rsi + 896], 31
    btr qword [rsi + 912], 24
    btr qword [rsi + 928], 17
    btr qword [rsi + 944], 10
    btr qword [rsi + 960], 3
    add rsi, 968
    sub ecx, 121
    cmp ecx, 121
    jae run_sieve_dense_loop_121
run_sieve_dense_tail_121:
    test ecx, ecx
    jz run_sieve_dense_restore_121
    cmp ecx, 1
    jb run_sieve_dense_restore_121
    btr qword [rsi], 60
    cmp ecx, 3
    jb run_sieve_dense_restore_121
    btr qword [rsi + 16], 53
    cmp ecx, 5
    jb run_sieve_dense_restore_121
    btr qword [rsi + 32], 46
    cmp ecx, 7
    jb run_sieve_dense_restore_121
    btr qword [rsi + 48], 39
    cmp ecx, 9
    jb run_sieve_dense_restore_121
    btr qword [rsi + 64], 32
    cmp ecx, 11
    jb run_sieve_dense_restore_121
    btr qword [rsi + 80], 25
    cmp ecx, 13
    jb run_sieve_dense_restore_121
    btr qword [rsi + 96], 18
    cmp ecx, 15
    jb run_sieve_dense_restore_121
    btr qword [rsi + 112], 11
    cmp ecx, 17
    jb run_sieve_dense_restore_121
    btr qword [rsi + 128], 4
    cmp ecx, 18
    jb run_sieve_dense_restore_121
    btr qword [rsi + 136], 61
    cmp ecx, 20
    jb run_sieve_dense_restore_121
    btr qword [rsi + 152], 54
    cmp ecx, 22
    jb run_sieve_dense_restore_121
    btr qword [rsi + 168], 47
    cmp ecx, 24
    jb run_sieve_dense_restore_121
    btr qword [rsi + 184], 40
    cmp ecx, 26
    jb run_sieve_dense_restore_121
    btr qword [rsi + 200], 33
    cmp ecx, 28
    jb run_sieve_dense_restore_121
    btr qword [rsi + 216], 26
    cmp ecx, 30
    jb run_sieve_dense_restore_121
    btr qword [rsi + 232], 19
    cmp ecx, 32
    jb run_sieve_dense_restore_121
    btr qword [rsi + 248], 12
    cmp ecx, 34
    jb run_sieve_dense_restore_121
    btr qword [rsi + 264], 5
    cmp ecx, 35
    jb run_sieve_dense_restore_121
    btr qword [rsi + 272], 62
    cmp ecx, 37
    jb run_sieve_dense_restore_121
    btr qword [rsi + 288], 55
    cmp ecx, 39
    jb run_sieve_dense_restore_121
    btr qword [rsi + 304], 48
    cmp ecx, 41
    jb run_sieve_dense_restore_121
    btr qword [rsi + 320], 41
    cmp ecx, 43
    jb run_sieve_dense_restore_121
    btr qword [rsi + 336], 34
    cmp ecx, 45
    jb run_sieve_dense_restore_121
    btr qword [rsi + 352], 27
    cmp ecx, 47
    jb run_sieve_dense_restore_121
    btr qword [rsi + 368], 20
    cmp ecx, 49
    jb run_sieve_dense_restore_121
    btr qword [rsi + 384], 13
    cmp ecx, 51
    jb run_sieve_dense_restore_121
    btr qword [rsi + 400], 6
    cmp ecx, 52
    jb run_sieve_dense_restore_121
    btr qword [rsi + 408], 63
    cmp ecx, 54
    jb run_sieve_dense_restore_121
    btr qword [rsi + 424], 56
    cmp ecx, 56
    jb run_sieve_dense_restore_121
    btr qword [rsi + 440], 49
    cmp ecx, 58
    jb run_sieve_dense_restore_121
    btr qword [rsi + 456], 42
    cmp ecx, 60
    jb run_sieve_dense_restore_121
    btr qword [rsi + 472], 35
    cmp ecx, 62
    jb run_sieve_dense_restore_121
    btr qword [rsi + 488], 28
    cmp ecx, 64
    jb run_sieve_dense_restore_121
    btr qword [rsi + 504], 21
    cmp ecx, 66
    jb run_sieve_dense_restore_121
    btr qword [rsi + 520], 14
    cmp ecx, 68
    jb run_sieve_dense_restore_121
    btr qword [rsi + 536], 7
    cmp ecx, 70
    jb run_sieve_dense_restore_121
    btr qword [rsi + 552], 0
    cmp ecx, 71
    jb run_sieve_dense_restore_121
    btr qword [rsi + 560], 57
    cmp ecx, 73
    jb run_sieve_dense_restore_121
    btr qword [rsi + 576], 50
    cmp ecx, 75
    jb run_sieve_dense_restore_121
    btr qword [rsi + 592], 43
    cmp ecx, 77
    jb run_sieve_dense_restore_121
    btr qword [rsi + 608], 36
    cmp ecx, 79
    jb run_sieve_dense_restore_121
    btr qword [rsi + 624], 29
    cmp ecx, 81
    jb run_sieve_dense_restore_121
    btr qword [rsi + 640], 22
    cmp ecx, 83
    jb run_sieve_dense_restore_121
    btr qword [rsi + 656], 15
    cmp ecx, 85
    jb run_sieve_dense_restore_121
    btr qword [rsi + 672], 8
    cmp ecx, 87
    jb run_sieve_dense_restore_121
    btr qword [rsi + 688], 1
    cmp ecx, 88
    jb run_sieve_dense_restore_121
    btr qword [rsi + 696], 58
    cmp ecx, 90
    jb run_sieve_dense_restore_121
    btr qword [rsi + 712], 51
    cmp ecx, 92
    jb run_sieve_dense_restore_121
    btr qword [rsi + 728], 44
    cmp ecx, 94
    jb run_sieve_dense_restore_121
    btr qword [rsi + 744], 37
    cmp ecx, 96
    jb run_sieve_dense_restore_121
    btr qword [rsi + 760], 30
    cmp ecx, 98
    jb run_sieve_dense_restore_121
    btr qword [rsi + 776], 23
    cmp ecx, 100
    jb run_sieve_dense_restore_121
    btr qword [rsi + 792], 16
    cmp ecx, 102
    jb run_sieve_dense_restore_121
    btr qword [rsi + 808], 9
    cmp ecx, 104
    jb run_sieve_dense_restore_121
    btr qword [rsi + 824], 2
    cmp ecx, 105
    jb run_sieve_dense_restore_121
    btr qword [rsi + 832], 59
    cmp ecx, 107
    jb run_sieve_dense_restore_121
    btr qword [rsi + 848], 52
    cmp ecx, 109
    jb run_sieve_dense_restore_121
    btr qword [rsi + 864], 45
    cmp ecx, 111
    jb run_sieve_dense_restore_121
    btr qword [rsi + 880], 38
    cmp ecx, 113
    jb run_sieve_dense_restore_121
    btr qword [rsi + 896], 31
    cmp ecx, 115
    jb run_sieve_dense_restore_121
    btr qword [rsi + 912], 24
    cmp ecx, 117
    jb run_sieve_dense_restore_121
    btr qword [rsi + 928], 17
    cmp ecx, 119
    jb run_sieve_dense_restore_121
    btr qword [rsi + 944], 10
    cmp ecx, 121
    jb run_sieve_dense_restore_121
    btr qword [rsi + 960], 3
run_sieve_dense_restore_121:
    bts qword [r14], 60
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_123:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 123
    jb run_sieve_dense_tail_123
align 16
run_sieve_dense_loop_123:
    btr qword [rsi], 61
    btr qword [rsi + 16], 56
    btr qword [rsi + 32], 51
    btr qword [rsi + 48], 46
    btr qword [rsi + 64], 41
    btr qword [rsi + 80], 36
    btr qword [rsi + 96], 31
    btr qword [rsi + 112], 26
    btr qword [rsi + 128], 21
    btr qword [rsi + 144], 16
    btr qword [rsi + 160], 11
    btr qword [rsi + 176], 6
    btr qword [rsi + 192], 1
    btr qword [rsi + 200], 60
    btr qword [rsi + 216], 55
    btr qword [rsi + 232], 50
    btr qword [rsi + 248], 45
    btr qword [rsi + 264], 40
    btr qword [rsi + 280], 35
    btr qword [rsi + 296], 30
    btr qword [rsi + 312], 25
    btr qword [rsi + 328], 20
    btr qword [rsi + 344], 15
    btr qword [rsi + 360], 10
    btr qword [rsi + 376], 5
    btr qword [rsi + 392], 0
    btr qword [rsi + 400], 59
    btr qword [rsi + 416], 54
    btr qword [rsi + 432], 49
    btr qword [rsi + 448], 44
    btr qword [rsi + 464], 39
    btr qword [rsi + 480], 34
    btr qword [rsi + 496], 29
    btr qword [rsi + 512], 24
    btr qword [rsi + 528], 19
    btr qword [rsi + 544], 14
    btr qword [rsi + 560], 9
    btr qword [rsi + 576], 4
    btr qword [rsi + 584], 63
    btr qword [rsi + 600], 58
    btr qword [rsi + 616], 53
    btr qword [rsi + 632], 48
    btr qword [rsi + 648], 43
    btr qword [rsi + 664], 38
    btr qword [rsi + 680], 33
    btr qword [rsi + 696], 28
    btr qword [rsi + 712], 23
    btr qword [rsi + 728], 18
    btr qword [rsi + 744], 13
    btr qword [rsi + 760], 8
    btr qword [rsi + 776], 3
    btr qword [rsi + 784], 62
    btr qword [rsi + 800], 57
    btr qword [rsi + 816], 52
    btr qword [rsi + 832], 47
    btr qword [rsi + 848], 42
    btr qword [rsi + 864], 37
    btr qword [rsi + 880], 32
    btr qword [rsi + 896], 27
    btr qword [rsi + 912], 22
    btr qword [rsi + 928], 17
    btr qword [rsi + 944], 12
    btr qword [rsi + 960], 7
    btr qword [rsi + 976], 2
    add rsi, 984
    sub ecx, 123
    cmp ecx, 123
    jae run_sieve_dense_loop_123
run_sieve_dense_tail_123:
    test ecx, ecx
    jz run_sieve_dense_restore_123
    cmp ecx, 1
    jb run_sieve_dense_restore_123
    btr qword [rsi], 61
    cmp ecx, 3
    jb run_sieve_dense_restore_123
    btr qword [rsi + 16], 56
    cmp ecx, 5
    jb run_sieve_dense_restore_123
    btr qword [rsi + 32], 51
    cmp ecx, 7
    jb run_sieve_dense_restore_123
    btr qword [rsi + 48], 46
    cmp ecx, 9
    jb run_sieve_dense_restore_123
    btr qword [rsi + 64], 41
    cmp ecx, 11
    jb run_sieve_dense_restore_123
    btr qword [rsi + 80], 36
    cmp ecx, 13
    jb run_sieve_dense_restore_123
    btr qword [rsi + 96], 31
    cmp ecx, 15
    jb run_sieve_dense_restore_123
    btr qword [rsi + 112], 26
    cmp ecx, 17
    jb run_sieve_dense_restore_123
    btr qword [rsi + 128], 21
    cmp ecx, 19
    jb run_sieve_dense_restore_123
    btr qword [rsi + 144], 16
    cmp ecx, 21
    jb run_sieve_dense_restore_123
    btr qword [rsi + 160], 11
    cmp ecx, 23
    jb run_sieve_dense_restore_123
    btr qword [rsi + 176], 6
    cmp ecx, 25
    jb run_sieve_dense_restore_123
    btr qword [rsi + 192], 1
    cmp ecx, 26
    jb run_sieve_dense_restore_123
    btr qword [rsi + 200], 60
    cmp ecx, 28
    jb run_sieve_dense_restore_123
    btr qword [rsi + 216], 55
    cmp ecx, 30
    jb run_sieve_dense_restore_123
    btr qword [rsi + 232], 50
    cmp ecx, 32
    jb run_sieve_dense_restore_123
    btr qword [rsi + 248], 45
    cmp ecx, 34
    jb run_sieve_dense_restore_123
    btr qword [rsi + 264], 40
    cmp ecx, 36
    jb run_sieve_dense_restore_123
    btr qword [rsi + 280], 35
    cmp ecx, 38
    jb run_sieve_dense_restore_123
    btr qword [rsi + 296], 30
    cmp ecx, 40
    jb run_sieve_dense_restore_123
    btr qword [rsi + 312], 25
    cmp ecx, 42
    jb run_sieve_dense_restore_123
    btr qword [rsi + 328], 20
    cmp ecx, 44
    jb run_sieve_dense_restore_123
    btr qword [rsi + 344], 15
    cmp ecx, 46
    jb run_sieve_dense_restore_123
    btr qword [rsi + 360], 10
    cmp ecx, 48
    jb run_sieve_dense_restore_123
    btr qword [rsi + 376], 5
    cmp ecx, 50
    jb run_sieve_dense_restore_123
    btr qword [rsi + 392], 0
    cmp ecx, 51
    jb run_sieve_dense_restore_123
    btr qword [rsi + 400], 59
    cmp ecx, 53
    jb run_sieve_dense_restore_123
    btr qword [rsi + 416], 54
    cmp ecx, 55
    jb run_sieve_dense_restore_123
    btr qword [rsi + 432], 49
    cmp ecx, 57
    jb run_sieve_dense_restore_123
    btr qword [rsi + 448], 44
    cmp ecx, 59
    jb run_sieve_dense_restore_123
    btr qword [rsi + 464], 39
    cmp ecx, 61
    jb run_sieve_dense_restore_123
    btr qword [rsi + 480], 34
    cmp ecx, 63
    jb run_sieve_dense_restore_123
    btr qword [rsi + 496], 29
    cmp ecx, 65
    jb run_sieve_dense_restore_123
    btr qword [rsi + 512], 24
    cmp ecx, 67
    jb run_sieve_dense_restore_123
    btr qword [rsi + 528], 19
    cmp ecx, 69
    jb run_sieve_dense_restore_123
    btr qword [rsi + 544], 14
    cmp ecx, 71
    jb run_sieve_dense_restore_123
    btr qword [rsi + 560], 9
    cmp ecx, 73
    jb run_sieve_dense_restore_123
    btr qword [rsi + 576], 4
    cmp ecx, 74
    jb run_sieve_dense_restore_123
    btr qword [rsi + 584], 63
    cmp ecx, 76
    jb run_sieve_dense_restore_123
    btr qword [rsi + 600], 58
    cmp ecx, 78
    jb run_sieve_dense_restore_123
    btr qword [rsi + 616], 53
    cmp ecx, 80
    jb run_sieve_dense_restore_123
    btr qword [rsi + 632], 48
    cmp ecx, 82
    jb run_sieve_dense_restore_123
    btr qword [rsi + 648], 43
    cmp ecx, 84
    jb run_sieve_dense_restore_123
    btr qword [rsi + 664], 38
    cmp ecx, 86
    jb run_sieve_dense_restore_123
    btr qword [rsi + 680], 33
    cmp ecx, 88
    jb run_sieve_dense_restore_123
    btr qword [rsi + 696], 28
    cmp ecx, 90
    jb run_sieve_dense_restore_123
    btr qword [rsi + 712], 23
    cmp ecx, 92
    jb run_sieve_dense_restore_123
    btr qword [rsi + 728], 18
    cmp ecx, 94
    jb run_sieve_dense_restore_123
    btr qword [rsi + 744], 13
    cmp ecx, 96
    jb run_sieve_dense_restore_123
    btr qword [rsi + 760], 8
    cmp ecx, 98
    jb run_sieve_dense_restore_123
    btr qword [rsi + 776], 3
    cmp ecx, 99
    jb run_sieve_dense_restore_123
    btr qword [rsi + 784], 62
    cmp ecx, 101
    jb run_sieve_dense_restore_123
    btr qword [rsi + 800], 57
    cmp ecx, 103
    jb run_sieve_dense_restore_123
    btr qword [rsi + 816], 52
    cmp ecx, 105
    jb run_sieve_dense_restore_123
    btr qword [rsi + 832], 47
    cmp ecx, 107
    jb run_sieve_dense_restore_123
    btr qword [rsi + 848], 42
    cmp ecx, 109
    jb run_sieve_dense_restore_123
    btr qword [rsi + 864], 37
    cmp ecx, 111
    jb run_sieve_dense_restore_123
    btr qword [rsi + 880], 32
    cmp ecx, 113
    jb run_sieve_dense_restore_123
    btr qword [rsi + 896], 27
    cmp ecx, 115
    jb run_sieve_dense_restore_123
    btr qword [rsi + 912], 22
    cmp ecx, 117
    jb run_sieve_dense_restore_123
    btr qword [rsi + 928], 17
    cmp ecx, 119
    jb run_sieve_dense_restore_123
    btr qword [rsi + 944], 12
    cmp ecx, 121
    jb run_sieve_dense_restore_123
    btr qword [rsi + 960], 7
    cmp ecx, 123
    jb run_sieve_dense_restore_123
    btr qword [rsi + 976], 2
run_sieve_dense_restore_123:
    bts qword [r14], 61
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_125:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 125
    jb run_sieve_dense_tail_125
align 16
run_sieve_dense_loop_125:
    btr qword [rsi], 62
    btr qword [rsi + 16], 59
    btr qword [rsi + 32], 56
    btr qword [rsi + 48], 53
    btr qword [rsi + 64], 50
    btr qword [rsi + 80], 47
    btr qword [rsi + 96], 44
    btr qword [rsi + 112], 41
    btr qword [rsi + 128], 38
    btr qword [rsi + 144], 35
    btr qword [rsi + 160], 32
    btr qword [rsi + 176], 29
    btr qword [rsi + 192], 26
    btr qword [rsi + 208], 23
    btr qword [rsi + 224], 20
    btr qword [rsi + 240], 17
    btr qword [rsi + 256], 14
    btr qword [rsi + 272], 11
    btr qword [rsi + 288], 8
    btr qword [rsi + 304], 5
    btr qword [rsi + 320], 2
    btr qword [rsi + 328], 63
    btr qword [rsi + 344], 60
    btr qword [rsi + 360], 57
    btr qword [rsi + 376], 54
    btr qword [rsi + 392], 51
    btr qword [rsi + 408], 48
    btr qword [rsi + 424], 45
    btr qword [rsi + 440], 42
    btr qword [rsi + 456], 39
    btr qword [rsi + 472], 36
    btr qword [rsi + 488], 33
    btr qword [rsi + 504], 30
    btr qword [rsi + 520], 27
    btr qword [rsi + 536], 24
    btr qword [rsi + 552], 21
    btr qword [rsi + 568], 18
    btr qword [rsi + 584], 15
    btr qword [rsi + 600], 12
    btr qword [rsi + 616], 9
    btr qword [rsi + 632], 6
    btr qword [rsi + 648], 3
    btr qword [rsi + 664], 0
    btr qword [rsi + 672], 61
    btr qword [rsi + 688], 58
    btr qword [rsi + 704], 55
    btr qword [rsi + 720], 52
    btr qword [rsi + 736], 49
    btr qword [rsi + 752], 46
    btr qword [rsi + 768], 43
    btr qword [rsi + 784], 40
    btr qword [rsi + 800], 37
    btr qword [rsi + 816], 34
    btr qword [rsi + 832], 31
    btr qword [rsi + 848], 28
    btr qword [rsi + 864], 25
    btr qword [rsi + 880], 22
    btr qword [rsi + 896], 19
    btr qword [rsi + 912], 16
    btr qword [rsi + 928], 13
    btr qword [rsi + 944], 10
    btr qword [rsi + 960], 7
    btr qword [rsi + 976], 4
    btr qword [rsi + 992], 1
    add rsi, 1000
    sub ecx, 125
    cmp ecx, 125
    jae run_sieve_dense_loop_125
run_sieve_dense_tail_125:
    test ecx, ecx
    jz run_sieve_dense_restore_125
    cmp ecx, 1
    jb run_sieve_dense_restore_125
    btr qword [rsi], 62
    cmp ecx, 3
    jb run_sieve_dense_restore_125
    btr qword [rsi + 16], 59
    cmp ecx, 5
    jb run_sieve_dense_restore_125
    btr qword [rsi + 32], 56
    cmp ecx, 7
    jb run_sieve_dense_restore_125
    btr qword [rsi + 48], 53
    cmp ecx, 9
    jb run_sieve_dense_restore_125
    btr qword [rsi + 64], 50
    cmp ecx, 11
    jb run_sieve_dense_restore_125
    btr qword [rsi + 80], 47
    cmp ecx, 13
    jb run_sieve_dense_restore_125
    btr qword [rsi + 96], 44
    cmp ecx, 15
    jb run_sieve_dense_restore_125
    btr qword [rsi + 112], 41
    cmp ecx, 17
    jb run_sieve_dense_restore_125
    btr qword [rsi + 128], 38
    cmp ecx, 19
    jb run_sieve_dense_restore_125
    btr qword [rsi + 144], 35
    cmp ecx, 21
    jb run_sieve_dense_restore_125
    btr qword [rsi + 160], 32
    cmp ecx, 23
    jb run_sieve_dense_restore_125
    btr qword [rsi + 176], 29
    cmp ecx, 25
    jb run_sieve_dense_restore_125
    btr qword [rsi + 192], 26
    cmp ecx, 27
    jb run_sieve_dense_restore_125
    btr qword [rsi + 208], 23
    cmp ecx, 29
    jb run_sieve_dense_restore_125
    btr qword [rsi + 224], 20
    cmp ecx, 31
    jb run_sieve_dense_restore_125
    btr qword [rsi + 240], 17
    cmp ecx, 33
    jb run_sieve_dense_restore_125
    btr qword [rsi + 256], 14
    cmp ecx, 35
    jb run_sieve_dense_restore_125
    btr qword [rsi + 272], 11
    cmp ecx, 37
    jb run_sieve_dense_restore_125
    btr qword [rsi + 288], 8
    cmp ecx, 39
    jb run_sieve_dense_restore_125
    btr qword [rsi + 304], 5
    cmp ecx, 41
    jb run_sieve_dense_restore_125
    btr qword [rsi + 320], 2
    cmp ecx, 42
    jb run_sieve_dense_restore_125
    btr qword [rsi + 328], 63
    cmp ecx, 44
    jb run_sieve_dense_restore_125
    btr qword [rsi + 344], 60
    cmp ecx, 46
    jb run_sieve_dense_restore_125
    btr qword [rsi + 360], 57
    cmp ecx, 48
    jb run_sieve_dense_restore_125
    btr qword [rsi + 376], 54
    cmp ecx, 50
    jb run_sieve_dense_restore_125
    btr qword [rsi + 392], 51
    cmp ecx, 52
    jb run_sieve_dense_restore_125
    btr qword [rsi + 408], 48
    cmp ecx, 54
    jb run_sieve_dense_restore_125
    btr qword [rsi + 424], 45
    cmp ecx, 56
    jb run_sieve_dense_restore_125
    btr qword [rsi + 440], 42
    cmp ecx, 58
    jb run_sieve_dense_restore_125
    btr qword [rsi + 456], 39
    cmp ecx, 60
    jb run_sieve_dense_restore_125
    btr qword [rsi + 472], 36
    cmp ecx, 62
    jb run_sieve_dense_restore_125
    btr qword [rsi + 488], 33
    cmp ecx, 64
    jb run_sieve_dense_restore_125
    btr qword [rsi + 504], 30
    cmp ecx, 66
    jb run_sieve_dense_restore_125
    btr qword [rsi + 520], 27
    cmp ecx, 68
    jb run_sieve_dense_restore_125
    btr qword [rsi + 536], 24
    cmp ecx, 70
    jb run_sieve_dense_restore_125
    btr qword [rsi + 552], 21
    cmp ecx, 72
    jb run_sieve_dense_restore_125
    btr qword [rsi + 568], 18
    cmp ecx, 74
    jb run_sieve_dense_restore_125
    btr qword [rsi + 584], 15
    cmp ecx, 76
    jb run_sieve_dense_restore_125
    btr qword [rsi + 600], 12
    cmp ecx, 78
    jb run_sieve_dense_restore_125
    btr qword [rsi + 616], 9
    cmp ecx, 80
    jb run_sieve_dense_restore_125
    btr qword [rsi + 632], 6
    cmp ecx, 82
    jb run_sieve_dense_restore_125
    btr qword [rsi + 648], 3
    cmp ecx, 84
    jb run_sieve_dense_restore_125
    btr qword [rsi + 664], 0
    cmp ecx, 85
    jb run_sieve_dense_restore_125
    btr qword [rsi + 672], 61
    cmp ecx, 87
    jb run_sieve_dense_restore_125
    btr qword [rsi + 688], 58
    cmp ecx, 89
    jb run_sieve_dense_restore_125
    btr qword [rsi + 704], 55
    cmp ecx, 91
    jb run_sieve_dense_restore_125
    btr qword [rsi + 720], 52
    cmp ecx, 93
    jb run_sieve_dense_restore_125
    btr qword [rsi + 736], 49
    cmp ecx, 95
    jb run_sieve_dense_restore_125
    btr qword [rsi + 752], 46
    cmp ecx, 97
    jb run_sieve_dense_restore_125
    btr qword [rsi + 768], 43
    cmp ecx, 99
    jb run_sieve_dense_restore_125
    btr qword [rsi + 784], 40
    cmp ecx, 101
    jb run_sieve_dense_restore_125
    btr qword [rsi + 800], 37
    cmp ecx, 103
    jb run_sieve_dense_restore_125
    btr qword [rsi + 816], 34
    cmp ecx, 105
    jb run_sieve_dense_restore_125
    btr qword [rsi + 832], 31
    cmp ecx, 107
    jb run_sieve_dense_restore_125
    btr qword [rsi + 848], 28
    cmp ecx, 109
    jb run_sieve_dense_restore_125
    btr qword [rsi + 864], 25
    cmp ecx, 111
    jb run_sieve_dense_restore_125
    btr qword [rsi + 880], 22
    cmp ecx, 113
    jb run_sieve_dense_restore_125
    btr qword [rsi + 896], 19
    cmp ecx, 115
    jb run_sieve_dense_restore_125
    btr qword [rsi + 912], 16
    cmp ecx, 117
    jb run_sieve_dense_restore_125
    btr qword [rsi + 928], 13
    cmp ecx, 119
    jb run_sieve_dense_restore_125
    btr qword [rsi + 944], 10
    cmp ecx, 121
    jb run_sieve_dense_restore_125
    btr qword [rsi + 960], 7
    cmp ecx, 123
    jb run_sieve_dense_restore_125
    btr qword [rsi + 976], 4
    cmp ecx, 125
    jb run_sieve_dense_restore_125
    btr qword [rsi + 992], 1
run_sieve_dense_restore_125:
    bts qword [r14], 62
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_127:
    mov rsi, r14
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, 127
    jb run_sieve_dense_tail_127
align 16
run_sieve_dense_loop_127:
    btr qword [rsi], 63
    btr qword [rsi + 16], 62
    btr qword [rsi + 32], 61
    btr qword [rsi + 48], 60
    btr qword [rsi + 64], 59
    btr qword [rsi + 80], 58
    btr qword [rsi + 96], 57
    btr qword [rsi + 112], 56
    btr qword [rsi + 128], 55
    btr qword [rsi + 144], 54
    btr qword [rsi + 160], 53
    btr qword [rsi + 176], 52
    btr qword [rsi + 192], 51
    btr qword [rsi + 208], 50
    btr qword [rsi + 224], 49
    btr qword [rsi + 240], 48
    btr qword [rsi + 256], 47
    btr qword [rsi + 272], 46
    btr qword [rsi + 288], 45
    btr qword [rsi + 304], 44
    btr qword [rsi + 320], 43
    btr qword [rsi + 336], 42
    btr qword [rsi + 352], 41
    btr qword [rsi + 368], 40
    btr qword [rsi + 384], 39
    btr qword [rsi + 400], 38
    btr qword [rsi + 416], 37
    btr qword [rsi + 432], 36
    btr qword [rsi + 448], 35
    btr qword [rsi + 464], 34
    btr qword [rsi + 480], 33
    btr qword [rsi + 496], 32
    btr qword [rsi + 512], 31
    btr qword [rsi + 528], 30
    btr qword [rsi + 544], 29
    btr qword [rsi + 560], 28
    btr qword [rsi + 576], 27
    btr qword [rsi + 592], 26
    btr qword [rsi + 608], 25
    btr qword [rsi + 624], 24
    btr qword [rsi + 640], 23
    btr qword [rsi + 656], 22
    btr qword [rsi + 672], 21
    btr qword [rsi + 688], 20
    btr qword [rsi + 704], 19
    btr qword [rsi + 720], 18
    btr qword [rsi + 736], 17
    btr qword [rsi + 752], 16
    btr qword [rsi + 768], 15
    btr qword [rsi + 784], 14
    btr qword [rsi + 800], 13
    btr qword [rsi + 816], 12
    btr qword [rsi + 832], 11
    btr qword [rsi + 848], 10
    btr qword [rsi + 864], 9
    btr qword [rsi + 880], 8
    btr qword [rsi + 896], 7
    btr qword [rsi + 912], 6
    btr qword [rsi + 928], 5
    btr qword [rsi + 944], 4
    btr qword [rsi + 960], 3
    btr qword [rsi + 976], 2
    btr qword [rsi + 992], 1
    btr qword [rsi + 1008], 0
    add rsi, 1016
    sub ecx, 127
    cmp ecx, 127
    jae run_sieve_dense_loop_127
run_sieve_dense_tail_127:
    test ecx, ecx
    jz run_sieve_dense_restore_127
    cmp ecx, 1
    jb run_sieve_dense_restore_127
    btr qword [rsi], 63
    cmp ecx, 3
    jb run_sieve_dense_restore_127
    btr qword [rsi + 16], 62
    cmp ecx, 5
    jb run_sieve_dense_restore_127
    btr qword [rsi + 32], 61
    cmp ecx, 7
    jb run_sieve_dense_restore_127
    btr qword [rsi + 48], 60
    cmp ecx, 9
    jb run_sieve_dense_restore_127
    btr qword [rsi + 64], 59
    cmp ecx, 11
    jb run_sieve_dense_restore_127
    btr qword [rsi + 80], 58
    cmp ecx, 13
    jb run_sieve_dense_restore_127
    btr qword [rsi + 96], 57
    cmp ecx, 15
    jb run_sieve_dense_restore_127
    btr qword [rsi + 112], 56
    cmp ecx, 17
    jb run_sieve_dense_restore_127
    btr qword [rsi + 128], 55
    cmp ecx, 19
    jb run_sieve_dense_restore_127
    btr qword [rsi + 144], 54
    cmp ecx, 21
    jb run_sieve_dense_restore_127
    btr qword [rsi + 160], 53
    cmp ecx, 23
    jb run_sieve_dense_restore_127
    btr qword [rsi + 176], 52
    cmp ecx, 25
    jb run_sieve_dense_restore_127
    btr qword [rsi + 192], 51
    cmp ecx, 27
    jb run_sieve_dense_restore_127
    btr qword [rsi + 208], 50
    cmp ecx, 29
    jb run_sieve_dense_restore_127
    btr qword [rsi + 224], 49
    cmp ecx, 31
    jb run_sieve_dense_restore_127
    btr qword [rsi + 240], 48
    cmp ecx, 33
    jb run_sieve_dense_restore_127
    btr qword [rsi + 256], 47
    cmp ecx, 35
    jb run_sieve_dense_restore_127
    btr qword [rsi + 272], 46
    cmp ecx, 37
    jb run_sieve_dense_restore_127
    btr qword [rsi + 288], 45
    cmp ecx, 39
    jb run_sieve_dense_restore_127
    btr qword [rsi + 304], 44
    cmp ecx, 41
    jb run_sieve_dense_restore_127
    btr qword [rsi + 320], 43
    cmp ecx, 43
    jb run_sieve_dense_restore_127
    btr qword [rsi + 336], 42
    cmp ecx, 45
    jb run_sieve_dense_restore_127
    btr qword [rsi + 352], 41
    cmp ecx, 47
    jb run_sieve_dense_restore_127
    btr qword [rsi + 368], 40
    cmp ecx, 49
    jb run_sieve_dense_restore_127
    btr qword [rsi + 384], 39
    cmp ecx, 51
    jb run_sieve_dense_restore_127
    btr qword [rsi + 400], 38
    cmp ecx, 53
    jb run_sieve_dense_restore_127
    btr qword [rsi + 416], 37
    cmp ecx, 55
    jb run_sieve_dense_restore_127
    btr qword [rsi + 432], 36
    cmp ecx, 57
    jb run_sieve_dense_restore_127
    btr qword [rsi + 448], 35
    cmp ecx, 59
    jb run_sieve_dense_restore_127
    btr qword [rsi + 464], 34
    cmp ecx, 61
    jb run_sieve_dense_restore_127
    btr qword [rsi + 480], 33
    cmp ecx, 63
    jb run_sieve_dense_restore_127
    btr qword [rsi + 496], 32
    cmp ecx, 65
    jb run_sieve_dense_restore_127
    btr qword [rsi + 512], 31
    cmp ecx, 67
    jb run_sieve_dense_restore_127
    btr qword [rsi + 528], 30
    cmp ecx, 69
    jb run_sieve_dense_restore_127
    btr qword [rsi + 544], 29
    cmp ecx, 71
    jb run_sieve_dense_restore_127
    btr qword [rsi + 560], 28
    cmp ecx, 73
    jb run_sieve_dense_restore_127
    btr qword [rsi + 576], 27
    cmp ecx, 75
    jb run_sieve_dense_restore_127
    btr qword [rsi + 592], 26
    cmp ecx, 77
    jb run_sieve_dense_restore_127
    btr qword [rsi + 608], 25
    cmp ecx, 79
    jb run_sieve_dense_restore_127
    btr qword [rsi + 624], 24
    cmp ecx, 81
    jb run_sieve_dense_restore_127
    btr qword [rsi + 640], 23
    cmp ecx, 83
    jb run_sieve_dense_restore_127
    btr qword [rsi + 656], 22
    cmp ecx, 85
    jb run_sieve_dense_restore_127
    btr qword [rsi + 672], 21
    cmp ecx, 87
    jb run_sieve_dense_restore_127
    btr qword [rsi + 688], 20
    cmp ecx, 89
    jb run_sieve_dense_restore_127
    btr qword [rsi + 704], 19
    cmp ecx, 91
    jb run_sieve_dense_restore_127
    btr qword [rsi + 720], 18
    cmp ecx, 93
    jb run_sieve_dense_restore_127
    btr qword [rsi + 736], 17
    cmp ecx, 95
    jb run_sieve_dense_restore_127
    btr qword [rsi + 752], 16
    cmp ecx, 97
    jb run_sieve_dense_restore_127
    btr qword [rsi + 768], 15
    cmp ecx, 99
    jb run_sieve_dense_restore_127
    btr qword [rsi + 784], 14
    cmp ecx, 101
    jb run_sieve_dense_restore_127
    btr qword [rsi + 800], 13
    cmp ecx, 103
    jb run_sieve_dense_restore_127
    btr qword [rsi + 816], 12
    cmp ecx, 105
    jb run_sieve_dense_restore_127
    btr qword [rsi + 832], 11
    cmp ecx, 107
    jb run_sieve_dense_restore_127
    btr qword [rsi + 848], 10
    cmp ecx, 109
    jb run_sieve_dense_restore_127
    btr qword [rsi + 864], 9
    cmp ecx, 111
    jb run_sieve_dense_restore_127
    btr qword [rsi + 880], 8
    cmp ecx, 113
    jb run_sieve_dense_restore_127
    btr qword [rsi + 896], 7
    cmp ecx, 115
    jb run_sieve_dense_restore_127
    btr qword [rsi + 912], 6
    cmp ecx, 117
    jb run_sieve_dense_restore_127
    btr qword [rsi + 928], 5
    cmp ecx, 119
    jb run_sieve_dense_restore_127
    btr qword [rsi + 944], 4
    cmp ecx, 121
    jb run_sieve_dense_restore_127
    btr qword [rsi + 960], 3
    cmp ecx, 123
    jb run_sieve_dense_restore_127
    btr qword [rsi + 976], 2
    cmp ecx, 125
    jb run_sieve_dense_restore_127
    btr qword [rsi + 992], 1
    cmp ecx, 127
    jb run_sieve_dense_restore_127
    btr qword [rsi + 1008], 0
run_sieve_dense_restore_127:
    bts qword [r14], 63
    mov r13, [r14]
    jmp run_sieve_next
align 16
run_sieve_dense_129:
    lea rsi, [r14 + 1032]
    mov ecx, [r15 + worker_state.word_count]
    sub ecx, 129
    cmp ecx, 129
    jb run_sieve_dense_tail_129
align 16
run_sieve_dense_loop_129:
    btr qword [rsi + 8], 0
    btr qword [rsi + 24], 1
    btr qword [rsi + 40], 2
    btr qword [rsi + 56], 3
    btr qword [rsi + 72], 4
    btr qword [rsi + 88], 5
    btr qword [rsi + 104], 6
    btr qword [rsi + 120], 7
    btr qword [rsi + 136], 8
    btr qword [rsi + 152], 9
    btr qword [rsi + 168], 10
    btr qword [rsi + 184], 11
    btr qword [rsi + 200], 12
    btr qword [rsi + 216], 13
    btr qword [rsi + 232], 14
    btr qword [rsi + 248], 15
    btr qword [rsi + 264], 16
    btr qword [rsi + 280], 17
    btr qword [rsi + 296], 18
    btr qword [rsi + 312], 19
    btr qword [rsi + 328], 20
    btr qword [rsi + 344], 21
    btr qword [rsi + 360], 22
    btr qword [rsi + 376], 23
    btr qword [rsi + 392], 24
    btr qword [rsi + 408], 25
    btr qword [rsi + 424], 26
    btr qword [rsi + 440], 27
    btr qword [rsi + 456], 28
    btr qword [rsi + 472], 29
    btr qword [rsi + 488], 30
    btr qword [rsi + 504], 31
    btr qword [rsi + 520], 32
    btr qword [rsi + 536], 33
    btr qword [rsi + 552], 34
    btr qword [rsi + 568], 35
    btr qword [rsi + 584], 36
    btr qword [rsi + 600], 37
    btr qword [rsi + 616], 38
    btr qword [rsi + 632], 39
    btr qword [rsi + 648], 40
    btr qword [rsi + 664], 41
    btr qword [rsi + 680], 42
    btr qword [rsi + 696], 43
    btr qword [rsi + 712], 44
    btr qword [rsi + 728], 45
    btr qword [rsi + 744], 46
    btr qword [rsi + 760], 47
    btr qword [rsi + 776], 48
    btr qword [rsi + 792], 49
    btr qword [rsi + 808], 50
    btr qword [rsi + 824], 51
    btr qword [rsi + 840], 52
    btr qword [rsi + 856], 53
    btr qword [rsi + 872], 54
    btr qword [rsi + 888], 55
    btr qword [rsi + 904], 56
    btr qword [rsi + 920], 57
    btr qword [rsi + 936], 58
    btr qword [rsi + 952], 59
    btr qword [rsi + 968], 60
    btr qword [rsi + 984], 61
    btr qword [rsi + 1000], 62
    btr qword [rsi + 1016], 63
    add rsi, 1032
    sub ecx, 129
    cmp ecx, 129
    jae run_sieve_dense_loop_129
run_sieve_dense_tail_129:
    test ecx, ecx
    jz run_sieve_dense_restore_129
    cmp ecx, 2
    jb run_sieve_dense_restore_129
    btr qword [rsi + 8], 0
    cmp ecx, 4
    jb run_sieve_dense_restore_129
    btr qword [rsi + 24], 1
    cmp ecx, 6
    jb run_sieve_dense_restore_129
    btr qword [rsi + 40], 2
    cmp ecx, 8
    jb run_sieve_dense_restore_129
    btr qword [rsi + 56], 3
    cmp ecx, 10
    jb run_sieve_dense_restore_129
    btr qword [rsi + 72], 4
    cmp ecx, 12
    jb run_sieve_dense_restore_129
    btr qword [rsi + 88], 5
    cmp ecx, 14
    jb run_sieve_dense_restore_129
    btr qword [rsi + 104], 6
    cmp ecx, 16
    jb run_sieve_dense_restore_129
    btr qword [rsi + 120], 7
    cmp ecx, 18
    jb run_sieve_dense_restore_129
    btr qword [rsi + 136], 8
    cmp ecx, 20
    jb run_sieve_dense_restore_129
    btr qword [rsi + 152], 9
    cmp ecx, 22
    jb run_sieve_dense_restore_129
    btr qword [rsi + 168], 10
    cmp ecx, 24
    jb run_sieve_dense_restore_129
    btr qword [rsi + 184], 11
    cmp ecx, 26
    jb run_sieve_dense_restore_129
    btr qword [rsi + 200], 12
    cmp ecx, 28
    jb run_sieve_dense_restore_129
    btr qword [rsi + 216], 13
    cmp ecx, 30
    jb run_sieve_dense_restore_129
    btr qword [rsi + 232], 14
    cmp ecx, 32
    jb run_sieve_dense_restore_129
    btr qword [rsi + 248], 15
    cmp ecx, 34
    jb run_sieve_dense_restore_129
    btr qword [rsi + 264], 16
    cmp ecx, 36
    jb run_sieve_dense_restore_129
    btr qword [rsi + 280], 17
    cmp ecx, 38
    jb run_sieve_dense_restore_129
    btr qword [rsi + 296], 18
    cmp ecx, 40
    jb run_sieve_dense_restore_129
    btr qword [rsi + 312], 19
    cmp ecx, 42
    jb run_sieve_dense_restore_129
    btr qword [rsi + 328], 20
    cmp ecx, 44
    jb run_sieve_dense_restore_129
    btr qword [rsi + 344], 21
    cmp ecx, 46
    jb run_sieve_dense_restore_129
    btr qword [rsi + 360], 22
    cmp ecx, 48
    jb run_sieve_dense_restore_129
    btr qword [rsi + 376], 23
    cmp ecx, 50
    jb run_sieve_dense_restore_129
    btr qword [rsi + 392], 24
    cmp ecx, 52
    jb run_sieve_dense_restore_129
    btr qword [rsi + 408], 25
    cmp ecx, 54
    jb run_sieve_dense_restore_129
    btr qword [rsi + 424], 26
    cmp ecx, 56
    jb run_sieve_dense_restore_129
    btr qword [rsi + 440], 27
    cmp ecx, 58
    jb run_sieve_dense_restore_129
    btr qword [rsi + 456], 28
    cmp ecx, 60
    jb run_sieve_dense_restore_129
    btr qword [rsi + 472], 29
    cmp ecx, 62
    jb run_sieve_dense_restore_129
    btr qword [rsi + 488], 30
    cmp ecx, 64
    jb run_sieve_dense_restore_129
    btr qword [rsi + 504], 31
    cmp ecx, 66
    jb run_sieve_dense_restore_129
    btr qword [rsi + 520], 32
    cmp ecx, 68
    jb run_sieve_dense_restore_129
    btr qword [rsi + 536], 33
    cmp ecx, 70
    jb run_sieve_dense_restore_129
    btr qword [rsi + 552], 34
    cmp ecx, 72
    jb run_sieve_dense_restore_129
    btr qword [rsi + 568], 35
    cmp ecx, 74
    jb run_sieve_dense_restore_129
    btr qword [rsi + 584], 36
    cmp ecx, 76
    jb run_sieve_dense_restore_129
    btr qword [rsi + 600], 37
    cmp ecx, 78
    jb run_sieve_dense_restore_129
    btr qword [rsi + 616], 38
    cmp ecx, 80
    jb run_sieve_dense_restore_129
    btr qword [rsi + 632], 39
    cmp ecx, 82
    jb run_sieve_dense_restore_129
    btr qword [rsi + 648], 40
    cmp ecx, 84
    jb run_sieve_dense_restore_129
    btr qword [rsi + 664], 41
    cmp ecx, 86
    jb run_sieve_dense_restore_129
    btr qword [rsi + 680], 42
    cmp ecx, 88
    jb run_sieve_dense_restore_129
    btr qword [rsi + 696], 43
    cmp ecx, 90
    jb run_sieve_dense_restore_129
    btr qword [rsi + 712], 44
    cmp ecx, 92
    jb run_sieve_dense_restore_129
    btr qword [rsi + 728], 45
    cmp ecx, 94
    jb run_sieve_dense_restore_129
    btr qword [rsi + 744], 46
    cmp ecx, 96
    jb run_sieve_dense_restore_129
    btr qword [rsi + 760], 47
    cmp ecx, 98
    jb run_sieve_dense_restore_129
    btr qword [rsi + 776], 48
    cmp ecx, 100
    jb run_sieve_dense_restore_129
    btr qword [rsi + 792], 49
    cmp ecx, 102
    jb run_sieve_dense_restore_129
    btr qword [rsi + 808], 50
    cmp ecx, 104
    jb run_sieve_dense_restore_129
    btr qword [rsi + 824], 51
    cmp ecx, 106
    jb run_sieve_dense_restore_129
    btr qword [rsi + 840], 52
    cmp ecx, 108
    jb run_sieve_dense_restore_129
    btr qword [rsi + 856], 53
    cmp ecx, 110
    jb run_sieve_dense_restore_129
    btr qword [rsi + 872], 54
    cmp ecx, 112
    jb run_sieve_dense_restore_129
    btr qword [rsi + 888], 55
    cmp ecx, 114
    jb run_sieve_dense_restore_129
    btr qword [rsi + 904], 56
    cmp ecx, 116
    jb run_sieve_dense_restore_129
    btr qword [rsi + 920], 57
    cmp ecx, 118
    jb run_sieve_dense_restore_129
    btr qword [rsi + 936], 58
    cmp ecx, 120
    jb run_sieve_dense_restore_129
    btr qword [rsi + 952], 59
    cmp ecx, 122
    jb run_sieve_dense_restore_129
    btr qword [rsi + 968], 60
    cmp ecx, 124
    jb run_sieve_dense_restore_129
    btr qword [rsi + 984], 61
    cmp ecx, 126
    jb run_sieve_dense_restore_129
    btr qword [rsi + 1000], 62
    cmp ecx, 128
    jb run_sieve_dense_restore_129
    btr qword [rsi + 1016], 63
run_sieve_dense_restore_129:
    bts qword [r14 + 8], 0
    mov r13, [r14]
    jmp run_sieve_next

run_sieve_mark_large:
    mov eax, r11d
    mov edx, eax
    shr edx, 6

    mov ecx, eax
    and ecx, 63

    lea rsi, [r14 + rdx*8]
    mov r9, [rsi]

run_sieve_inner_large:
    btr r9, rcx

    add eax, r8d
    cmp eax, r12d
    jae run_sieve_store_exit

    add ecx, r8d
    mov edx, ecx
    shr edx, 6
    and ecx, 63
    test edx, edx
    jz run_sieve_inner_large

    mov [rsi], r9
    lea rsi, [rsi + rdx*8]
    mov r9, [rsi]
    jmp run_sieve_inner_large

run_sieve_store_exit:
    mov [rsi], r9

run_sieve_next:
    mov r13, [r14 + rdi*8]

run_sieve_advance:
    lea r11d, [r11d + r8d*2 + 2]
    add r8d, 2
    shl r10, 1
    jnz run_sieve_outer

    inc edi
    mov r10, 1
    mov r13, [r14 + rdi*8]
    jmp run_sieve_outer

run_sieve_done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbp
    pop rbx
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
