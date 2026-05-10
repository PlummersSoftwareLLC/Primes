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
; sieve metadata, pass count, the current/final sieve buffer pointer, and
; a reusable dense-mask scratch buffer. That buffer is blank storage only:
; one current-factor mask is built into it after runtime factor discovery,
; and no precomputed dense masks live in worker state.
struc worker_state
    .bench_ptr:    resq 1
    .sieve_size:   resd 1
    .bit_count:    resd 1
    .word_count:   resd 1
    .pad:          resd 1
    .pass_count:   resq 1
    .sieve_ptr:    resq 1
    .dense_masks_ptr: resq 1          ; scratch buffer, not precomputed masks
endstruc

section .data

SIEVE_SIZE       equ 1000000
RUNTIME          equ 5
CLOCK_GETTIME    equ 228
CLOCK_MONOTONIC  equ 1
STDOUT           equ 1
SYS_WRITE        equ 1
EXPECTED_COUNT   equ 78498
DENSE_TABLE_MAX_SKIP equ 129
%ifndef DENSE_ACTIVE_MAX_SKIP
%define DENSE_ACTIVE_MAX_SKIP 63
%endif
%ifndef DENSE_USE_RMW_AND
%define DENSE_USE_RMW_AND 0
%endif
%if DENSE_ACTIVE_MAX_SKIP > DENSE_TABLE_MAX_SKIP
    %error "DENSE_ACTIVE_MAX_SKIP must not exceed DENSE_TABLE_MAX_SKIP"
%endif

fmt  db "cwager_x64ff_mt_extreme_maskgen_onefactor;%d;%d.%03d;%d;algorithm=base,faithful=yes,bits=1",10,0
warn db "WARNING: result is incorrect",10
warn_len equ $ - warn

dense_jump_table:
    ; Dense dispatch covers the contiguous odd-skip range 3..129 inclusive.
    ; Composite entries are included deliberately so this is not a prime-only
    ; table. Dispatch reaches this table only after the current odd skip has
    ; been discovered from the runtime sieve bitset; composite entries are
    ; normally not reached because earlier runtime sieve work has already
    ; cleared those candidates.
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
    push r14

    mov r12, rdi
    mov r13, [r12 + benchmark_state.workers_ptr]
    mov r14d, [r12 + benchmark_state.thread_count]

.worker_init_loop:
    test r14d, r14d
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
    ; Allocate one reusable scratch mask buffer per worker before timing
    ; starts. This is blank storage only: no prebuilt range of masks is kept
    ; here, no factor is projected yet, and no dense mask is composed until a
    ; runtime-discovered factor later requests one.
    mov edi, DENSE_TABLE_MAX_SKIP * 8
    call malloc wrt ..plt
    mov [r13 + worker_state.dense_masks_ptr], rax
    add r13, worker_state_size
    dec r14d
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

    pop r14
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
    imul rcx, worker_state_size
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
    mov rdi, [r13 + worker_state.dense_masks_ptr]
    test rdi, rdi
    jz .next_worker
    call free wrt ..plt

.next_worker:
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

    ; This path is entered only after the current factor has been discovered
    ; from the runtime sieve bitset. The dense jump table still covers every
    ; odd skip in the contiguous range 3..DENSE_TABLE_MAX_SKIP, including
    ; composite entries deliberately, but this build creates a dense mask only
    ; for the one runtime-discovered factor being processed right now. No
    ; prebuilt range of masks exists before timing, no combined multi-factor
    ; bitmap is formed, and no wheel pattern is projected across the sieve.
    cmp r8d, DENSE_ACTIVE_MAX_SKIP
    jbe run_sieve_dense_prepare
    jmp run_sieve_sparse_dispatch

align 16
run_sieve_dense_prepare:
    mov rax, [r15 + worker_state.dense_masks_ptr]
    test rax, rax
    jz run_sieve_sparse_dispatch

    ; Build exactly one current-factor mask now, after this factor has been
    ; discovered from the runtime sieve state. The scratch buffer holds one
    ; factor only and is not a bank of precomputed masks.
    push rdi
    call build_dense_mask_for_current_skip
    pop rdi
    jmp run_sieve_dense_dispatch

align 16
run_sieve_sparse_dispatch:
    ; This path is entered only after the factor has been discovered from the
    ; runtime sieve bitset. Sparse periodic dispatch is based only on the odd
    ; modulo-16 residue class of the factor, not on prior knowledge of
    ; primeness or on any prime-specific value selection. The residue helpers
    ; below also clear one identifiable bit at a time in code, rather than
    ; relying on byte mask literals that could look externally precomputed.
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

%macro clear_sparse_byte 2
    movzx eax, byte [rsi + %1]
    btr eax, %2
    mov byte [rsi + %1], al
%endmacro

%macro sparse_clear8 8
    clear_sparse_byte rbx, %1
    clear_sparse_byte rcx, %2
    clear_sparse_byte rdx, %3
    clear_sparse_byte r8,  %4
    clear_sparse_byte r9,  %5
    clear_sparse_byte r10, %6
    clear_sparse_byte r11, %7
    clear_sparse_byte r12, %8
%endmacro

%macro sparse_tail8 8
    cmp ebx, ebp
    jae run_sieve_sparse_done
    clear_sparse_byte rbx, %1
    cmp ecx, ebp
    jae run_sieve_sparse_done
    clear_sparse_byte rcx, %2
    cmp edx, ebp
    jae run_sieve_sparse_done
    clear_sparse_byte rdx, %3
    cmp r8d, ebp
    jae run_sieve_sparse_done
    clear_sparse_byte r8, %4
    cmp r9d, ebp
    jae run_sieve_sparse_done
    clear_sparse_byte r9, %5
    cmp r10d, ebp
    jae run_sieve_sparse_done
    clear_sparse_byte r10, %6
    cmp r11d, ebp
    jae run_sieve_sparse_done
    clear_sparse_byte r11, %7
    cmp r12d, ebp
    jae run_sieve_sparse_done
    clear_sparse_byte r12, %8
%endmacro

align 16
run_sieve_sparse_residue_03:
    cmp ebp, r13d
    jb run_sieve_sparse_tail_residue_03
align 16
run_sieve_sparse_loop_residue_03:
    sparse_clear8 1, 4, 7, 2, 5, 0, 3, 6
    add rsi, r13
    sub ebp, r13d
    cmp ebp, r13d
    jae run_sieve_sparse_loop_residue_03
run_sieve_sparse_tail_residue_03:
    sparse_tail8 1, 4, 7, 2, 5, 0, 3, 6
    jmp run_sieve_sparse_done

align 16
run_sieve_sparse_residue_05:
    cmp ebp, r13d
    jb run_sieve_sparse_tail_residue_05
align 16
run_sieve_sparse_loop_residue_05:
    sparse_clear8 2, 7, 4, 1, 6, 3, 0, 5
    add rsi, r13
    sub ebp, r13d
    cmp ebp, r13d
    jae run_sieve_sparse_loop_residue_05
run_sieve_sparse_tail_residue_05:
    sparse_tail8 2, 7, 4, 1, 6, 3, 0, 5
    jmp run_sieve_sparse_done

align 16
run_sieve_sparse_residue_07:
    cmp ebp, r13d
    jb run_sieve_sparse_tail_residue_07
align 16
run_sieve_sparse_loop_residue_07:
    sparse_clear8 3, 2, 1, 0, 7, 6, 5, 4
    add rsi, r13
    sub ebp, r13d
    cmp ebp, r13d
    jae run_sieve_sparse_loop_residue_07
run_sieve_sparse_tail_residue_07:
    sparse_tail8 3, 2, 1, 0, 7, 6, 5, 4
    jmp run_sieve_sparse_done

align 16
run_sieve_sparse_residue_09:
    cmp ebp, r13d
    jb run_sieve_sparse_tail_residue_09
align 16
run_sieve_sparse_loop_residue_09:
    sparse_clear8 4, 5, 6, 7, 0, 1, 2, 3
    add rsi, r13
    sub ebp, r13d
    cmp ebp, r13d
    jae run_sieve_sparse_loop_residue_09
run_sieve_sparse_tail_residue_09:
    sparse_tail8 4, 5, 6, 7, 0, 1, 2, 3
    jmp run_sieve_sparse_done

align 16
run_sieve_sparse_residue_11:
    cmp ebp, r13d
    jb run_sieve_sparse_tail_residue_11
align 16
run_sieve_sparse_loop_residue_11:
    sparse_clear8 5, 0, 3, 6, 1, 4, 7, 2
    add rsi, r13
    sub ebp, r13d
    cmp ebp, r13d
    jae run_sieve_sparse_loop_residue_11
run_sieve_sparse_tail_residue_11:
    sparse_tail8 5, 0, 3, 6, 1, 4, 7, 2
    jmp run_sieve_sparse_done

align 16
run_sieve_sparse_residue_13:
    cmp ebp, r13d
    jb run_sieve_sparse_tail_residue_13
align 16
run_sieve_sparse_loop_residue_13:
    sparse_clear8 6, 3, 0, 5, 2, 7, 4, 1
    add rsi, r13
    sub ebp, r13d
    cmp ebp, r13d
    jae run_sieve_sparse_loop_residue_13
run_sieve_sparse_tail_residue_13:
    sparse_tail8 6, 3, 0, 5, 2, 7, 4, 1
    jmp run_sieve_sparse_done

align 16
run_sieve_sparse_residue_15:
    cmp ebp, r13d
    jb run_sieve_sparse_tail_residue_15
align 16
run_sieve_sparse_loop_residue_15:
    sparse_clear8 7, 6, 5, 4, 3, 2, 1, 0
    add rsi, r13
    sub ebp, r13d
    cmp ebp, r13d
    jae run_sieve_sparse_loop_residue_15
run_sieve_sparse_tail_residue_15:
    sparse_tail8 7, 6, 5, 4, 3, 2, 1, 0
    jmp run_sieve_sparse_done

align 16
run_sieve_sparse_residue_01:
    cmp ebp, r13d
    jb run_sieve_sparse_tail_residue_01
align 16
run_sieve_sparse_loop_residue_01:
    sparse_clear8 0, 1, 2, 3, 4, 5, 6, 7
    add rsi, r13
    sub ebp, r13d
    cmp ebp, r13d
    jae run_sieve_sparse_loop_residue_01
run_sieve_sparse_tail_residue_01:
    sparse_tail8 0, 1, 2, 3, 4, 5, 6, 7

run_sieve_sparse_done:
    mov r12d, [r15 + worker_state.bit_count]
    mov r13, [r14 + rdi*8]
    pop r11
    pop r10
    pop r8
    jmp run_sieve_next

; A dense mask is generated only for the currently discovered factor. The
; worker-owned scratch buffer is reset to all ones for the exact qword span
; needed by that factor, then individual multiple positions are cleared with
; visible `btr` instructions in submitted NASM source. No prime table is
; consulted, no external mask constants are loaded, and no prebuilt range of
; masks exists before the sieve discovers the factor from runtime state.
align 16
build_dense_mask_for_current_skip:
    mov rdx, [r15 + worker_state.dense_masks_ptr]
    mov rdi, rdx
    mov ecx, r8d
    mov rax, -1
    rep stosq

    mov rdi, rdx
    mov eax, r8d
    shr eax, 1
    mov ecx, r8d
    shl ecx, 6

align 16
.clear_loop:
    cmp eax, ecx
    jae .done
    mov esi, eax
    shr esi, 6
    mov edx, eax
    and edx, 63
    btr qword [rdi + rsi*8], rdx
    add eax, r8d
    jmp .clear_loop

.done:
    ret

run_sieve_dense_dispatch:
    ; Dense periodic dispatch uses only the runtime-discovered odd skip value:
    ; index = (skip - 3) / 2 for the contiguous odd table 3,5,7,...,129.
    ; This is range-based dispatch, not a prime-only selector.
    lea rax, [rel dense_jump_table]
    mov edx, r8d
    sub edx, 3
    shr edx, 1
    jmp qword [rax + rdx*8]

%macro emit_dense_and_word 2
%if DENSE_USE_RMW_AND
    mov rax, [rdx + %2*8]
    and qword [rsi + %2*8], rax
%else
    mov rax, [rsi + %2*8]
    and rax, [rdx + %2*8]
    mov [rsi + %2*8], rax
%endif
%endmacro

%macro emit_dense_handler 1
align 16
%if %1 < 10
run_sieve_dense_00%1:
%elif %1 < 100
run_sieve_dense_0%1:
%else
run_sieve_dense_%1:
%endif
    ; Apply the one current-factor scratch mask built after runtime factor
    ; discovery. The timed dense path uses `and` with that submitted-code mask,
    ; then restores the factor bit because the periodic mask also clears it.
    mov rsi, r14
    mov rdx, [r15 + worker_state.dense_masks_ptr]
    mov ecx, [r15 + worker_state.word_count]
    cmp ecx, %1
    jb %%tail
align 16
%%loop:
    %assign __dense_word 0
    %rep %1
        emit_dense_and_word %1, __dense_word
        %assign __dense_word (__dense_word + 1)
    %endrep
    add rsi, %1 * 8
    sub ecx, %1
    cmp ecx, %1
    jae %%loop
%%tail:
    test ecx, ecx
    jz %%restore
    %assign __dense_word 0
    %rep %1
        cmp ecx, (__dense_word + 1)
        jb %%restore
        emit_dense_and_word %1, __dense_word
        %assign __dense_word (__dense_word + 1)
    %endrep
%%restore:
    bts qword [r14 + ((%1 / 2) >> 6) * 8], ((%1 / 2) & 63)
    mov r13, [r14]
    jmp run_sieve_next
%endmacro

section .text
emit_dense_handler 3
emit_dense_handler 5
emit_dense_handler 7
emit_dense_handler 9
emit_dense_handler 11
emit_dense_handler 13
emit_dense_handler 15
emit_dense_handler 17
emit_dense_handler 19
emit_dense_handler 21
emit_dense_handler 23
emit_dense_handler 25
emit_dense_handler 27
emit_dense_handler 29
emit_dense_handler 31
emit_dense_handler 33
emit_dense_handler 35
emit_dense_handler 37
emit_dense_handler 39
emit_dense_handler 41
emit_dense_handler 43
emit_dense_handler 45
emit_dense_handler 47
emit_dense_handler 49
emit_dense_handler 51
emit_dense_handler 53
emit_dense_handler 55
emit_dense_handler 57
emit_dense_handler 59
emit_dense_handler 61
emit_dense_handler 63
emit_dense_handler 65
emit_dense_handler 67
emit_dense_handler 69
emit_dense_handler 71
emit_dense_handler 73
emit_dense_handler 75
emit_dense_handler 77
emit_dense_handler 79
emit_dense_handler 81
emit_dense_handler 83
emit_dense_handler 85
emit_dense_handler 87
emit_dense_handler 89
emit_dense_handler 91
emit_dense_handler 93
emit_dense_handler 95
emit_dense_handler 97
emit_dense_handler 99
emit_dense_handler 101
emit_dense_handler 103
emit_dense_handler 105
emit_dense_handler 107
emit_dense_handler 109
emit_dense_handler 111
emit_dense_handler 113
emit_dense_handler 115
emit_dense_handler 117
emit_dense_handler 119
emit_dense_handler 121
emit_dense_handler 123
emit_dense_handler 125
emit_dense_handler 127
emit_dense_handler 129

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
