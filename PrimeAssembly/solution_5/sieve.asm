.intel_syntax noprefix
.text
.globl run_sieve_pass
.p2align 5

# ==============================================================================
# uint64_t run_sieve_pass(uint8_t* buffer [rdi], uint64_t limit [rsi])
# ==============================================================================
run_sieve_pass:
    push    rbp
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15

    # rdi: buffer base (64-byte aligned)
    # rsi: limit (1,000,000)

    # 1. Tier 3: AVX2 pattern initialization with prime 7 pattern (149 iters * 224 bytes = 33,376 bytes)
    lea     rax, [rip + pat7_table]
    vmovdqa ymm0, [rax]
    vmovdqa ymm1, [rax + 32]
    vmovdqa ymm2, [rax + 64]
    vmovdqa ymm3, [rax + 96]
    vmovdqa ymm4, [rax + 128]
    vmovdqa ymm5, [rax + 160]
    vmovdqa ymm6, [rax + 192]

    mov     rcx, 149
    mov     rdx, rdi

.p2align 4
.Lavx2_pat7_loop:
    vmovdqa [rdx], ymm0
    vmovdqa [rdx + 32], ymm1
    vmovdqa [rdx + 64], ymm2
    vmovdqa [rdx + 96], ymm3
    vmovdqa [rdx + 128], ymm4
    vmovdqa [rdx + 160], ymm5
    vmovdqa [rdx + 192], ymm6
    add     rdx, 224
    dec     rcx
    jnz     .Lavx2_pat7_loop

    # 2. Tier 3: AVX2 pattern blitting with prime 11 pattern (95 iters * 352 bytes = 33,440 bytes)
    lea     rax, [rip + pat11_table]
    vmovdqa ymm0, [rax]
    vmovdqa ymm1, [rax + 32]
    vmovdqa ymm2, [rax + 64]
    vmovdqa ymm3, [rax + 96]
    vmovdqa ymm4, [rax + 128]
    vmovdqa ymm5, [rax + 160]
    vmovdqa ymm6, [rax + 192]
    vmovdqa ymm7, [rax + 224]
    vmovdqa ymm8, [rax + 256]
    vmovdqa ymm9, [rax + 288]
    vmovdqa ymm10, [rax + 320]

    mov     rcx, 95
    mov     rdx, rdi

.p2align 4
.Lavx2_pat11_loop:
    vpor    ymm11, ymm0, [rdx]
    vmovdqa [rdx], ymm11
    vpor    ymm12, ymm1, [rdx + 32]
    vmovdqa [rdx + 32], ymm12
    vpor    ymm11, ymm2, [rdx + 64]
    vmovdqa [rdx + 64], ymm11
    vpor    ymm12, ymm3, [rdx + 96]
    vmovdqa [rdx + 96], ymm12
    vpor    ymm11, ymm4, [rdx + 128]
    vmovdqa [rdx + 128], ymm11
    vpor    ymm12, ymm5, [rdx + 160]
    vmovdqa [rdx + 160], ymm12
    vpor    ymm11, ymm6, [rdx + 192]
    vmovdqa [rdx + 192], ymm11
    vpor    ymm12, ymm7, [rdx + 224]
    vmovdqa [rdx + 224], ymm12
    vpor    ymm11, ymm8, [rdx + 256]
    vmovdqa [rdx + 256], ymm11
    vpor    ymm12, ymm9, [rdx + 288]
    vmovdqa [rdx + 288], ymm12
    vpor    ymm11, ymm10, [rdx + 320]
    vmovdqa [rdx + 320], ymm11
    add     rdx, 352
    dec     rcx
    jnz     .Lavx2_pat11_loop

    # 3. Tier 3: AVX2 pattern blitting with prime 13 pattern (81 iters * 416 bytes = 33,696 bytes)
    lea     rax, [rip + pat13_table]
    vmovdqa ymm0, [rax]
    vmovdqa ymm1, [rax + 32]
    vmovdqa ymm2, [rax + 64]
    vmovdqa ymm3, [rax + 96]
    vmovdqa ymm4, [rax + 128]
    vmovdqa ymm5, [rax + 160]
    vmovdqa ymm6, [rax + 192]
    vmovdqa ymm7, [rax + 224]
    vmovdqa ymm8, [rax + 256]
    vmovdqa ymm9, [rax + 288]
    vmovdqa ymm10, [rax + 320]
    vmovdqa ymm11, [rax + 352]
    vmovdqa ymm12, [rax + 384]

    mov     rcx, 81
    mov     rdx, rdi

.p2align 4
.Lavx2_pat13_loop:
    vpor    ymm13, ymm0, [rdx]
    vmovdqa [rdx], ymm13
    vpor    ymm14, ymm1, [rdx + 32]
    vmovdqa [rdx + 32], ymm14
    vpor    ymm13, ymm2, [rdx + 64]
    vmovdqa [rdx + 64], ymm13
    vpor    ymm14, ymm3, [rdx + 96]
    vmovdqa [rdx + 96], ymm14
    vpor    ymm13, ymm4, [rdx + 128]
    vmovdqa [rdx + 128], ymm13
    vpor    ymm14, ymm5, [rdx + 160]
    vmovdqa [rdx + 160], ymm14
    vpor    ymm13, ymm6, [rdx + 192]
    vmovdqa [rdx + 192], ymm13
    vpor    ymm14, ymm7, [rdx + 224]
    vmovdqa [rdx + 224], ymm14
    vpor    ymm13, ymm8, [rdx + 256]
    vmovdqa [rdx + 256], ymm13
    vpor    ymm14, ymm9, [rdx + 288]
    vmovdqa [rdx + 288], ymm14
    vpor    ymm13, ymm10, [rdx + 320]
    vmovdqa [rdx + 320], ymm13
    vpor    ymm14, ymm11, [rdx + 352]
    vmovdqa [rdx + 352], ymm14
    vpor    ymm13, ymm12, [rdx + 384]
    vmovdqa [rdx + 384], ymm13
    add     rdx, 416
    dec     rcx
    jnz     .Lavx2_pat13_loop

    # 4. Tier 3: AVX2 pattern blitting with prime 17 pattern (62 iters * 544 bytes = 33,728 bytes)
    lea     rax, [rip + pat17_table]
    mov     rcx, 62
    mov     rdx, rdi

.p2align 4
.Lavx2_pat17_loop:
    .irp i, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16
        vmovdqa ymm0, [rax + \i * 32]
        vpor    ymm0, ymm0, [rdx + \i * 32]
        vmovdqa [rdx + \i * 32], ymm0
    .endr
    add     rdx, 544
    dec     rcx
    jnz     .Lavx2_pat17_loop

    # 5. Tier 3: AVX2 pattern blitting with prime 19 pattern (56 iters * 608 bytes = 34,048 bytes)
    lea     rax, [rip + pat19_table]
    mov     rcx, 56
    mov     rdx, rdi

.p2align 4
.Lavx2_pat19_loop:
    .irp i, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18
        vmovdqa ymm0, [rax + \i * 32]
        vpor    ymm0, ymm0, [rdx + \i * 32]
        vmovdqa [rdx + \i * 32], ymm0
    .endr
    add     rdx, 608
    dec     rcx
    jnz     .Lavx2_pat19_loop

    # 6. Mark non-prime boundary bits & restore primes 7, 11, 13, 17, 19:
    # ~(2 | 4 | 8 | 16 | 32) = ~62 = 0xC1
    and     byte ptr [rdi], 0xC1
    or      byte ptr [rdi], 1
    or      byte ptr [rdi + 33333], 0xFC
    mov     byte ptr [rdi + 33334], 0xFF
    mov     byte ptr [rdi + 33335], 0xFF

    # 7. Discover primes up to 1000 starting from 23
    lea     rbp, [rip + tables]
    xor     r12d, r12d          # b = 0 (byte index)

.Lbyte_search_loop:
    movzx   ebx, byte ptr [rdi + r12]
    cmp     bl, 0xFF
    je      .Lnext_byte

    xor     r15d, r15d          # i = 0 (wheel index 0..7)
.Lbit_search_loop:
    bt      ebx, r15d
    jc      .Lnext_bit          # composite

    # p = b * 30 + wheel[i]
    imul    eax, r12d, 30
    movzx   ecx, byte ptr [rbp + r15] # wheel[i]
    add     eax, ecx            # eax = p

    cmp     eax, 19
    jbe     .Lnext_bit          # skip <= 19 (already sieved!)

    cmp     eax, 1000
    ja      .Ldone_sieve

    # Sieve multiples of prime p (p >= 23)
    mov     r8d, eax            # r8d = p
    xor     r9d, r9d            # qi = 0 (0..7)

.Lqi_loop:
    movzx   r10d, byte ptr [rbp + r9] # qr = wheel[qi]

    # rem_diff = (qr >= p_mod30) ? (qr - p_mod30) : (qr + 30 - p_mod30)
    mov     r11d, r10d
    sub     r11d, ecx
    jge     1f
    add     r11d, 30
1:
    # q = p + rem_diff
    add     r11d, r8d           # q

    # m = p * q
    mov     rax, r8
    imul    rax, r11            # m
    cmp     rax, 1000000
    ja      .Lnext_qi

    # m_byte = (m * 0x88888889) >> 36
    mov     rdx, 0x88888889
    imul    rdx, rax
    shr     rdx, 36             # rdx = m_byte

    # m_rem = m - m_byte * 30
    imul    ecx, edx, 30
    mov     r11d, eax
    sub     r11d, ecx           # r11d = m_rem

    # mask = 1 << res_to_bit[m_rem]
    movzx   r11d, byte ptr [rbp + 8 + r11] # m_bit
    mov     eax, 1
    shlx    eax, eax, r11d      # al = mask

    # Marking loop for stream qi:
    lea     rsi, [rdi + rdx]    # ptr = buffer + m_byte
    lea     rdx, [rdi + 33334]  # end = buffer + 33334

    # 4-way pipelined unrolling setup
    lea     r13, [r8 + r8*2]    # r13 = 3*p
    lea     r14, [r8 * 4]       # r14 = 4*p
    mov     rcx, rdx
    sub     rcx, r13            # rcx = limit4 (end - 3*p)

    cmp     rsi, rcx
    jae     .Lmark_tail

.p2align 4
.Lmark_stream_pipelined4:
    movzx   r10d, byte ptr [rsi]
    movzx   r11d, byte ptr [rsi + r8]
    movzx   ebx,  byte ptr [rsi + r8*2]
    movzx   edx,  byte ptr [rsi + r13]

    or      r10b, al
    or      r11b, al
    or      bl,   al
    or      dl,   al

    mov     byte ptr [rsi], r10b
    mov     byte ptr [rsi + r8], r11b
    mov     byte ptr [rsi + r8*2], bl
    mov     byte ptr [rsi + r13], dl

    add     rsi, r14
    cmp     rsi, rcx
    jb      .Lmark_stream_pipelined4

.Lmark_tail:
    lea     rdx, [rdi + 33334]  # restore rdx = end
.Lmark_tail_loop:
    cmp     rsi, rdx
    jae     .Lnext_qi
    or      byte ptr [rsi], al
    add     rsi, r8
    jmp     .Lmark_tail_loop

.Lnext_qi:
    movzx   ebx, byte ptr [rdi + r12] # reload ebx for byte_search
    movzx   ecx, byte ptr [rbp + r15] # reload p_mod30
    inc     r9d
    cmp     r9d, 8
    jne     .Lqi_loop

.Lnext_bit:
    inc     r15d
    cmp     r15d, 8
    jne     .Lbit_search_loop

.Lnext_byte:
    inc     r12d
    cmp     r12d, 34
    jb      .Lbyte_search_loop

.Ldone_sieve:
    # 8. Count primes: 4-way unrolled dependency-free popcnt over 4167 quadwords
    xor     rax, rax            # acc0
    xor     r8,  r8             # acc1
    xor     r9,  r9             # acc2
    xor     r10, r10            # acc3
    mov     rcx, 1041           # 1041 * 4 = 4164 quadwords
    mov     rdx, rdi

.p2align 4
.Lcount_unroll4:
    popcnt  r11, qword ptr [rdx]
    add     rax, r11
    popcnt  r12, qword ptr [rdx + 8]
    add     r8,  r12
    popcnt  r13, qword ptr [rdx + 16]
    add     r9,  r13
    popcnt  r14, qword ptr [rdx + 24]
    add     r10, r14
    add     rdx, 32
    dec     rcx
    jnz     .Lcount_unroll4

    # 3 tail quadwords (4167 - 4164 = 3)
    popcnt  r11, qword ptr [rdx]
    add     rax, r11
    popcnt  r12, qword ptr [rdx + 8]
    add     r8,  r12
    popcnt  r13, qword ptr [rdx + 16]
    add     r9,  r13

    # Sum accumulators
    add     rax, r8
    add     r9,  r10
    add     rax, r9

    mov     rcx, 266691
    sub     rcx, rax
    mov     rax, rcx            # rax = 78498

    vzeroupper

    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

# Read-only tables
.section .rodata
.p2align 5
pat7_table:
    # 7 YMM vectors (224 bytes)
    .quad 0x0240040881102002, 0x2002400408811020, 0x1020024004088110, 0x8110200240040881
    .quad 0x0881102002400408, 0x0408811020024004, 0x4004088110200240, 0x0240040881102002
    .quad 0x2002400408811020, 0x1020024004088110, 0x8110200240040881, 0x0881102002400408
    .quad 0x0408811020024004, 0x4004088110200240, 0x0240040881102002, 0x2002400408811020
    .quad 0x1020024004088110, 0x8110200240040881, 0x0881102002400408, 0x0408811020024004
    .quad 0x4004088110200240, 0x0240040881102002, 0x2002400408811020, 0x1020024004088110
    .quad 0x8110200240040881, 0x0881102002400408, 0x0408811020024004, 0x4004088110200240

.p2align 5
pat11_table:
    # 11 YMM vectors (352 bytes)
    .quad 0x0082004100100004, 0x4100100004200008, 0x0004200008008200, 0x0008008200410010
    .quad 0x8200410010000420, 0x0010000420000800, 0x0420000800820041, 0x0800820041001000
    .quad 0x0041001000042000, 0x1000042000080082, 0x2000080082004100, 0x0082004100100004
    .quad 0x4100100004200008, 0x0004200008008200, 0x0008008200410010, 0x8200410010000420
    .quad 0x0010000420000800, 0x0420000800820041, 0x0800820041001000, 0x0041001000042000
    .quad 0x1000042000080082, 0x2000080082004100, 0x0082004100100004, 0x4100100004200008
    .quad 0x0004200008008200, 0x0008008200410010, 0x8200410010000420, 0x0010000420000800
    .quad 0x0420000800820041, 0x0800820041001000, 0x0041001000042000, 0x1000042000080082
    .quad 0x2000080082004100, 0x0082004100100004, 0x4100100004200008, 0x0004200008008200
    .quad 0x0008008200410010, 0x8200410010000420, 0x0010000420000800, 0x0420000800820041
    .quad 0x0800820041001000, 0x0041001000042000, 0x1000042000080082, 0x2000080082004100

.p2align 5
pat13_table:
    # 13 YMM vectors (416 bytes)
    .quad 0x0400204001000008, 0x0000081000008002, 0x0080020400204001, 0x2040010000081000
    .quad 0x0810000080020400, 0x0204002040010000, 0x0100000810000080, 0x0000800204002040
    .quad 0x0020400100000810, 0x0008100000800204, 0x8002040020400100, 0x4001000008100000
    .quad 0x1000008002040020, 0x0400204001000008, 0x0000081000008002, 0x0080020400204001
    .quad 0x2040010000081000, 0x0810000080020400, 0x0204002040010000, 0x0100000810000080
    .quad 0x0000800204002040, 0x0020400100000810, 0x0008100000800204, 0x8002040020400100
    .quad 0x4001000008100000, 0x1000008002040020, 0x0400204001000008, 0x0000081000008002
    .quad 0x0080020400204001, 0x2040010000081000, 0x0810000080020400, 0x0204002040010000
    .quad 0x0100000810000080, 0x0000800204002040, 0x0020400100000810, 0x0008100000800204
    .quad 0x8002040020400100, 0x4001000008100000, 0x1000008002040020, 0x0400204001000008
    .quad 0x0000081000008002, 0x0080020400204001, 0x2040010000081000, 0x0810000080020400
    .quad 0x0204002040010000, 0x0100000810000080, 0x0000800204002040, 0x0020400100000810
    .quad 0x0008100000800204, 0x8002040020400100, 0x4001000008100000, 0x1000008002040020

.p2align 5
pat17_table:
    # 17 YMM vectors (544 bytes)
    .quad 0x0402000080000010, 0x0000010000402000, 0x0200008000001008, 0x0001000040200004
    .quad 0x0000800000100800, 0x0100004020000402, 0x0080000010080000, 0x0000402000040200
    .quad 0x8000001008000001, 0x0040200004020000, 0x0000100800000100, 0x4020000402000080
    .quad 0x0010080000010000, 0x2000040200008000, 0x1008000001000040, 0x0004020000800000
    .quad 0x0800000100004020, 0x0402000080000010, 0x0000010000402000, 0x0200008000001008
    .quad 0x0001000040200004, 0x0000800000100800, 0x0100004020000402, 0x0080000010080000
    .quad 0x0000402000040200, 0x8000001008000001, 0x0040200004020000, 0x0000100800000100
    .quad 0x4020000402000080, 0x0010080000010000, 0x2000040200008000, 0x1008000001000040
    .quad 0x0004020000800000, 0x0800000100004020, 0x0402000080000010, 0x0000010000402000
    .quad 0x0200008000001008, 0x0001000040200004, 0x0000800000100800, 0x0100004020000402
    .quad 0x0080000010080000, 0x0000402000040200, 0x8000001008000001, 0x0040200004020000
    .quad 0x0000100800000100, 0x4020000402000080, 0x0010080000010000, 0x2000040200008000
    .quad 0x1008000001000040, 0x0004020000800000, 0x0800000100004020, 0x0402000080000010
    .quad 0x0000010000402000, 0x0200008000001008, 0x0001000040200004, 0x0000800000100800
    .quad 0x0100004020000402, 0x0080000010080000, 0x0000402000040200, 0x8000001008000001
    .quad 0x0040200004020000, 0x0000100800000100, 0x4020000402000080, 0x0010080000010000
    .quad 0x2000040200008000, 0x1008000001000040, 0x0004020000800000, 0x0800000100004020

.p2align 5
pat19_table:
    # 19 YMM vectors (608 bytes)
    .quad 0x0080000800000020, 0x0010000100400002, 0x0800000020040000, 0x0100400002008000
    .quad 0x0020040000001000, 0x0002008000080000, 0x0000001000010040, 0x8000080000002004
    .quad 0x1000010040000200, 0x0000002004000000, 0x0040000200800008, 0x2004000000100001
    .quad 0x0200800008000000, 0x0000100001004000, 0x0008000000200400, 0x0001004000020080
    .quad 0x0000200400000010, 0x4000020080000800, 0x0400000010000100, 0x0080000800000020
    .quad 0x0010000100400002, 0x0800000020040000, 0x0100400002008000, 0x0020040000001000
    .quad 0x0002008000080000, 0x0000001000010040, 0x8000080000002004, 0x1000010040000200
    .quad 0x0000002004000000, 0x0040000200800008, 0x2004000000100001, 0x0200800008000000
    .quad 0x0000100001004000, 0x0008000000200400, 0x0001004000020080, 0x0000200400000010
    .quad 0x4000020080000800, 0x0400000010000100, 0x0080000800000020, 0x0010000100400002
    .quad 0x0800000020040000, 0x0100400002008000, 0x0020040000001000, 0x0002008000080000
    .quad 0x0000001000010040, 0x8000080000002004, 0x1000010040000200, 0x0000002004000000
    .quad 0x0040000200800008, 0x2004000000100001, 0x0200800008000000, 0x0000100001004000
    .quad 0x0008000000200400, 0x0001004000020080, 0x0000200400000010, 0x4000020080000800
    .quad 0x0400000010000100, 0x0080000800000020, 0x0010000100400002, 0x0800000020040000
    .quad 0x0100400002008000, 0x0020040000001000, 0x0002008000080000, 0x0000001000010040
    .quad 0x8000080000002004, 0x1000010040000200, 0x0000002004000000, 0x0040000200800008
    .quad 0x2004000000100001, 0x0200800008000000, 0x0000100001004000, 0x0008000000200400
    .quad 0x0001004000020080, 0x0000200400000010, 0x4000020080000800, 0x0400000010000100

.p2align 4
tables:
wheel_table:
    .byte 1, 7, 11, 13, 17, 19, 23, 29

res_to_bit_table:
    .byte 0, 0, 0, 0, 0, 0, 0, 1, 0, 0
    .byte 0, 2, 0, 3, 0, 0, 0, 4, 0, 5
    .byte 0, 0, 0, 6, 0, 0, 0, 0, 0, 7
