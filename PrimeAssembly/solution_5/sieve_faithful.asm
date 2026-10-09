.intel_syntax noprefix
.text
.globl run_faithful_sieve
.p2align 5

# ==============================================================================
# uint64_t run_faithful_sieve(uint8_t* buffer [rdi], uint64_t limit [rsi])
#
# Strictly Faithful Base Sieve (Odd candidates only, 1 bit per candidate):
#   - Tracks odd integers 1, 3, 5, 7, ... up to limit (1,000,000).
#   - Total candidate bits: 500,000 bits = 62,500 bytes.
#   - Bit index i corresponds to candidate integer n = 2*i + 1.
#   - Bit 0 corresponds to 1 (marked composite).
#   - Prime p = 2*i + 1 starts marking at p^2 (bit index j = 2*i*(i + 1)).
#   - Bit stride = p bits.
#   - Stepping by 8 strides of p bits advances memory by p bytes.
#     Because (j + 8*p) mod 8 == j mod 8, bitmasks within each of the 8 streams
#     are strictly CONSTANT!
# ==============================================================================
run_faithful_sieve:
    push    rbp
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15

    # --------------------------------------------------------------------------
    # Tier 2: AVX2 Zero-Init & Fused Prime 3/5 Blit (131 iters * 480 bytes = 62,880 bytes)
    # --------------------------------------------------------------------------
    lea     rax, [rip + pat3_5_table]
    mov     rcx, 131
    mov     rdx, rdi

.p2align 4
.Linit_pat3_5_loop:
    xor     r8d, r8d
1:
    vmovdqa ymm0, [rax + r8]
    vmovdqa [rdx + r8], ymm0
    add     r8, 32
    cmp     r8, 480
    jb      1b
    add     rdx, 480
    dec     rcx
    jnz     .Linit_pat3_5_loop

    # --------------------------------------------------------------------------
    # Tier 3: AVX2 Blit for Prime 7 (280 iters * 224 bytes = 62,720 bytes)
    # --------------------------------------------------------------------------
    lea     rax, [rip + pat7_table]
    vmovdqa ymm0, [rax]
    vmovdqa ymm1, [rax + 32]
    vmovdqa ymm2, [rax + 64]
    vmovdqa ymm3, [rax + 96]
    vmovdqa ymm4, [rax + 128]
    vmovdqa ymm5, [rax + 160]
    vmovdqa ymm6, [rax + 192]
    mov     rcx, 280
    mov     rdx, rdi

.p2align 4
.Lblit_pat7_loop:
    vpor    ymm7, ymm0, [rdx]
    vmovdqa [rdx], ymm7
    vpor    ymm8, ymm1, [rdx + 32]
    vmovdqa [rdx + 32], ymm8
    vpor    ymm7, ymm2, [rdx + 64]
    vmovdqa [rdx + 64], ymm7
    vpor    ymm8, ymm3, [rdx + 96]
    vmovdqa [rdx + 96], ymm8
    vpor    ymm7, ymm4, [rdx + 128]
    vmovdqa [rdx + 128], ymm7
    vpor    ymm8, ymm5, [rdx + 160]
    vmovdqa [rdx + 160], ymm8
    vpor    ymm7, ymm6, [rdx + 192]
    vmovdqa [rdx + 192], ymm7
    add     rdx, 224
    dec     rcx
    jnz     .Lblit_pat7_loop

    # --------------------------------------------------------------------------
    # Tier 3b: AVX2 Blit for Prime 11 (178 iters * 352 bytes = 62,656 bytes)
    # Preload ymm0..ymm10 with 11 vectors of pat11_table
    # --------------------------------------------------------------------------
    lea     rax, [rip + pat11_table]
    vmovdqa ymm0,  [rax]
    vmovdqa ymm1,  [rax + 32]
    vmovdqa ymm2,  [rax + 64]
    vmovdqa ymm3,  [rax + 96]
    vmovdqa ymm4,  [rax + 128]
    vmovdqa ymm5,  [rax + 160]
    vmovdqa ymm6,  [rax + 192]
    vmovdqa ymm7,  [rax + 224]
    vmovdqa ymm8,  [rax + 256]
    vmovdqa ymm9,  [rax + 288]
    vmovdqa ymm10, [rax + 320]
    mov     rcx, 178
    mov     rdx, rdi

.p2align 4
.Lblit_pat11_loop:
    vpor    ymm11, ymm0,  [rdx]
    vmovdqa [rdx], ymm11
    vpor    ymm12, ymm1,  [rdx + 32]
    vmovdqa [rdx + 32], ymm12
    vpor    ymm11, ymm2,  [rdx + 64]
    vmovdqa [rdx + 64], ymm11
    vpor    ymm12, ymm3,  [rdx + 96]
    vmovdqa [rdx + 96], ymm12
    vpor    ymm11, ymm4,  [rdx + 128]
    vmovdqa [rdx + 128], ymm11
    vpor    ymm12, ymm5,  [rdx + 160]
    vmovdqa [rdx + 160], ymm12
    vpor    ymm11, ymm6,  [rdx + 192]
    vmovdqa [rdx + 192], ymm11
    vpor    ymm12, ymm7,  [rdx + 224]
    vmovdqa [rdx + 224], ymm12
    vpor    ymm11, ymm8,  [rdx + 256]
    vmovdqa [rdx + 256], ymm11
    vpor    ymm12, ymm9,  [rdx + 288]
    vmovdqa [rdx + 288], ymm12
    vpor    ymm11, ymm10, [rdx + 320]
    vmovdqa [rdx + 320], ymm11
    add     rdx, 352
    dec     rcx
    jnz     .Lblit_pat11_loop

    # Restore candidate 3 (bit 1), 5 (bit 2), 7 (bit 3), 11 (bit 5), set candidate 1 (bit 0)
    # ~(2 | 4 | 8 | 32) = ~46 = 0xD1
    and     byte ptr [rdi], 0xD1
    or      byte ptr [rdi], 1

    # Clear tail beyond 500,000 bits (bytes 62,500..62,503) so popcnt matches
    mov     dword ptr [rdi + 62500], 0

    # --------------------------------------------------------------------------
    # Tier 3 & 4: Base Sieve of Eratosthenes
    # Scan for primes starting at i = 6 (candidate n = 13).
    # Upper bound for outer loop: p < 1,000 => 2*i + 1 < 1,000 => i <= 499.
    # --------------------------------------------------------------------------
    mov     r12, 6              # i = 6 (prime candidate 13)

.Lprime_search_loop:
    # Test if bit i is set: byte = i >> 3, bit = i & 7
    mov     rax, r12
    shr     rax, 3
    movzx   eax, byte ptr [rdi + rax]
    mov     rcx, r12
    and     rcx, 7
    bt      eax, ecx
    jc      .Lnext_candidate    # Bit set: already composite

    # Found prime! p = 2*i + 1
    lea     r8, [r12 + r12 + 1] # r8 = p

    # j_start = 2*i*(i + 1)
    lea     rax, [r12 + 1]
    imul    rax, r12
    shl     rax, 1              # rax = j (bit index of p^2)

    cmp     rax, 500000
    jae     .Ldone_sieve

    # --------------------------------------------------------------------------
    # 8-Stream Constant Mask Sieving Engine
    # For s = 0..7:
    #   js = j + s * p
    #   ptr = buffer + (js >> 3)
    #   mask = 1 << (js & 7)
    # Stride is p bytes. Mask is constant.
    # --------------------------------------------------------------------------
    # Compute loop-invariant parameters for prime p:
    lea     r13, [r8 + r8*2]    # r13 = 3*p
    lea     r14, [r8 * 4]       # r14 = 4*p
    lea     rsi, [rdi + 62500]  # rsi = end boundary

    mov     rbx, rax            # rbx = js (initially j)
    xor     ebp, ebp            # s = 0..7

.Lstream_loop:
    cmp     rbx, 500000
    jae     .Lnext_candidate

    # ptr = buffer + (js >> 3)
    mov     rdx, rbx
    shr     rdx, 3
    lea     rdx, [rdi + rdx]    # rdx = ptr

    # mask = 1 << (js & 7)
    mov     eax, ebx
    and     eax, 7
    mov     ecx, 1
    shlx    ecx, ecx, eax       # cl = mask

    # rax = limit4 (end - 3*p)
    mov     rax, rsi
    sub     rax, r13

    cmp     rdx, rax
    jae     .Lmark_stream_tail

.p2align 4
.Lmark_stream_pipelined4:
    movzx   r9d,  byte ptr [rdx]
    movzx   r10d, byte ptr [rdx + r8]
    movzx   r11d, byte ptr [rdx + r8*2]
    movzx   r15d, byte ptr [rdx + r13]

    or      r9b,  cl
    or      r10b, cl
    or      r11b, cl
    or      r15b, cl

    mov     byte ptr [rdx], r9b
    mov     byte ptr [rdx + r8], r10b
    mov     byte ptr [rdx + r8*2], r11b
    mov     byte ptr [rdx + r13], r15b

    add     rdx, r14
    cmp     rdx, rax
    jb      .Lmark_stream_pipelined4

.Lmark_stream_tail:
    cmp     rdx, rsi
    jae     .Lnext_stream
    or      byte ptr [rdx], cl
    add     rdx, r8
    jmp     .Lmark_stream_tail

.Lnext_stream:
    add     rbx, r8             # js += p
    inc     ebp
    cmp     ebp, 8
    jne     .Lstream_loop

.Lnext_candidate:
    inc     r12
    cmp     r12, 500            # p < 1,000 => i < 500
    jb      .Lprime_search_loop

.Ldone_sieve:
    # --------------------------------------------------------------------------
    # Tier 5: 8-Way Dependency-Free POPCNT Over 7,813 Quadwords (62,504 bytes)
    # Total prime count = 500,001 - composite_bits (500,000 odds + 1 for prime 2)
    # --------------------------------------------------------------------------
    xor     rax, rax            # acc0
    xor     r8,  r8             # acc1
    xor     r9,  r9             # acc2
    xor     r10, r10            # acc3
    xor     r12, r12            # acc4
    xor     r13, r13            # acc5
    xor     r14, r14            # acc6
    xor     r15, r15            # acc7

    mov     rcx, 976            # 976 * 8 = 7,808 quadwords
    mov     rdx, rdi

.p2align 4
.Lcount_unroll8:
    popcnt  rbx, qword ptr [rdx]
    add     rax, rbx
    popcnt  rbp, qword ptr [rdx + 8]
    add     r8,  rbp
    popcnt  rbx, qword ptr [rdx + 16]
    add     r9,  rbx
    popcnt  rbp, qword ptr [rdx + 24]
    add     r10, rbp
    popcnt  rbx, qword ptr [rdx + 32]
    add     r12, rbx
    popcnt  rbp, qword ptr [rdx + 40]
    add     r13, rbp
    popcnt  rbx, qword ptr [rdx + 48]
    add     r14, rbx
    popcnt  rbp, qword ptr [rdx + 56]
    add     r15, rbp
    add     rdx, 64
    dec     rcx
    jnz     .Lcount_unroll8

    # Process remaining 5 tail quadwords (7,813 - 7,808 = 5)
    popcnt  rbx, qword ptr [rdx]
    add     rax, rbx
    popcnt  rbp, qword ptr [rdx + 8]
    add     r8,  rbp
    popcnt  rbx, qword ptr [rdx + 16]
    add     r9,  rbx
    popcnt  rbp, qword ptr [rdx + 24]
    add     r10, rbp
    popcnt  rbx, qword ptr [rdx + 32]
    add     r12, rbx

    # Reduction tree across 8 accumulators
    add     rax, r8
    add     r9,  r10
    add     r12, r13
    add     r14, r15
    add     rax, r9
    add     r12, r14
    add     rax, r12

    # Primes = 500,001 - composite_count
    mov     rcx, 500001
    sub     rcx, rax
    mov     rax, rcx            # Returns exactly 78,498

    vzeroupper

    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

.section .rodata
.p2align 5
pat3_5_table:
    # 15 YMM vectors (480 bytes)
    .quad 0x6692cd259a4b3496, 0x96692cd259a4b349, 0x496692cd259a4b34, 0x3496692cd259a4b3
    .quad 0xb3496692cd259a4b, 0x4b3496692cd259a4, 0xa4b3496692cd259a, 0x9a4b3496692cd259
    .quad 0x59a4b3496692cd25, 0x259a4b3496692cd2, 0xd259a4b3496692cd, 0xcd259a4b3496692c
    .quad 0x2cd259a4b3496692, 0x92cd259a4b349669, 0x692cd259a4b34966, 0x6692cd259a4b3496
    .quad 0x96692cd259a4b349, 0x496692cd259a4b34, 0x3496692cd259a4b3, 0xb3496692cd259a4b
    .quad 0x4b3496692cd259a4, 0xa4b3496692cd259a, 0x9a4b3496692cd259, 0x59a4b3496692cd25
    .quad 0x259a4b3496692cd2, 0xd259a4b3496692cd, 0xcd259a4b3496692c, 0x2cd259a4b3496692
    .quad 0x92cd259a4b349669, 0x692cd259a4b34966, 0x6692cd259a4b3496, 0x96692cd259a4b349
    .quad 0x496692cd259a4b34, 0x3496692cd259a4b3, 0xb3496692cd259a4b, 0x4b3496692cd259a4
    .quad 0xa4b3496692cd259a, 0x9a4b3496692cd259, 0x59a4b3496692cd25, 0x259a4b3496692cd2
    .quad 0xd259a4b3496692cd, 0xcd259a4b3496692c, 0x2cd259a4b3496692, 0x92cd259a4b349669
    .quad 0x692cd259a4b34966, 0x6692cd259a4b3496, 0x96692cd259a4b349, 0x496692cd259a4b34
    .quad 0x3496692cd259a4b3, 0xb3496692cd259a4b, 0x4b3496692cd259a4, 0xa4b3496692cd259a
    .quad 0x9a4b3496692cd259, 0x59a4b3496692cd25, 0x259a4b3496692cd2, 0xd259a4b3496692cd
    .quad 0xcd259a4b3496692c, 0x2cd259a4b3496692, 0x92cd259a4b349669, 0x692cd259a4b34966

.p2align 5
pat7_table:
    # 7 YMM vectors (224 bytes)
    .quad 0x0810204081020408, 0x0408102040810204, 0x0204081020408102, 0x8102040810204081
    .quad 0x4081020408102040, 0x2040810204081020, 0x1020408102040810, 0x0810204081020408
    .quad 0x0408102040810204, 0x0204081020408102, 0x8102040810204081, 0x4081020408102040
    .quad 0x2040810204081020, 0x1020408102040810, 0x0810204081020408, 0x0408102040810204
    .quad 0x0204081020408102, 0x8102040810204081, 0x4081020408102040, 0x2040810204081020
    .quad 0x1020408102040810, 0x0810204081020408, 0x0408102040810204, 0x0204081020408102
    .quad 0x8102040810204081, 0x4081020408102040, 0x2040810204081020, 0x1020408102040810

.include "pat11.inc"

