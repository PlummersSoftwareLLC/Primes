	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 15, 4	sdk_version 10, 15, 4
	.section	__TEXT,__const
	.p2align	5               ## -- Begin function make_prime_roots_db
LCPI0_0:
	.quad	20                      ## 0x14
	.quad	25                      ## 0x19
	.quad	30                      ## 0x1e
	.quad	35                      ## 0x23
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_make_prime_roots_db
	.p2align	4, 0x90
_make_prime_roots_db:                   ## @make_prime_roots_db
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movslq	_prime_roots(%rip), %rax
	movslq	_prime_roots+4(%rip), %rcx
	shlq	$5, %rcx
	movslq	_prime_roots+8(%rip), %rdx
	shlq	$10, %rdx
	movslq	_prime_roots+12(%rip), %rsi
	shlq	$15, %rsi
	orq	%rdx, %rsi
	orq	%rcx, %rsi
	vpmovzxdq	_prime_roots+16(%rip), %ymm0 ## ymm0 = mem[0],zero,mem[1],zero,mem[2],zero,mem[3],zero
	orq	%rax, %rsi
	vpsllq	$32, %ymm0, %ymm1
	vpsrad	$31, %ymm1, %ymm1
	vpblendd	$42, %ymm1, %ymm0, %ymm0 ## ymm0 = ymm0[0],ymm1[1],ymm0[2],ymm1[3],ymm0[4],ymm1[5],ymm0[6,7]
	vpsllvq	LCPI0_0(%rip), %ymm0, %ymm0
	vextracti128	$1, %ymm0, %xmm1
	vpor	%xmm1, %xmm0, %xmm0
	vpshufd	$78, %xmm0, %xmm1       ## xmm1 = xmm0[2,3,0,1]
	vpor	%xmm1, %xmm0, %xmm0
	vmovq	%xmm0, %rax
	orq	%rsi, %rax
	popq	%rbp
	vzeroupper
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__const
	.p2align	5               ## -- Begin function make_lookup_root_db
LCPI1_0:
	.quad	16                      ## 0x10
	.quad	20                      ## 0x14
	.quad	24                      ## 0x18
	.quad	28                      ## 0x1c
LCPI1_1:
	.quad	48                      ## 0x30
	.quad	52                      ## 0x34
	.quad	56                      ## 0x38
	.quad	60                      ## 0x3c
LCPI1_2:
	.quad	32                      ## 0x20
	.quad	36                      ## 0x24
	.quad	40                      ## 0x28
	.quad	44                      ## 0x2c
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_make_lookup_root_db
	.p2align	4, 0x90
_make_lookup_root_db:                   ## @make_lookup_root_db
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movslq	_lookup_roots(%rip), %rax
	movslq	_lookup_roots+4(%rip), %rcx
	shlq	$4, %rcx
	orq	%rax, %rcx
	movslq	_lookup_roots+8(%rip), %rdx
	shlq	$8, %rdx
	movslq	_lookup_roots+12(%rip), %rsi
	vpmovsxdq	_lookup_roots+16(%rip), %ymm0
	vpsllvq	LCPI1_0(%rip), %ymm0, %ymm0
	vpmovzxdq	_lookup_roots+32(%rip), %ymm1 ## ymm1 = mem[0],zero,mem[1],zero,mem[2],zero,mem[3],zero
	vpmovzxdq	_lookup_roots+48(%rip), %ymm2 ## ymm2 = mem[0],zero,mem[1],zero,mem[2],zero,mem[3],zero
	vpsllvq	LCPI1_1(%rip), %ymm2, %ymm2
	shlq	$12, %rsi
	vpsllvq	LCPI1_2(%rip), %ymm1, %ymm1
	vpor	%ymm2, %ymm1, %ymm1
	vextracti128	$1, %ymm1, %xmm2
	vpor	%xmm2, %xmm1, %xmm1
	vpshufd	$78, %xmm1, %xmm2       ## xmm2 = xmm1[2,3,0,1]
	vpor	%xmm2, %xmm1, %xmm1
	vmovq	%xmm1, %rdi
	vextracti128	$1, %ymm0, %xmm1
	vpor	%xmm1, %xmm0, %xmm0
	vpshufd	$78, %xmm0, %xmm1       ## xmm1 = xmm0[2,3,0,1]
	vpor	%xmm1, %xmm0, %xmm0
	vmovq	%xmm0, %rax
	orq	%rsi, %rax
	orq	%rdx, %rax
	orq	%rdi, %rax
	orq	%rcx, %rax
	popq	%rbp
	vzeroupper
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_create_sieve           ## -- Begin function create_sieve
	.p2align	4, 0x90
_create_sieve:                          ## @create_sieve
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	pushq	%rax
	.cfi_offset %rbx, -24
	movl	%edi, %ebx
	leal	(,%rbx,8), %eax
	cltq
	imulq	$-2004318071, %rax, %rax ## imm = 0x88888889
	shrq	$32, %rax
	leal	(%rax,%rbx,8), %eax
	movl	%eax, %ecx
	shrl	$31, %ecx
	sarl	$3, %eax
	addl	%ecx, %eax
	addl	%eax, %eax
	movslq	%eax, %rdi
	movl	$1, %esi
	callq	_calloc
	movl	%ebx, %edx
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_delete_sieve           ## -- Begin function delete_sieve
	.p2align	4, 0x90
_delete_sieve:                          ## @delete_sieve
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	popq	%rbp
	jmp	_free                   ## TAILCALL
	.cfi_endproc
                                        ## -- End function
	.globl	_run_sieve              ## -- Begin function run_sieve
	.p2align	4, 0x90
_run_sieve:                             ## @run_sieve
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_count_primes           ## -- Begin function count_primes
	.p2align	4, 0x90
_count_primes:                          ## @count_primes
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movl	%esi, %r12d
	movq	%rdi, -56(%rbp)         ## 8-byte Spill
	leaq	L_.str(%rip), %rdi
	xorl	%eax, %eax
	callq	_printf
	movl	$3, %r15d
	cmpl	$8, %r12d
	jl	LBB5_7
## %bb.1:
	movl	$3, %r15d
	movl	$7, %ebx
	movl	$6, %r13d
	movl	$2290649225, %r14d      ## imm = 0x88888889
	leaq	_lookup_roots(%rip), %rsi
	jmp	LBB5_2
	.p2align	4, 0x90
LBB5_6:                                 ##   in Loop: Header=BB5_2 Depth=1
	addl	$2, %ebx
	addl	$2, %r13d
	cmpl	%r12d, %ebx
	jge	LBB5_7
LBB5_2:                                 ## =>This Inner Loop Header: Depth=1
	movl	%r13d, %eax
	imulq	%r14, %rax
	shrq	$36, %rax
	imull	$-30, %eax, %eax
	addl	%ebx, %eax
	shrl	%eax
	movl	(%rsi,%rax,4), %eax
	cmpl	$8, %eax
	je	LBB5_6
## %bb.3:                               ##   in Loop: Header=BB5_2 Depth=1
	movl	%ebx, %ecx
	imulq	%r14, %rcx
	shrq	$36, %rcx
	movl	%ecx, %ecx
	movq	-56(%rbp), %rdx         ## 8-byte Reload
	movzbl	(%rdx,%rcx), %ecx
	btl	%eax, %ecx
	jb	LBB5_6
## %bb.4:                               ##   in Loop: Header=BB5_2 Depth=1
	incl	%r15d
	cmpl	$49, %r15d
	jg	LBB5_6
## %bb.5:                               ##   in Loop: Header=BB5_2 Depth=1
	leaq	L_.str.1(%rip), %rdi
	movl	%r15d, -44(%rbp)        ## 4-byte Spill
	movq	%rsi, %r15
	movl	%ebx, %esi
	xorl	%eax, %eax
	callq	_printf
	movq	%r15, %rsi
	movl	-44(%rbp), %r15d        ## 4-byte Reload
	jmp	LBB5_6
LBB5_7:
	movl	$10, %edi
	callq	_putchar
	movl	%r15d, %eax
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__const
	.p2align	5               ## -- Begin function main
LCPI6_0:
	.quad	5                       ## 0x5
	.quad	10                      ## 0xa
	.quad	15                      ## 0xf
	.quad	20                      ## 0x14
LCPI6_1:
	.quad	20                      ## 0x14
	.quad	24                      ## 0x18
	.quad	28                      ## 0x1c
	.quad	32                      ## 0x20
LCPI6_2:
	.quad	4                       ## 0x4
	.quad	8                       ## 0x8
	.quad	12                      ## 0xc
	.quad	16                      ## 0x10
LCPI6_3:
	.quad	36                      ## 0x24
	.quad	40                      ## 0x28
	.quad	44                      ## 0x2c
	.quad	48                      ## 0x30
	.section	__TEXT,__literal8,8byte_literals
	.p2align	3
LCPI6_4:
	.quad	4472406533629990549     ## double 1.0000000000000001E-9
LCPI6_5:
	.quad	4617315517961601024     ## double 5
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	andq	$-32, %rsp
	subq	$192, %rsp
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movq	%rsi, 16(%rsp)          ## 8-byte Spill
	movl	%edi, 8(%rsp)           ## 4-byte Spill
	leaq	L_.str.3(%rip), %rdi
	movl	$1, %esi
	xorl	%eax, %eax
	callq	_printf
	movslq	_prime_roots(%rip), %rax
	vpmovsxdq	_prime_roots+4(%rip), %ymm0
	vpsllvq	LCPI6_0(%rip), %ymm0, %ymm0
	vmovdqa	%ymm0, 64(%rsp)         ## 32-byte Spill
	movslq	_prime_roots+20(%rip), %rcx
	shlq	$25, %rcx
	movslq	_prime_roots+24(%rip), %rdx
	shlq	$30, %rdx
	orq	%rcx, %rdx
	movl	_prime_roots+28(%rip), %r13d
	shlq	$35, %r13
	orq	%rdx, %r13
	vpmovsxdq	_lookup_roots+4(%rip), %ymm0
	vpmovsxdq	_lookup_roots+20(%rip), %ymm1
	vpmovzxdq	_lookup_roots+20(%rip), %ymm2 ## ymm2 = mem[0],zero,mem[1],zero,mem[2],zero,mem[3],zero
	orq	%rax, %r13
	vpblendd	$192, %ymm2, %ymm1, %ymm1 ## ymm1 = ymm1[0,1,2,3,4,5],ymm2[6,7]
	vpsllvq	LCPI6_1(%rip), %ymm1, %ymm1
	vpsllvq	LCPI6_2(%rip), %ymm0, %ymm0
	movslq	_lookup_roots(%rip), %rcx
	movq	%rcx, 24(%rsp)          ## 8-byte Spill
	vpor	%ymm1, %ymm0, %ymm0
	vmovdqa	%ymm0, 96(%rsp)         ## 32-byte Spill
	vpmovzxdq	_lookup_roots+36(%rip), %ymm0 ## ymm0 = mem[0],zero,mem[1],zero,mem[2],zero,mem[3],zero
	vpsllvq	LCPI6_3(%rip), %ymm0, %ymm0
	vmovdqa	%ymm0, 128(%rsp)        ## 32-byte Spill
	movl	_lookup_roots+52(%rip), %r14d
	shlq	$52, %r14
	movl	_lookup_roots+56(%rip), %r15d
	shlq	$56, %r15
	movl	_lookup_roots+60(%rip), %r12d
	shlq	$60, %r12
	leaq	L_.str.4(%rip), %rbx
	movq	%rbx, %rdi
	xorl	%esi, %esi
	movl	%ecx, %edx
                                        ## kill: def $ecx killed $ecx killed $rcx
	xorl	%eax, %eax
	vzeroupper
	callq	_printf
	movl	_lookup_roots+4(%rip), %ecx
	movq	%rbx, %rdi
	movl	$1, %esi
	movl	%ecx, %edx
	xorl	%eax, %eax
	callq	_printf
	movl	_lookup_roots+8(%rip), %ecx
	movq	%rbx, %rdi
	movl	$2, %esi
	movl	%ecx, %edx
	xorl	%eax, %eax
	callq	_printf
	movl	_lookup_roots+12(%rip), %ecx
	movq	%rbx, %rdi
	movl	$3, %esi
	movl	%ecx, %edx
	xorl	%eax, %eax
	callq	_printf
	movl	_lookup_roots+16(%rip), %ecx
	movl	$4, 4(%rsp)             ## 4-byte Folded Spill
	movq	%rbx, %rdi
	movl	$4, %esi
	movl	%ecx, %edx
	xorl	%eax, %eax
	callq	_printf
	movl	_lookup_roots+20(%rip), %ecx
	movq	%rbx, %rdi
	movl	$5, %esi
	movl	%ecx, %edx
	xorl	%eax, %eax
	callq	_printf
	movl	_lookup_roots+24(%rip), %ecx
	movq	%rbx, %rdi
	movl	$6, %esi
	movl	%ecx, %edx
	xorl	%eax, %eax
	callq	_printf
	movl	_lookup_roots+28(%rip), %ecx
	movq	%rbx, %rdi
	movl	$7, %esi
	movl	%ecx, %edx
	xorl	%eax, %eax
	callq	_printf
	movl	_lookup_roots+32(%rip), %ecx
	movq	%rbx, %rdi
	movl	$8, %esi
	movl	%ecx, %edx
	xorl	%eax, %eax
	callq	_printf
	movl	_lookup_roots+36(%rip), %ecx
	movq	%rbx, %rdi
	movl	$9, %esi
	movl	%ecx, %edx
	xorl	%eax, %eax
	callq	_printf
	movl	_lookup_roots+40(%rip), %ecx
	movq	%rbx, %rdi
	movl	$10, %esi
	movl	%ecx, %edx
	xorl	%eax, %eax
	callq	_printf
	movl	_lookup_roots+44(%rip), %ecx
	movq	%rbx, %rdi
	movl	$11, %esi
	movl	%ecx, %edx
	xorl	%eax, %eax
	callq	_printf
	movl	_lookup_roots+48(%rip), %ecx
	movq	%rbx, %rdi
	movl	$12, %esi
	movl	%ecx, %edx
	xorl	%eax, %eax
	callq	_printf
	movl	_lookup_roots+52(%rip), %ecx
	movq	%rbx, %rdi
	movl	$13, %esi
	movl	%ecx, %edx
	xorl	%eax, %eax
	callq	_printf
	movl	_lookup_roots+56(%rip), %ecx
	movq	%rbx, %rdi
	movl	$14, %esi
	movl	%ecx, %edx
	xorl	%eax, %eax
	callq	_printf
	movl	_lookup_roots+60(%rip), %ecx
	movq	%rbx, %rdi
	movl	$15, %esi
	movl	%ecx, %edx
	xorl	%eax, %eax
	callq	_printf
	vmovdqa	64(%rsp), %ymm1         ## 32-byte Reload
	vextracti128	$1, %ymm1, %xmm0
	vpor	%xmm0, %xmm1, %xmm0
	vpshufd	$78, %xmm0, %xmm1       ## xmm1 = xmm0[2,3,0,1]
	vpor	%xmm1, %xmm0, %xmm0
	vmovq	%xmm0, %rsi
	orq	%r13, %rsi
	vmovdqa	96(%rsp), %ymm1         ## 32-byte Reload
	vextracti128	$1, %ymm1, %xmm0
	vpor	%xmm0, %xmm1, %xmm0
	vpshufd	$78, %xmm0, %xmm1       ## xmm1 = xmm0[2,3,0,1]
	vpor	%xmm1, %xmm0, %xmm0
	vmovq	%xmm0, %rax
	vmovdqa	128(%rsp), %ymm1        ## 32-byte Reload
	vextracti128	$1, %ymm1, %xmm0
	vpor	%xmm0, %xmm1, %xmm0
	vpshufd	$78, %xmm0, %xmm1       ## xmm1 = xmm0[2,3,0,1]
	vpor	%xmm1, %xmm0, %xmm0
	vmovq	%xmm0, %rdx
	orq	%r14, %rdx
	orq	%r15, %rdx
	orq	%r12, %rdx
	orq	24(%rsp), %rdx          ## 8-byte Folded Reload
	orq	%rax, %rdx
	leaq	L_.str.5(%rip), %rdi
	xorl	%eax, %eax
	vzeroupper
	callq	_printf
	movl	$1000000, 12(%rsp)      ## imm = 0xF4240
	cmpl	$2, 8(%rsp)             ## 4-byte Folded Reload
	jl	LBB6_12
## %bb.1:
	movq	16(%rsp), %rax          ## 8-byte Reload
	movq	8(%rax), %rdi
	leaq	L_.str.6(%rip), %rsi
	leaq	12(%rsp), %rdx
	xorl	%eax, %eax
	callq	_sscanf
	movl	12(%rsp), %eax
	cmpl	$99999, %eax            ## imm = 0x1869F
	jle	LBB6_2
## %bb.9:
	cmpl	$9999999, %eax          ## imm = 0x98967F
	jle	LBB6_10
## %bb.13:
	cmpl	$10000000, %eax         ## imm = 0x989680
	je	LBB6_19
## %bb.14:
	cmpl	$100000000, %eax        ## imm = 0x5F5E100
	je	LBB6_20
## %bb.15:
	cmpl	$1000000000, %eax       ## imm = 0x3B9ACA00
	jne	LBB6_21
## %bb.16:
	movl	$50847534, 4(%rsp)      ## 4-byte Folded Spill
                                        ## imm = 0x307DF2E
	jmp	LBB6_22
LBB6_2:
	cmpl	$999, %eax              ## imm = 0x3E7
	jg	LBB6_6
## %bb.3:
	cmpl	$10, %eax
	je	LBB6_22
## %bb.4:
	cmpl	$100, %eax
	jne	LBB6_21
## %bb.5:
	movl	$25, 4(%rsp)            ## 4-byte Folded Spill
	jmp	LBB6_22
LBB6_10:
	cmpl	$100000, %eax           ## imm = 0x186A0
	je	LBB6_18
## %bb.11:
	cmpl	$1000000, %eax          ## imm = 0xF4240
	jne	LBB6_21
LBB6_12:
	movl	$78498, 4(%rsp)         ## 4-byte Folded Spill
                                        ## imm = 0x132A2
LBB6_22:
	leaq	48(%rsp), %rsi
	movl	$6, %edi
	callq	_clock_gettime
	movl	$1, %r14d
	leaq	32(%rsp), %rbx
	.p2align	4, 0x90
LBB6_23:                                ## =>This Inner Loop Header: Depth=1
	movl	12(%rsp), %r13d
	leal	(,%r13,8), %eax
	cltq
	imulq	$-2004318071, %rax, %rax ## imm = 0x88888889
	shrq	$32, %rax
	leal	(%rax,%r13,8), %eax
	movl	%eax, %ecx
	shrl	$31, %ecx
	sarl	$3, %eax
	addl	%ecx, %eax
	addl	%eax, %eax
	movslq	%eax, %rdi
	movl	$1, %esi
	callq	_calloc
	movq	%rax, %r12
	movl	$6, %edi
	movq	%rbx, %rsi
	callq	_clock_gettime
	vcvtsi2sdq	32(%rsp), %xmm3, %xmm0
	vcvtsi2sdq	40(%rsp), %xmm3, %xmm1
	vcvtsi2sdq	48(%rsp), %xmm3, %xmm2
	vsubsd	%xmm2, %xmm0, %xmm0
	vcvtsi2sdq	56(%rsp), %xmm3, %xmm2
	vsubsd	%xmm2, %xmm1, %xmm1
	vfmadd132sd	LCPI6_4(%rip), %xmm0, %xmm1 ## xmm1 = (xmm1 * mem) + xmm0
	vucomisd	LCPI6_5(%rip), %xmm1
	jae	LBB6_24
## %bb.32:                              ##   in Loop: Header=BB6_23 Depth=1
	movq	%r12, %rdi
	callq	_free
	incl	%r14d
	jmp	LBB6_23
LBB6_24:
	vmovsd	%xmm1, 64(%rsp)         ## 8-byte Spill
	leaq	L_.str(%rip), %rdi
	xorl	%eax, %eax
	callq	_printf
	movl	$3, %edx
	cmpl	$8, %r13d
	jl	LBB6_31
## %bb.25:
	movl	$3, %edx
	movl	$7, %ebx
	movl	$6, %r15d
	movl	$2290649225, %esi       ## imm = 0x88888889
	leaq	_lookup_roots(%rip), %rdi
	jmp	LBB6_26
	.p2align	4, 0x90
LBB6_30:                                ##   in Loop: Header=BB6_26 Depth=1
	addl	$2, %ebx
	addl	$2, %r15d
	cmpl	%r13d, %ebx
	jge	LBB6_31
LBB6_26:                                ## =>This Inner Loop Header: Depth=1
	movl	%r15d, %eax
	imulq	%rsi, %rax
	shrq	$36, %rax
	imull	$-30, %eax, %eax
	addl	%ebx, %eax
	shrl	%eax
	movl	(%rdi,%rax,4), %eax
	cmpl	$8, %eax
	je	LBB6_30
## %bb.27:                              ##   in Loop: Header=BB6_26 Depth=1
	movl	%ebx, %ecx
	imulq	%rsi, %rcx
	shrq	$36, %rcx
	movl	%ecx, %ecx
	movzbl	(%r12,%rcx), %ecx
	btl	%eax, %ecx
	jb	LBB6_30
## %bb.28:                              ##   in Loop: Header=BB6_26 Depth=1
	incl	%edx
	cmpl	$49, %edx
	jg	LBB6_30
## %bb.29:                              ##   in Loop: Header=BB6_26 Depth=1
	leaq	L_.str.1(%rip), %rdi
	movl	%ebx, %esi
	xorl	%eax, %eax
	movl	%edx, 8(%rsp)           ## 4-byte Spill
	callq	_printf
	leaq	_lookup_roots(%rip), %rdi
	movl	$2290649225, %esi       ## imm = 0x88888889
	movl	8(%rsp), %edx           ## 4-byte Reload
	jmp	LBB6_30
LBB6_31:
	movl	$10, %edi
	movl	%edx, %ebx
	callq	_putchar
	leaq	L_.str.7(%rip), %rdi
	movl	%r14d, %esi
	vmovsd	64(%rsp), %xmm0         ## 8-byte Reload
                                        ## xmm0 = mem[0],zero
	movb	$1, %al
	callq	_printf
	xorl	%esi, %esi
	cmpl	4(%rsp), %ebx           ## 4-byte Folded Reload
	sete	%sil
	leaq	L_.str.8(%rip), %rdi
	movl	%ebx, %edx
	xorl	%eax, %eax
	callq	_printf
	xorl	%eax, %eax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
LBB6_6:
	cmpl	$1000, %eax             ## imm = 0x3E8
	je	LBB6_17
## %bb.7:
	cmpl	$10000, %eax            ## imm = 0x2710
	jne	LBB6_21
## %bb.8:
	movl	$1229, 4(%rsp)          ## 4-byte Folded Spill
                                        ## imm = 0x4CD
	jmp	LBB6_22
LBB6_19:
	movl	$664579, 4(%rsp)        ## 4-byte Folded Spill
                                        ## imm = 0xA2403
	jmp	LBB6_22
LBB6_20:
	movl	$5761455, 4(%rsp)       ## 4-byte Folded Spill
                                        ## imm = 0x57E9AF
	jmp	LBB6_22
LBB6_18:
	movl	$9592, 4(%rsp)          ## 4-byte Folded Spill
                                        ## imm = 0x2578
	jmp	LBB6_22
LBB6_17:
	movl	$168, 4(%rsp)           ## 4-byte Folded Spill
	jmp	LBB6_22
LBB6_21:
	movl	$-1, 4(%rsp)            ## 4-byte Folded Spill
	jmp	LBB6_22
	.cfi_endproc
                                        ## -- End function
	.section	__DATA,__data
	.globl	_prime_roots            ## @prime_roots
	.p2align	4
_prime_roots:
	.long	1                       ## 0x1
	.long	7                       ## 0x7
	.long	11                      ## 0xb
	.long	13                      ## 0xd
	.long	17                      ## 0x11
	.long	19                      ## 0x13
	.long	23                      ## 0x17
	.long	29                      ## 0x1d

	.globl	_lookup_roots           ## @lookup_roots
	.p2align	4
_lookup_roots:
	.long	0                       ## 0x0
	.long	8                       ## 0x8
	.long	8                       ## 0x8
	.long	1                       ## 0x1
	.long	8                       ## 0x8
	.long	2                       ## 0x2
	.long	3                       ## 0x3
	.long	8                       ## 0x8
	.long	4                       ## 0x4
	.long	5                       ## 0x5
	.long	8                       ## 0x8
	.long	6                       ## 0x6
	.long	8                       ## 0x8
	.long	8                       ## 0x8
	.long	7                       ## 0x7
	.space	60

	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"2,3,5,"

L_.str.1:                               ## @.str.1
	.asciz	"%d,"

L_.str.3:                               ## @.str.3
	.asciz	"sizeof %d\n"

L_.str.4:                               ## @.str.4
	.asciz	"pos %2d  Lookup %2d  db %d\n"

L_.str.5:                               ## @.str.5
	.asciz	"prime_roots_db %llu   lookup_root_db %d \n"

L_.str.6:                               ## @.str.6
	.asciz	"%d"

L_.str.7:                               ## @.str.7
	.asciz	"rogiervandam;%d;%f;1;algorithm=wheel,faithful=yes,bits=1\n"

L_.str.8:                               ## @.str.8
	.asciz	"valid=%d primes=%d\n"


.subsections_via_symbols
