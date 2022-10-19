	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 15, 4	sdk_version 10, 15, 4
	.section	__TEXT,__const
	.p2align	5               ## -- Begin function main
LCPI0_0:
	.long	1                       ## 0x1
	.long	2                       ## 0x2
	.long	3                       ## 0x3
	.long	4                       ## 0x4
	.long	5                       ## 0x5
	.long	6                       ## 0x6
	.long	7                       ## 0x7
	.long	8                       ## 0x8
	.section	__TEXT,__literal4,4byte_literals
	.p2align	2
LCPI0_1:
	.long	8                       ## 0x8
LCPI0_2:
	.long	16                      ## 0x10
LCPI0_3:
	.long	24                      ## 0x18
LCPI0_4:
	.long	31                      ## 0x1f
LCPI0_5:
	.long	1                       ## 0x1
LCPI0_6:
	.long	32                      ## 0x20
	.section	__TEXT,__literal8,8byte_literals
	.p2align	3
LCPI0_7:
	.quad	4472406533629990549     ## double 1.0000000000000001E-9
LCPI0_8:
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
	subq	$320, %rsp              ## imm = 0x140
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movl	$1000000, 28(%rsp)      ## imm = 0xF4240
	cmpl	$1, %edi
	jle	LBB0_1
## %bb.5:
	movq	8(%rsi), %rdi
	leaq	L_.str(%rip), %rsi
	leaq	28(%rsp), %rdx
	xorl	%eax, %eax
	callq	_sscanf
LBB0_1:
	xorl	%ebx, %ebx
	leaq	l___const.main.known_sieveSize(%rip), %r15
	movl	$1, %eax
	vmovd	%eax, %xmm0
	vmovdqa	%ymm0, 160(%rsp)        ## 32-byte Spill
	vpbroadcastd	LCPI0_1(%rip), %ymm0 ## ymm0 = [8,8,8,8,8,8,8,8]
	vpbroadcastd	LCPI0_2(%rip), %ymm1 ## ymm1 = [16,16,16,16,16,16,16,16]
	vpbroadcastd	LCPI0_3(%rip), %ymm2 ## ymm2 = [24,24,24,24,24,24,24,24]
	vpbroadcastd	LCPI0_4(%rip), %ymm3 ## ymm3 = [31,31,31,31,31,31,31,31]
	vpbroadcastd	LCPI0_5(%rip), %ymm4 ## ymm4 = [1,1,1,1,1,1,1,1]
	vpbroadcastd	LCPI0_6(%rip), %ymm5 ## ymm5 = [32,32,32,32,32,32,32,32]
	xorl	%r13d, %r13d
	vmovdqa	%ymm0, 64(%rsp)         ## 32-byte Spill
	vmovdqa	%ymm1, 256(%rsp)        ## 32-byte Spill
	vmovdqa	%ymm2, 224(%rsp)        ## 32-byte Spill
	vmovdqa	%ymm3, 128(%rsp)        ## 32-byte Spill
	vmovdqa	%ymm4, 96(%rsp)         ## 32-byte Spill
	vmovdqa	%ymm5, 192(%rsp)        ## 32-byte Spill
	movl	28(%rsp), %edi
	cmpl	(%r15,%rbx,4), %edi
	jne	LBB0_12
	jmp	LBB0_3
	.p2align	4, 0x90
LBB0_4:
	movq	(%r14), %rdi
	movl	$1, %r12d
LBB0_11:
	vzeroupper
	callq	_free
	movq	%r14, %rdi
	callq	_free
	xorl	%eax, %eax
	leaq	l___const.main.known_PrimeCount(%rip), %rcx
	cmpl	(%rcx,%rbx,4), %r12d
	sete	%al
	addl	%eax, %r13d
LBB0_12:                                ## =>This Inner Loop Header: Depth=1
	incq	%rbx
	cmpq	$9, %rbx
	je	LBB0_13
## %bb.2:                               ##   in Loop: Header=BB0_12 Depth=1
	movl	28(%rsp), %edi
	cmpl	(%r15,%rbx,4), %edi
	jne	LBB0_12
LBB0_3:
	vzeroupper
	callq	_run_sieve
	movq	%rax, %r14
	movl	8(%rax), %eax
	cmpl	$1, %eax
	jbe	LBB0_4
## %bb.6:
	movq	(%r14), %rdi
	leal	-1(%rax), %r8d
	movl	$1, %ecx
	movl	$1, %r12d
	cmpl	$32, %r8d
	jb	LBB0_10
## %bb.7:
	movl	%r8d, %esi
	andl	$-32, %esi
	leal	1(%rsi), %ecx
	vpxor	%xmm0, %xmm0, %xmm0
	movl	%esi, %edx
	vmovdqa	LCPI0_0(%rip), %ymm4    ## ymm4 = [1,2,3,4,5,6,7,8]
	vmovdqa	160(%rsp), %ymm1        ## 32-byte Reload
	vpxor	%xmm2, %xmm2, %xmm2
	vpxor	%xmm3, %xmm3, %xmm3
	vmovdqa	128(%rsp), %ymm15       ## 32-byte Reload
	vmovdqa	96(%rsp), %ymm14        ## 32-byte Reload
	.p2align	4, 0x90
LBB0_8:                                 ## =>This Inner Loop Header: Depth=1
	vpaddd	64(%rsp), %ymm4, %ymm5  ## 32-byte Folded Reload
	vpaddd	256(%rsp), %ymm4, %ymm6 ## 32-byte Folded Reload
	vpaddd	224(%rsp), %ymm4, %ymm7 ## 32-byte Folded Reload
	vpsrld	$5, %ymm4, %ymm8
	vpsrld	$5, %ymm5, %ymm9
	vpsrld	$5, %ymm6, %ymm10
	vpsrld	$5, %ymm7, %ymm11
	vpcmpeqd	%ymm12, %ymm12, %ymm12
	vpgatherdd	%ymm12, (%rdi,%ymm8,4), %ymm13
	vpcmpeqd	%ymm8, %ymm8, %ymm8
	vpgatherdd	%ymm8, (%rdi,%ymm9,4), %ymm12
	vpcmpeqd	%ymm8, %ymm8, %ymm8
	vpgatherdd	%ymm8, (%rdi,%ymm10,4), %ymm9
	vpcmpeqd	%ymm8, %ymm8, %ymm8
	vpgatherdd	%ymm8, (%rdi,%ymm11,4), %ymm10
	vpand	%ymm15, %ymm4, %ymm8
	vpsllvd	%ymm8, %ymm14, %ymm8
	vpand	%ymm8, %ymm13, %ymm8
	vpand	%ymm15, %ymm5, %ymm5
	vpsllvd	%ymm5, %ymm14, %ymm5
	vpand	%ymm5, %ymm12, %ymm5
	vpand	%ymm15, %ymm6, %ymm6
	vpsllvd	%ymm6, %ymm14, %ymm6
	vpand	%ymm6, %ymm9, %ymm6
	vpand	%ymm15, %ymm7, %ymm7
	vpsllvd	%ymm7, %ymm14, %ymm7
	vpand	%ymm7, %ymm10, %ymm7
	vpxor	%xmm9, %xmm9, %xmm9
	vpcmpeqd	%ymm9, %ymm8, %ymm8
	vpsubd	%ymm8, %ymm1, %ymm1
	vpcmpeqd	%ymm9, %ymm5, %ymm5
	vpsubd	%ymm5, %ymm0, %ymm0
	vpcmpeqd	%ymm9, %ymm6, %ymm5
	vpsubd	%ymm5, %ymm2, %ymm2
	vpcmpeqd	%ymm9, %ymm7, %ymm5
	vpsubd	%ymm5, %ymm3, %ymm3
	vpaddd	192(%rsp), %ymm4, %ymm4 ## 32-byte Folded Reload
	addl	$-32, %edx
	jne	LBB0_8
## %bb.9:
	vpaddd	%ymm1, %ymm0, %ymm0
	vpaddd	%ymm0, %ymm2, %ymm0
	vpaddd	%ymm0, %ymm3, %ymm0
	vextracti128	$1, %ymm0, %xmm1
	vpaddd	%xmm1, %xmm0, %xmm0
	vpshufd	$78, %xmm0, %xmm1       ## xmm1 = xmm0[2,3,0,1]
	vpaddd	%xmm1, %xmm0, %xmm0
	vpshufd	$229, %xmm0, %xmm1      ## xmm1 = xmm0[1,1,2,3]
	vpaddd	%xmm1, %xmm0, %xmm0
	vmovd	%xmm0, %r12d
	cmpl	%esi, %r8d
	je	LBB0_11
	.p2align	4, 0x90
LBB0_10:                                ## =>This Inner Loop Header: Depth=1
	movl	%ecx, %edx
	shrl	$5, %edx
	movl	(%rdi,%rdx,4), %edx
	xorl	%esi, %esi
	btl	%ecx, %edx
	setae	%sil
	addl	%esi, %r12d
	incl	%ecx
	cmpl	%ecx, %eax
	jne	LBB0_10
	jmp	LBB0_11
LBB0_13:
	cmpl	$1, %r13d
	jne	LBB0_17
## %bb.14:
	leaq	48(%rsp), %rsi
	movl	$6, %edi
	vzeroupper
	callq	_clock_gettime
	xorl	%ebx, %ebx
	leaq	32(%rsp), %r14
	.p2align	4, 0x90
LBB0_15:                                ## =>This Inner Loop Header: Depth=1
	movl	28(%rsp), %edi
	callq	_run_sieve
	movq	%rax, %r15
	movl	$6, %edi
	movq	%r14, %rsi
	callq	_clock_gettime
	incl	%ebx
	vxorps	%xmm15, %xmm15, %xmm15
	vcvtsi2sdq	32(%rsp), %xmm15, %xmm0
	vxorps	%xmm15, %xmm15, %xmm15
	vcvtsi2sdq	40(%rsp), %xmm15, %xmm1
	vxorps	%xmm15, %xmm15, %xmm15
	vcvtsi2sdq	48(%rsp), %xmm15, %xmm2
	vsubsd	%xmm2, %xmm0, %xmm0
	vxorps	%xmm15, %xmm15, %xmm15
	vcvtsi2sdq	56(%rsp), %xmm15, %xmm2
	vsubsd	%xmm2, %xmm1, %xmm1
	vfmadd132sd	LCPI0_7(%rip), %xmm0, %xmm1 ## xmm1 = (xmm1 * mem) + xmm0
	vmovsd	%xmm1, 64(%rsp)         ## 8-byte Spill
	movq	(%r15), %rdi
	callq	_free
	movq	%r15, %rdi
	callq	_free
	vmovq	64(%rsp), %xmm0         ## 8-byte Folded Reload
                                        ## xmm0 = mem[0],zero
	vucomisd	LCPI0_8(%rip), %xmm0
	jbe	LBB0_15
## %bb.16:
	leaq	L_.str.1(%rip), %rdi
	movl	%ebx, %esi
	movb	$1, %al
	callq	_printf
LBB0_17:
	xorl	%eax, %eax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	vzeroupper
	retq
	.cfi_endproc
                                        ## -- End function
	.p2align	4, 0x90         ## -- Begin function run_sieve
_run_sieve:                             ## @run_sieve
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
	movl	%edi, %r14d
	movl	$16, %edi
	callq	_malloc
	movq	%rax, %r15
	shrl	%r14d
	movl	%r14d, %r12d
	andl	$-32, %r12d
	addl	$32, %r12d
	movl	%r14d, 8(%rax)
	movq	%r12, %rdi
	callq	_malloc
	movq	%rax, %rbx
	movq	%r15, -56(%rbp)         ## 8-byte Spill
	movq	%rax, (%r15)
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	___bzero
	testl	%r14d, %r14d
	je	LBB1_35
## %bb.1:
	movl	$1, %r15d
	movl	$192, %r9d
	movl	$96, %esi
	movl	$1, %edx
	movl	%r14d, -44(%rbp)        ## 4-byte Spill
	jmp	LBB1_2
	.p2align	4, 0x90
LBB1_8:                                 ##   in Loop: Header=BB1_2 Depth=1
	movl	%esi, %r8d
LBB1_34:                                ##   in Loop: Header=BB1_2 Depth=1
	movl	%r8d, %esi
	cmpl	%r14d, %edx
	ja	LBB1_35
LBB1_2:                                 ## =>This Loop Header: Depth=1
                                        ##     Child Loop BB1_4 Depth 2
                                        ##     Child Loop BB1_6 Depth 2
                                        ##     Child Loop BB1_17 Depth 2
                                        ##     Child Loop BB1_18 Depth 2
                                        ##       Child Loop BB1_20 Depth 3
                                        ##       Child Loop BB1_23 Depth 3
                                        ##       Child Loop BB1_26 Depth 3
                                        ##     Child Loop BB1_31 Depth 2
                                        ##     Child Loop BB1_32 Depth 2
	leal	(%rdx,%rdx), %eax
	imull	%edx, %eax
	leal	(%rax,%rdx,2), %eax
	cmpl	%r9d, %eax
	ja	LBB1_35
## %bb.3:                               ##   in Loop: Header=BB1_2 Depth=1
	leal	1(%rdx,%rdx), %r8d
	jae	LBB1_5
	.p2align	4, 0x90
LBB1_4:                                 ##   Parent Loop BB1_2 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	shlxl	%eax, %r15d, %ecx
	movl	%eax, %edi
	shrl	$5, %edi
	orl	%ecx, (%rbx,%rdi,4)
	addl	%r8d, %eax
	cmpl	%r9d, %eax
	jb	LBB1_4
LBB1_5:                                 ##   in Loop: Header=BB1_2 Depth=1
	imull	%esi, %r8d
	leal	(%rsi,%rsi), %eax
	.p2align	4, 0x90
LBB1_6:                                 ##   Parent Loop BB1_2 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	addl	%eax, %r8d
	incl	%edx
	movl	%edx, %ecx
	shrl	$5, %ecx
	movl	(%rbx,%rcx,4), %ecx
	btl	%edx, %ecx
	jb	LBB1_6
## %bb.7:                               ##   in Loop: Header=BB1_2 Depth=1
	cmpl	%r14d, %r9d
	jae	LBB1_8
## %bb.9:                               ##   in Loop: Header=BB1_2 Depth=1
	leal	(%r8,%r8), %r9d
	leal	(,%r8,4), %eax
	cmpl	%r14d, %eax
	cmoval	%r14d, %r9d
	shrl	$5, %esi
	je	LBB1_34
## %bb.10:                              ##   in Loop: Header=BB1_2 Depth=1
	movl	%r9d, %ecx
	shrl	$5, %ecx
	movl	%esi, %r11d
	movl	%ecx, %r10d
	leaq	-1(%r11), %rax
	movl	%r11d, %r12d
	andl	$3, %r12d
	cmpq	$3, %rax
	jae	LBB1_14
## %bb.11:                              ##   in Loop: Header=BB1_2 Depth=1
	xorl	%r14d, %r14d
	jmp	LBB1_12
LBB1_14:                                ##   in Loop: Header=BB1_2 Depth=1
	movq	%r11, %r13
	subq	%r12, %r13
	xorl	%r14d, %r14d
	leaq	(%r14,%r11), %rax
	cmpq	%r10, %rax
	jae	LBB1_18
	jmp	LBB1_16
	.p2align	4, 0x90
LBB1_27:                                ##   in Loop: Header=BB1_18 Depth=2
	addq	$4, %r14
	addq	$-4, %r13
	je	LBB1_12
## %bb.15:                              ##   in Loop: Header=BB1_18 Depth=2
	leaq	(%r14,%r11), %rax
	cmpq	%r10, %rax
	jae	LBB1_18
LBB1_16:                                ##   in Loop: Header=BB1_2 Depth=1
	movl	(%rbx,%rax,4), %r15d
	.p2align	4, 0x90
LBB1_17:                                ##   Parent Loop BB1_2 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	addl	%esi, %eax
	movl	%r15d, (%rbx,%rax,4)
	cmpl	%ecx, %eax
	jb	LBB1_17
LBB1_18:                                ##   Parent Loop BB1_2 Depth=1
                                        ## =>  This Loop Header: Depth=2
                                        ##       Child Loop BB1_20 Depth 3
                                        ##       Child Loop BB1_23 Depth 3
                                        ##       Child Loop BB1_26 Depth 3
	movq	%r14, %rax
	orq	$1, %rax
	addq	%r11, %rax
	cmpq	%r10, %rax
	jae	LBB1_21
## %bb.19:                              ##   in Loop: Header=BB1_18 Depth=2
	movl	(%rbx,%rax,4), %edi
	.p2align	4, 0x90
LBB1_20:                                ##   Parent Loop BB1_2 Depth=1
                                        ##     Parent Loop BB1_18 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	addl	%esi, %eax
	movl	%edi, (%rbx,%rax,4)
	cmpl	%ecx, %eax
	jb	LBB1_20
LBB1_21:                                ##   in Loop: Header=BB1_18 Depth=2
	movq	%r14, %rax
	orq	$2, %rax
	addq	%r11, %rax
	cmpq	%r10, %rax
	jae	LBB1_24
## %bb.22:                              ##   in Loop: Header=BB1_18 Depth=2
	movl	(%rbx,%rax,4), %edi
	.p2align	4, 0x90
LBB1_23:                                ##   Parent Loop BB1_2 Depth=1
                                        ##     Parent Loop BB1_18 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	addl	%esi, %eax
	movl	%edi, (%rbx,%rax,4)
	cmpl	%ecx, %eax
	jb	LBB1_23
LBB1_24:                                ##   in Loop: Header=BB1_18 Depth=2
	movq	%r14, %rax
	orq	$3, %rax
	addq	%r11, %rax
	cmpq	%r10, %rax
	jae	LBB1_27
## %bb.25:                              ##   in Loop: Header=BB1_18 Depth=2
	movl	(%rbx,%rax,4), %edi
	.p2align	4, 0x90
LBB1_26:                                ##   Parent Loop BB1_2 Depth=1
                                        ##     Parent Loop BB1_18 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	addl	%esi, %eax
	movl	%edi, (%rbx,%rax,4)
	cmpl	%ecx, %eax
	jb	LBB1_26
	jmp	LBB1_27
	.p2align	4, 0x90
LBB1_12:                                ##   in Loop: Header=BB1_2 Depth=1
	testq	%r12, %r12
	je	LBB1_13
## %bb.28:                              ##   in Loop: Header=BB1_2 Depth=1
	movl	$1, %r15d
	leaq	(%r14,%r11), %rax
	cmpq	%r10, %rax
	jb	LBB1_30
	.p2align	4, 0x90
LBB1_32:                                ##   Parent Loop BB1_2 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	incq	%r14
	decq	%r12
	je	LBB1_33
## %bb.29:                              ##   in Loop: Header=BB1_32 Depth=2
	leaq	(%r14,%r11), %rax
	cmpq	%r10, %rax
	jae	LBB1_32
LBB1_30:                                ##   in Loop: Header=BB1_2 Depth=1
	movl	(%rbx,%rax,4), %edi
	.p2align	4, 0x90
LBB1_31:                                ##   Parent Loop BB1_2 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	addl	%esi, %eax
	movl	%edi, (%rbx,%rax,4)
	cmpl	%ecx, %eax
	jb	LBB1_31
	jmp	LBB1_32
LBB1_33:                                ##   in Loop: Header=BB1_2 Depth=1
	movl	-44(%rbp), %r14d        ## 4-byte Reload
	movl	%r8d, %esi
	cmpl	%r14d, %edx
	jbe	LBB1_2
	jmp	LBB1_35
LBB1_13:                                ##   in Loop: Header=BB1_2 Depth=1
	movl	-44(%rbp), %r14d        ## 4-byte Reload
	movl	$1, %r15d
	movl	%r8d, %esi
	cmpl	%r14d, %edx
	jbe	LBB1_2
LBB1_35:
	movq	-56(%rbp), %rax         ## 8-byte Reload
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
	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"%u"

	.section	__TEXT,__const
	.p2align	4               ## @__const.main.known_sieveSize
l___const.main.known_sieveSize:
	.long	10                      ## 0xa
	.long	100                     ## 0x64
	.long	1000                    ## 0x3e8
	.long	10000                   ## 0x2710
	.long	100000                  ## 0x186a0
	.long	1000000                 ## 0xf4240
	.long	10000000                ## 0x989680
	.long	100000000               ## 0x5f5e100
	.long	1000000000              ## 0x3b9aca00

	.p2align	4               ## @__const.main.known_PrimeCount
l___const.main.known_PrimeCount:
	.long	4                       ## 0x4
	.long	25                      ## 0x19
	.long	168                     ## 0xa8
	.long	1229                    ## 0x4cd
	.long	9592                    ## 0x2578
	.long	78498                   ## 0x132a2
	.long	664579                  ## 0xa2403
	.long	5761455                 ## 0x57e9af
	.long	50847534                ## 0x307df2e

	.section	__TEXT,__cstring,cstring_literals
L_.str.1:                               ## @.str.1
	.asciz	"rogiervandam;%d;%f;1;algorithm=other,faithful=yes,bits=1\n"


.subsections_via_symbols
