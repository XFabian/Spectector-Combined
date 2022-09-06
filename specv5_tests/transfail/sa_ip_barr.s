	.file	"sa_ip_barr.c"
	.text
	.globl	secret
	.bss
	.align 8
	.type	secret, @object
	.size	secret, 8
secret:
	.zero	8
	.globl	real_secret
	.data
	.align 4
	.type	real_secret, @object
	.size	real_secret, 4
real_secret:
	.long	83
	.text
	.globl	in_place
	.type	in_place, @function
in_place:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
#APP
# 11 "sa_ip_barr.c" 1
	pop %rax

# 0 "" 2
# 12 "sa_ip_barr.c" 1
	jmp return_label
# 0 "" 2
#NO_APP
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	in_place, .-in_place
	.globl	cache_encode
	.type	cache_encode, @function
cache_encode:
.LFB1:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
#APP
# 16 "sa_ip_barr.c" 1
	movq (%rcx), %rax

# 0 "" 2
#NO_APP
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	cache_encode, .-cache_encode
	.globl	attacker
	.type	attacker, @function
attacker:
.LFB2:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
#APP
# 20 "sa_ip_barr.c" 1
	movq $65, %r14	

# 0 "" 2
#NO_APP
	movl	$0, %eax
	call	in_place
#APP
# 22 "sa_ip_barr.c" 1
	mfence
# 0 "" 2
# 24 "sa_ip_barr.c" 1
	movq %r14, %rax
	
# 0 "" 2
#NO_APP
	movq	%rax, secret(%rip)
	movq	secret(%rip), %rdx
	movq	%rdx, %rcx
#APP
# 25 "sa_ip_barr.c" 1
	movq (%rcx), %rax

# 0 "" 2
#NO_APP
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2:
	.size	attacker, .-attacker
	.globl	victim
	.type	victim, @function
victim:
.LFB3:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
#APP
# 29 "sa_ip_barr.c" 1
	return_label:
# 0 "" 2
#NO_APP
	movl	real_secret(%rip), %eax
	movl	%eax, %ecx
#APP
# 31 "sa_ip_barr.c" 1
	movq %ecx, %r14	

# 0 "" 2
#NO_APP
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE3:
	.size	victim, .-victim
	.globl	main
	.type	main, @function
main:
.LFB4:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	$0, %eax
	call	attacker
	movl	$0, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE4:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 9.4.0-1ubuntu1~20.04.1) 9.4.0"
	.section	.note.GNU-stack,"",@progbits
	.section	.note.gnu.property,"a"
	.align 8
	.long	 1f - 0f
	.long	 4f - 1f
	.long	 5
0:
	.string	 "GNU"
1:
	.align 8
	.long	 0xc0000002
	.long	 3f - 2f
2:
	.long	 0x1
3:
	.align 8
4:
