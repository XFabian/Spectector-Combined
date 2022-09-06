	.file	"ca_ip.c"
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
# 10 "ca_ip.c" 1
	pop %rax

# 0 "" 2
# 11 "ca_ip.c" 1
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
	.globl	attacker
	.type	attacker, @function
attacker:
.LFB1:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
.L3:
	movl	$0, %eax
	call	in_place
	movq	secret(%rip), %rdx
	movq	%rdx, %rcx
#APP
# 18 "ca_ip.c" 1
	movq (%rcx), %rax

# 0 "" 2
#NO_APP
	jmp	.L3
	.cfi_endproc
.LFE1:
	.size	attacker, .-attacker
	.globl	victim
	.type	victim, @function
victim:
.LFB2:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
#APP
# 23 "ca_ip.c" 1
	return_label:
# 0 "" 2
#NO_APP
	movl	real_secret(%rip), %eax
	cltq
	movq	%rax, secret(%rip)
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2:
	.size	victim, .-victim
	.globl	main
	.type	main, @function
main:
.LFB3:
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
.LFE3:
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
