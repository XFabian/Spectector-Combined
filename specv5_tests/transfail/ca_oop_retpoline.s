	.file	"ca_oop_retpoline.c"
	.text
	.globl	secret
	.data
	.align 4
	.type	secret, @object
	.size	secret, 4
secret:
	.long	83
	.text
	.globl	pollute_rsb
	.type	pollute_rsb, @function
pollute_rsb:
.LFB6:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
#APP
# 13 "ca_oop_retpoline.c" 1
	pop %rax

# 0 "" 2
# 14 "ca_oop_retpoline.c" 1
	jmp return_label
# 0 "" 2
#NO_APP
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE6:
	.size	pollute_rsb, .-pollute_rsb
	.globl	attacker
	.type	attacker, @function
attacker:
.LFB7:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$0, %eax
	call	pollute_rsb
	movl	secret(%rip), %edx
	movl	%edx, %ecx
#APP
# 20 "ca_oop_retpoline.c" 1
	movq (%ecx), %rax

# 0 "" 2
#NO_APP
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE7:
	.size	attacker, .-attacker
	.globl	victim
	.type	victim, @function
victim:
.LFB8:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
#APP
# 24 "ca_oop_retpoline.c" 1
	return_label:
# 0 "" 2
# 25 "ca_oop_retpoline.c" 1
	popq %rbp
# 0 "" 2
# 26 "ca_oop_retpoline.c" 1
	call return_new
# 0 "" 2
# 27 "ca_oop_retpoline.c" 1
	speculate:
# 0 "" 2
# 28 "ca_oop_retpoline.c" 1
	nop
# 0 "" 2
# 29 "ca_oop_retpoline.c" 1
	jmp speculate
# 0 "" 2
# 30 "ca_oop_retpoline.c" 1
	return_new:
# 0 "" 2
# 31 "ca_oop_retpoline.c" 1
	add $8, %rsp	

# 0 "" 2
# 32 "ca_oop_retpoline.c" 1
	ret
# 0 "" 2
#NO_APP
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE8:
	.size	victim, .-victim
	.globl	main
	.type	main, @function
main:
.LFB9:
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
.LFE9:
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
