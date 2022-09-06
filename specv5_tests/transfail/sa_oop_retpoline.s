	.file	"sa_oop_retpoline.c"
	.text
	.globl	SECRET
	.data
	.align 16
	.type	SECRET, @object
	.size	SECRET, 20
SECRET:
	.string	"INACCESSIBLE SECRET"
	.comm	data,128,32
	.comm	idx,4,4
	.text
	.globl	call_manipulate_stack
	.type	call_manipulate_stack, @function
call_manipulate_stack:
.LFB6:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
#APP
# 16 "sa_oop_retpoline.c" 1
	pop %rax

# 0 "" 2
# 17 "sa_oop_retpoline.c" 1
	popq %rbp
# 0 "" 2
# 18 "sa_oop_retpoline.c" 1
	call return_new
# 0 "" 2
# 19 "sa_oop_retpoline.c" 1
	speculate:
# 0 "" 2
# 20 "sa_oop_retpoline.c" 1
	nop
# 0 "" 2
# 21 "sa_oop_retpoline.c" 1
	jmp speculate
# 0 "" 2
# 22 "sa_oop_retpoline.c" 1
	return_new:
# 0 "" 2
# 23 "sa_oop_retpoline.c" 1
	add $8, %rsp	

# 0 "" 2
# 24 "sa_oop_retpoline.c" 1
	ret
# 0 "" 2
#NO_APP
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE6:
	.size	call_manipulate_stack, .-call_manipulate_stack
	.globl	call_leak
	.type	call_leak, @function
call_leak:
.LFB7:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$0, %eax
	call	call_manipulate_stack
	movl	idx(%rip), %eax
	cltq
	leaq	SECRET(%rip), %rdx
	movzbl	(%rax,%rdx), %edx
	movl	%edx, %ecx
#APP
# 32 "sa_oop_retpoline.c" 1
	movq (%cl), %rax

# 0 "" 2
#NO_APP
	movl	$2, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE7:
	.size	call_leak, .-call_leak
	.globl	call_start
	.type	call_start, @function
call_start:
.LFB8:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$0, %eax
	call	call_leak
	movl	$1, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE8:
	.size	call_start, .-call_start
	.globl	confuse_compiler
	.type	confuse_compiler, @function
confuse_compiler:
.LFB9:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$0, %eax
	call	call_start
	movl	$0, %eax
	call	call_leak
	movl	$0, %eax
	call	call_manipulate_stack
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE9:
	.size	confuse_compiler, .-confuse_compiler
	.globl	main
	.type	main, @function
main:
.LFB10:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$48, %rsp
	movl	%edi, -36(%rbp)
	movq	%rsi, -48(%rbp)
	movq	%fs:40, %rax
	movq	%rax, -8(%rbp)
	xorl	%eax, %eax
	movb	$0, -12(%rbp)
	movl	$0, idx(%rip)
.L8:
	movl	idx(%rip), %eax
	addl	$1, %eax
	movslq	%eax, %rcx
	movabsq	$-3689348814741910323, %rdx
	movq	%rcx, %rax
	mulq	%rdx
	shrq	$4, %rdx
	movq	%rdx, %rax
	salq	$2, %rax
	addq	%rdx, %rax
	salq	$2, %rax
	subq	%rax, %rcx
	movq	%rcx, %rdx
	movl	%edx, %eax
	movl	%eax, idx(%rip)
	movl	$0, %eax
	call	call_start
	jmp	.L8
	.cfi_endproc
.LFE10:
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
