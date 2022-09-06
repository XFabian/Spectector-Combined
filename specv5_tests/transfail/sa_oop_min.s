	.file	"sa_oop_min.c"
	.text
	.comm	data,128,32
	.globl	secretarray
	.data
	.align 16
	.type	secretarray, @object
	.size	secretarray, 16
secretarray:
	.ascii	"\001\002\003\004\005\006\007\b\t\n\013\f\r\016\017\020"
	.comm	idx,4,4
	.globl	temp
	.bss
	.align 4
	.type	temp, @object
	.size	temp, 4
temp:
	.zero	4
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
# 18 "sa_oop_min.c" 1
	pop %rax

# 0 "" 2
#NO_APP
	movl	$0, %eax
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
	leaq	secretarray(%rip), %rdx
	movzbl	(%rax,%rdx), %eax
	movzbl	%al, %eax
	cltq
	leaq	data(%rip), %rdx
	movzbl	(%rax,%rdx), %eax
	movzbl	%al, %edx
	movl	temp(%rip), %eax
	andl	%edx, %eax
	movl	%eax, temp(%rip)
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
