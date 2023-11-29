	.file	"func_table.c"
	.text
	.globl	publicarray
	.data
	.align 16
	.type	publicarray, @object
	.size	publicarray, 16
publicarray:
	.ascii	"\001\002\003\004\005\006\007\b\t\n\013\f\r\016\017\020"
	.globl	secretarray_size
	.align 4
	.type	secretarray_size, @object
	.size	secretarray_size, 4
secretarray_size:
	.long	18
	.globl	secretarray
	.align 16
	.type	secretarray, @object
	.size	secretarray, 18
secretarray:
	.ascii	"\n\025 +6ALWbmny\204\217\232\245\260\273"
	.globl	temp
	.bss
	.type	temp, @object
	.size	temp, 1
temp:
	.zero	1
	.text
	.globl	leaky
	.type	leaky, @function
leaky:
.LFB6:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	leaq	secretarray(%rip), %rdx
	movzbl	(%rax,%rdx), %eax
	movb	%al, -1(%rbp)
	movzbl	-1(%rbp), %eax
	cltq
	leaq	publicarray(%rip), %rdx
	movzbl	(%rax,%rdx), %edx
	movzbl	temp(%rip), %eax
	andl	%edx, %eax
	movb	%al, temp(%rip)
#APP
# 16 "func_table.c" 1
	lfence
# 0 "" 2
#NO_APP
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE6:
	.size	leaky, .-leaky
	.section	.rodata
.LC0:
	.string	"Winter"
	.text
	.globl	func3
	.type	func3, @function
func3:
.LFB7:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	leaq	.LC0(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
#APP
# 19 "func_table.c" 1
	lfence
# 0 "" 2
#NO_APP
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE7:
	.size	func3, .-func3
	.section	.rodata
.LC1:
	.string	"Fall"
	.text
	.globl	func2
	.type	func2, @function
func2:
.LFB8:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	leaq	.LC1(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
#APP
# 20 "func_table.c" 1
	lfence
# 0 "" 2
#NO_APP
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE8:
	.size	func2, .-func2
	.section	.rodata
.LC2:
	.string	"Summer"
	.text
	.globl	func1
	.type	func1, @function
func1:
.LFB9:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	leaq	.LC2(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
#APP
# 21 "func_table.c" 1
	lfence
# 0 "" 2
#NO_APP
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE9:
	.size	func1, .-func1
	.section	.rodata
.LC3:
	.string	"Spring"
	.text
	.globl	func0
	.type	func0, @function
func0:
.LFB10:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	leaq	.LC3(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
#APP
# 22 "func_table.c" 1
	lfence
# 0 "" 2
#NO_APP
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE10:
	.size	func0, .-func0
	.globl	jump_table
	.section	.data.rel.local,"aw"
	.align 32
	.type	jump_table, @object
	.size	jump_table, 32
jump_table:
	.quad	func0
	.quad	func1
	.quad	func2
	.quad	func3
	.text
	.globl	main
	.type	main, @function
main:
.LFB11:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movq	8+jump_table(%rip), %rax
	call	__x86_indirect_thunk_rax
	movl	$0, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE11:
	.size	main, .-main
	.section	.text.__x86_indirect_thunk_rax,"axG",@progbits,__x86_indirect_thunk_rax,comdat
	.globl	__x86_indirect_thunk_rax
	.hidden	__x86_indirect_thunk_rax
	.type	__x86_indirect_thunk_rax, @function
__x86_indirect_thunk_rax:
.LFB12:
	.cfi_startproc
	call	.LIND1
.LIND0:
	pause
	lfence
	jmp	.LIND0
.LIND1:
	.cfi_def_cfa_offset 16
	mov	%rax, (%rsp)
	ret
	.cfi_endproc
.LFE12:
	.ident	"GCC: (Ubuntu 9.4.0-1ubuntu1~20.04.2) 9.4.0"
	.section	.note.GNU-stack,"",@progbits
