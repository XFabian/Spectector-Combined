	.file	"jump_table.c"
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
	.long	16
	.globl	secretarray
	.align 16
	.type	secretarray, @object
	.size	secretarray, 16
secretarray:
	.ascii	"\n\025 +6ALWbmny\204\217\232\245"
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
.LFB0:
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
# 15 "jump_table.c" 1
	lfence
# 0 "" 2
#NO_APP
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	leaky, .-leaky
	.section	.rodata
.LC0:
	.string	"Freezing"
.LC1:
	.string	"Dirty"
.LC2:
	.string	"Dry"
.LC3:
	.string	"Windy"
	.text
	.globl	main
	.type	main, @function
main:
.LFB1:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
#APP
# 20 "jump_table.c" 1
	lfence
# 0 "" 2
# 21 "jump_table.c" 1
	movl $5, %eax
# 0 "" 2
#NO_APP
	movl	$1, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	leaq	0(,%rax,8), %rdx
	leaq	table.2352(%rip), %rax
	movq	(%rdx,%rax), %rax
	nop
	jmp	__x86_indirect_thunk_rax
.L4:
	leaq	.LC0(%rip), %rdi
	call	puts@PLT
	jmp	.L2
.L6:
	leaq	.LC1(%rip), %rdi
	call	puts@PLT
	jmp	.L2
.L7:
	leaq	.LC2(%rip), %rdi
	call	puts@PLT
	jmp	.L2
.L8:
	leaq	.LC3(%rip), %rdi
	call	puts@PLT
	nop
.L2:
	leave
	.cfi_restore 6
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	main, .-main
	.section	.data.rel.local,"aw"
	.align 32
	.type	table.2352, @object
	.size	table.2352, 32
table.2352:
	.quad	.L4
	.quad	.L6
	.quad	.L7
	.quad	.L8
	.section	.text.__x86_indirect_thunk_rax,"axG",@progbits,__x86_indirect_thunk_rax,comdat
	.globl	__x86_indirect_thunk_rax
	.hidden	__x86_indirect_thunk_rax
	.type	__x86_indirect_thunk_rax, @function
__x86_indirect_thunk_rax:
.LFB2:
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
.LFE2:
	.ident	"GCC: (Ubuntu 9.4.0-1ubuntu1~20.04.2) 9.4.0"
	.section	.note.GNU-stack,"",@progbits
