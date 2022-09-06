	.file	"case6_mod.c"
	.text
	.globl	publicarray_size
	.data
	.align 4
	.type	publicarray_size, @object
	.size	publicarray_size, 4
publicarray_size:
	.long	16
	.globl	publicarray
	.align 4
	.type	publicarray, @object
	.size	publicarray, 16
publicarray:
	.ascii	"\001\002\003\004\005\006\007\b\t\n\013\f\r\016\017\020"
	.globl	publicarray2
	.align 32
	.type	publicarray2, @object
	.size	publicarray2, 512
publicarray2:
	.string	"\024"
	.zero	510
	.globl	secretarray_size
	.align 4
	.type	secretarray_size, @object
	.size	secretarray_size, 4
secretarray_size:
	.long	16
	.globl	secretarray
	.align 4
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
	.globl	case6_idx
	.align 4
	.type	case6_idx, @object
	.size	case6_idx, 4
case6_idx:
	.zero	4
	.text
	.globl	case_6
	.type	case_6, @function
case_6:
.LFB0:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	subl	$16, %esp
	movl	$secretarray, -8(%ebp)
	movl	$publicarray, -4(%ebp)
#APP
# 21 "case6_mod.c" 1
	mfence
# 0 "" 2
#NO_APP
	movl	secretarray_size, %eax
	decl	%eax
	andl	8(%ebp), %eax
	movl	%eax, %edx
	movl	$1, case6_idx
#APP
# 26 "case6_mod.c" 1
	mfence
# 0 "" 2
#NO_APP
	movl	case6_idx, %eax
	movl	-8(%ebp,%eax,4), %ecx
	movl	%edx, %eax
	addl	%ecx, %eax
	movb	(%eax), %al
	movb	%al, %cl
	movb	%cl, %al
	movzbl	%al, %eax
	sall	$4, %eax
	movb	publicarray2(%eax), %dl
	movb	temp, %al
	andl	%edx, %eax
	movb	%al, temp
	nop
	leave
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
.LFE0:
	.size	case_6, .-case_6
	.ident	"GCC: (GNU) 11.2.0"
	.section	.note.GNU-stack,"",@progbits
