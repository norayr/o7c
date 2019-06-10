	.file	"float.c"
	.section	.rodata
.LC1:
	.string	"%f\n"
	.text
.globl main
	.type	main, @function
main:
	leal	4(%esp), %ecx
	andl	$-16, %esp
	pushl	-4(%ecx)
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ecx
	subl	$36, %esp
	movl	$0x40200000, %eax
	movl	%eax, -8(%ebp)
	flds	-8(%ebp)
	fstpl	4(%esp)
	movl	$.LC1, (%esp)
	call	printf
	movl	$0, (%esp)
	call	exit
	.size	main, .-main
	.ident	"GCC: (GNU) 4.1.2 (Gentoo 4.1.2 p1.1)"
	.section	.note.GNU-stack,"",@progbits
