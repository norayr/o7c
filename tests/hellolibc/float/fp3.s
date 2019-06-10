	.file	"fp3.c"
	.section	.rodata
.LC0:
	.string	"%f\n"
	.text
.globl lalala
	.type	lalala, @function
lalala:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	flds	8(%ebp)
	fstpl	4(%esp)
	movl	$.LC0, (%esp)
	call	printf
	movl	$0, %eax
	leave
	ret
	.size	lalala, .-lalala
.globl main
	.type	main, @function
main:
	leal	4(%esp), %ecx
	andl	$-16, %esp
	pushl	-4(%ecx)
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ecx
	subl	$20, %esp
	movl	$0x40200000, %eax
	movl	%eax, -12(%ebp)
	movl	$0x40400000, %eax
	movl	%eax, -8(%ebp)
	flds	-12(%ebp)
	fmuls	-8(%ebp)
	fstps	(%esp)
	call	lalala
	movl	$0, %eax
	addl	$20, %esp
	popl	%ecx
	popl	%ebp
	leal	-4(%ecx), %esp
	ret
	.size	main, .-main
	.ident	"GCC: (GNU) 4.1.2 (Gentoo 4.1.2 p1.1)"
	.section	.note.GNU-stack,"",@progbits
