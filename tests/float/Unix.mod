MODULE Unix;
PROCEDURE write*(CONST s : ARRAY OF CHAR; l : INTEGER);
BEGIN
ASM
movl 8(%ebp), %edi
pushl %edi
call printf
popl %edi
END;
END write;

PROCEDURE writeint*(i : INTEGER);
BEGIN
ASM
.section .rodata
.unix_writeint_intlab:
        .string "%d\n"
        .text
        pushl   8(%ebp)
#        pushl   $0
        pushl   $.unix_writeint_intlab
        call    printf 
END;
END writeint;

PROCEDURE writefloat*(f : REAL);
BEGIN
ASM
.section .rodata
.unix_writefloat_floatlab:
        .string "%f\n"
        .text
	subl $36, %esp
        flds 8(%ebp)
	fstpl 4(%esp)
	#pushl   8(%ebp)
        #pushl   $0
        #pushl   $.unix_writefloat_floatlab
        movl   $.unix_writefloat_floatlab, (%esp)
        call    printf
	movl $0, (%esp)
END;
END writefloat;

PROCEDURE system*(CONST s : ARRAY OF CHAR);
BEGIN
ASM
movl 8(%ebp), %edi
pushl %edi
call system
popl %edi
END;
END system;

BEGIN
END Unix.

