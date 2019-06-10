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

