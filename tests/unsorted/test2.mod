MODULE test2;
CONST s* = "ssss";
i* = 5;
VAR a* : INTEGER;

PROCEDURE str*(CONST s : ARRAY OF CHAR; l : INTEGER);
BEGIN
ASM
movl 8(%ebp), %ecx
movl 12(%ebp), %edx
movl $1, %ebx
movl $4, %eax
int $0x80
END;
END str;

BEGIN
a := 2;
END test2.
