MODULE Unix;
(*VAR j : INTEGER;
ch : CHAR;*)
PROCEDURE write*(CONST s : ARRAY OF CHAR; l : INTEGER);
BEGIN
(*j := 0;*)
(*WHILE s[j] # 0X DO INC(j); ch := s[j] END;*)
(*ch := s[0];
j := l;*)
ASM
movl 8(%ebp), %ecx
movl 16(%ebp), %edx
movl $1, %ebx
movl $4, %eax
int $0x80
END;
END write;

BEGIN
END Unix.

