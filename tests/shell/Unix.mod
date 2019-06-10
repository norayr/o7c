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

PROCEDURE system*(CONST sh : ARRAY OF CHAR);
BEGIN
ASM
.section .data
.comm Unix_system_Null, 4
.comm Unix_system_Adr, 4
.section .data
Unix_default_shell: .ascii "/bin/sh\0"
.section .text
xor %eax, %eax
movl %eax, Unix_system_Null
leal Unix_default_shell, %ebx
movl 8(%ebp), %ecx
leal Unix_system_Null, %edx
movl $11, %eax
int $0x80
END;
END system;

BEGIN
END Unix.

