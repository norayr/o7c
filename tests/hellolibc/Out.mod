MODULE Out;
IMPORT Unix, IntStr;
(*VAR l : INTEGER;
ch : CHAR;
*)
PROCEDURE String* (CONST s : ARRAY OF CHAR);
VAR i : INTEGER;
BEGIN
i := 0;
WHILE s[i] # 0X DO
INC(i);
(*ch := s[i];*)
END;
   Unix.write(s, i)
END String;

PROCEDURE Char*(ch : CHAR);
VAR a : ARRAY 2 OF CHAR;
BEGIN
a[0] := ch;
a[1] := 0X;
Unix.write (a, 1);
END Char;

PROCEDURE Ln*;
VAR a : ARRAY 2 OF CHAR;
BEGIN
a[0] := 0AX;
a[1] := 0X;
Unix.write (a, 1);
END Ln;

PROCEDURE Int*(i, offset : INTEGER);
VAR a : ARRAY 11 OF CHAR;
k : INTEGER;
BEGIN
k := 0;
WHILE k # offset DO Char(" "); INC(k) END;
IntStr.IntToStr(i, a);
k := 0;
WHILE a[k] # 0X DO
INC(k);
END;
Unix.write (a, k);
END Int;

PROCEDURE Real*(f : REAL; offset : INTEGER);
VAR k : INTEGER;
BEGIN
k := 0;
WHILE k # offset DO Char(" "); INC(k) END;
Unix.writefloat(f);
END Real;
END Out.



















