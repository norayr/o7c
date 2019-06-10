MODULE test4;
(*IMPORT Out;*)
VAR s : ARRAY 15 OF CHAR;
i : INTEGER;
l : CHAR;
ch : CHAR;

PROCEDURE w(k : INTEGER; VAR a : ARRAY OF CHAR);
VAR b : ARRAY 5 OF CHAR;
e, t : INTEGER;
BEGIN
e := 1;
t := 65;
b[e] := CHR(t);
ch := b[e];
END w;

PROCEDURE u(i : INTEGER);
VAR ii : INTEGER;
dd : ARRAY 5 OF CHAR;
BEGIN
ii := 3;
dd[0] := "a";
dd[1] := "1";
w(ii-2, dd);
END u;

PROCEDURE write (VAR a : ARRAY OF CHAR);
BEGIN
i := 5;
u(i);
i := 0;
WHILE a[i] # 0X DO INC(i) END;
ch := a[2];
END write;

PROCEDURE a;
VAR k : INTEGER;
aa : ARRAY 5 OF CHAR;
BEGIN
s := "abcdef";
write (s);

k := 2;
(*aa[0] := "A";
aa[1] := "B";*)
aa[k] := "a";
(*aa := "aaa";*)
l := aa[k];
END a;


BEGIN
a;
(*s := "hello, world";*)
(*i := 0;*)
(*WHILE s[i] # 0X DO INC(i) END;*)
(*Out.String(s); Out.Ln*)
END test4.


