MODULE test5;
VAR s : ARRAY 5 OF CHAR;
PROCEDURE aaa (VAR a : ARRAY OF CHAR);
VAR b : ARRAY 4 OF CHAR;
i : INTEGER;
BEGIN
b := "abc";
a := b;

END ass;

BEGIN
s := "123";
aaa(s);
END test5.

