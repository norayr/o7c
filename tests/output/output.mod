MODULE output;
IMPORT Out;
VAR i : INTEGER;
s : ARRAY 15 OF CHAR;

BEGIN
 s := "hello, world";
 Out.String(s); Out.Ln;
 i := 1234567;
 Out.Int(i, 0); Out.Ln;
 Out.String("aaa"); Out.Int(5, 0);Out.Ln;
END output.


