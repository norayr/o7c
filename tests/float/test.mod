MODULE test;
IMPORT Unix;

VAR i : INTEGER;
f, ff, fff : REAL;
bool : BOOLEAN;
PROCEDURE write (a, b : REAL);
BEGIN
Unix.writefloat(a);
Unix.writefloat(b);
END write;


BEGIN
(*i := 5;*)
f := 5.25;
ff := 3.0;
fff := 1.0;
bool := ff > fff;
(*Unix.writeint(i);
Unix.writefloat(f);*)
IF bool THEN Unix.writeint(1) ELSE Unix.writeint (0) END;

write(fff, f * ff);
(*fff := f * ff;
Unix.writefloat(fff);*)
END test.
