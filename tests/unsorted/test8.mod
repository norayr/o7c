MODULE test8;
(*IMPORT Out;*)

CONST a = 0;

TYPE p = POINTER TO t;
t = RECORD x,y : INTEGER END;
 p1 = POINTER TO t1;
 t1 = RECORD(t) z : INTEGER END;
 p2 = POINTER TO t2;
 t2 = RECORD(t1) q : INTEGER END;
 p3 = POINTER TO t3;
 t3 = RECORD(t2) w : INTEGER END;
 (*p4 = POINTER TO t3;
 t4 = RECORD(t3) w : INTEGER END;
 p5 = POINTER TO t3;
 t5 = RECORD(t4) w : INTEGER END;
 p6 = POINTER TO t3;
 t6 = RECORD(t5) w : INTEGER END;
 p7 = POINTER TO t3;
 t7 = RECORD(t6) w : INTEGER END;*)

VAR P : p;
 P1 : p1;
 P2 : p2;
 P3 : p3;
 PP3 : p3;
k : INTEGER;

(*
PROCEDURE a;
VAR v : ARRAY 20 OF INTEGER;
p : ARRAY 200 OF INTEGER;
lim, sqr : INTEGER;
BEGIN
lim := 3;
sqr := 15;
Out.Int(v[lim]); Out.Ln;
v[lim] := sqr;
INC(lim);
sqr := p[lim] MOD p[lim];
END a;*)
BEGIN
NEW(P); NEW(P1); NEW(P2); NEW(P3); NEW(PP3);
IF P1 IS p3 THEN
k := 0
END;

(*P1 := P2;*)
(*P(p).x := 5;*)
(*IF P IS  p2 THEN k := 3 END;*)
(*a;*)
END test8.

