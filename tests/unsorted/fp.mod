MODULE fp;
CONST const = 5.0;

VAR a,b,c,d : REAL;
i : INTEGER;

PROCEDURE t(VAR f : REAL);
VAR ff : REAL;
BEGIN
f := f + ff - const
END t;
(*
PROCEDURE tes(a,b,c : REAL; VAR d : REAL);
BEGIN
d := a / b * c;


END tes;
*)

BEGIN
a := 5.5; t(a);
b := 5.4;
c := 5.3;
(*tes (a,b,c,d);*)
END fp.

