MODULE fpcomp;
(*CONST f3 = 4.5;*)
VAR f, f2 : REAL;
bool : BOOLEAN;
BEGIN
f := 4.0;
f2 := 4.9;
(*bool := f <= f2;*)
IF f < f2 THEN bool := TRUE ELSE bool := FALSE END;
END fpcomp.
