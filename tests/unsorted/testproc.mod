MODULE testproc;
VAR i : REAL;
PROCEDURE Add(VAR a : REAL; b : REAL);
BEGIN
a := a + b;
END Add;

PROCEDURE Begin(VAR a : REAL);
BEGIN
Add (a, 5.0);
END Begin;

BEGIN
i := 5.0;
Begin(i);
END testproc.
