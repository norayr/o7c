MODULE test7;

VAR f : PROCEDURE (x, y : INTEGER) : INTEGER;
l : INTEGER;

PROCEDURE F (x, y : INTEGER): INTEGER;
BEGIN
   RETURN x + y
END F;

BEGIN
f := F;
l := f(5, 3);
END test7.
