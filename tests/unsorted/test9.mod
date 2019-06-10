MODULE test9;
IMPORT Out;
PROCEDURE a;
VAR m : ARRAY 10,10 OF INTEGER;
j,i,k : INTEGER;
BEGIN
j := 0; i := 0; k := 0;
  REPEAT
     FOR i := 0 TO 9 DO
       m[i,j] := i;
     END;
   i := 0;  
  INC(j)
  UNTIL j= 9;

  REPEAT
     FOR i := 0 TO 9 DO
       Out.Int(m[i,j]); Out.Char (" ");
     END;
   i := 0;  
  INC(j); Out.Ln;
  UNTIL j= 9;


END a;

BEGIN

END test9.
