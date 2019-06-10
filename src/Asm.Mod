MODULE Asm;

IMPORT OSAS, OSAG;

PROCEDURE Asm*;
VAR ch : CHAR;
k : BOOLEAN;
a : ARRAY 2000 OF CHAR;
i, j : LONGINT;
BEGIN
k := FALSE;
i := 0;
REPEAT
   OSAS.Asm(ch);
   a[i] := ch;
   IF (a[i] = 'D') & (a[i-1] = 'N') & (a[i-2] = 'E') THEN
      k := TRUE
   END;
   INC(i)
UNTIL k;
FOR j := 0 TO i-4 DO
   OSAG.WriteChar(a[j])
END;

END Asm;



BEGIN


END Asm.
