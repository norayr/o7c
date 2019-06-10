MODULE and1;
 IMPORT Out;

 VAR a,b,c: INTEGER;
    aa,bb,cc: BOOLEAN;

PROCEDURE OutBool(bool: BOOLEAN);
BEGIN
  IF bool = TRUE THEN Out.String("TRUE"); Out.Ln;
  ELSIF bool = FALSE THEN Out.String("FALSE"); Out.Ln;
  ELSE Out.String("Oops"); Out.Ln;
  END;
END OutBool;

BEGIN

a := 1;
b := 2;
c := 3;

aa := TRUE;
bb := FALSE;
cc := ((aa & FALSE) OR (aa & aa & FALSE));
OutBool(cc);


END and1.
