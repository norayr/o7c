MODULE MagicSquares;   (*NW 3.9.2007 for ARM*)
  IMPORT Out;
  VAR n: INTEGER;
  
  PROCEDURE Generate(n: INTEGER);  (*magic square of order 3, 5, 7, ... *)
    VAR i, j, x, nx, nsq: INTEGER;
      M: ARRAY 13, 13 OF INTEGER;
  BEGIN 
    nsq := n*n; 
    x := 0;
    i := n DIV 2;
    j := n-1;
    WHILE x < nsq DO
      nx := n + x; 
      j := (j-1) MOD n; 
      INC(x); 
      M[i, j] := x;
      WHILE x < nx DO
        i := (i+1)  MOD n; 
        j := (j+1) MOD n;
        INC(x); 
        M[i, j] := x;
      END
    END ;
    FOR i := 0 TO n-1 DO
      FOR j := 0 TO n-1 DO Out.Int(M[i, j], 0); Out.Char (" "); END ;
      Out.Ln
    END
  END Generate;

BEGIN
  n := 3; Generate(n)
END MagicSquares.
