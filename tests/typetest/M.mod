MODULE M;
IMPORT Out;
(*IMPORT M0;*)
  TYPE R0 = RECORD x: INTEGER END ;
        R1 = RECORD (R0) y: INTEGER END ;
        R2 = RECORD (R1) z: INTEGER END ;
        P0 = POINTER TO R0;
        P1 = POINTER TO R1;
        P2 = POINTER TO R2;
  VAR k: INTEGER;
        p0: P0; p1: P1; p2: P2;

BEGIN NEW(p2);
NEW(p1);
  p1 := p2;

  IF p1 IS P2 THEN p1^.y := 3 END ;
  k := p2^.y;
  (* p2^.z := 5;
  k := p1(P2).z;*)
  Out.Int(k, 0); Out.Ln
END M.

