MODULE disp;
TYPE p=POINTER TO r;
     r=RECORD x,y : INTEGER END;
     p2 = POINTER TO r2;
     r2 = RECORD (r) z, q : INTEGER END;
VAR P : p; P2 : p2; k : INTEGER;
BEGIN
NEW(P) ;
NEW(P2);
DISPOSE (P);
P := P2;
DISPOSE(P);
END disp.
