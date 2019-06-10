MODULE hanoi;

 IMPORT Out;

 VAR DiscNum: INTEGER;

PROCEDURE WriteStep(Peg1,Peg2 : INTEGER);
    BEGIN
	    Out.Int(Peg1,0);
	    Out.String(" ---> ");
	    Out.Int(Peg2,0);
	    Out.Ln; 
END WriteStep;


PROCEDURE BuildTower(DiscNum, OrigPeg, NewPeg, TempPeg : INTEGER);
    BEGIN
    IF DiscNum = 1 THEN
	    WriteStep(OrigPeg,NewPeg);
    ELSE
	    BuildTower(DiscNum-1,OrigPeg,TempPeg,NewPeg);
	    WriteStep(OrigPeg,NewPeg);
	    BuildTower(DiscNum-1,TempPeg,NewPeg,OrigPeg);
    END;

END BuildTower;

BEGIN
    (*DiscNum := 0;*)
    (*Out.String("Please input the number of discs:"); Out.Ln;
    In.Int(DiscNum); Out.Ln;*)
    DiscNum := 15;
    IF DiscNum > 0 THEN
	    BuildTower(DiscNum, 1, 3, 2);
    ELSE
	    Out.String("Error: The number of discs is invalid"); Out.Ln;
    END;
END hanoi.
