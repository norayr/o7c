MODULE CompatReals;

    PROCEDURE Ten*(e: INTEGER): LONGREAL;
      VAR x, p: LONGREAL;
    BEGIN x := 1; p := 10;
      WHILE e > 0 DO
        IF ODD(e) THEN x := x*p END;
        e := e DIV 2;
        IF e > 0 THEN p := p*p END (* prevent overflow *)
      END;
      RETURN x
    END Ten;

END CompatReals.

