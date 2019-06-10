MODULE ooptest; 
IMPORT  Out; 

TYPE  T = RECORD x, y:  INTEGER  END; 
      T0 = RECORD (T) z: INTEGER END;  (* extension of T *) 
      T1 = RECORD (T) w: INTEGER END;  (* extension of T *)  
      action  = PROCEDURE (a,b: INTEGER): INTEGER; (* a procedural type *) 
      T2 = RECORD (T) 
     Add : action   (*  "Add" is a "method" of the T2 class *) 
     END; 
VAR 
  object: T2; result: INTEGER; 

PROCEDURE AddIt(a, b : INTEGER): INTEGER; 
BEGIN 
        RETURN a + b 
END AddIt; 

PROCEDURE Do*; 
BEGIN 
        Out.Int(result,0); 
        Out.Ln; 
END Do; 

BEGIN 
  object.x := 123; object.y := 4; object.Add := AddIt; 
  result := object.Add (object.x, object.y); (* method call *) 
  Do
END ooptest.

