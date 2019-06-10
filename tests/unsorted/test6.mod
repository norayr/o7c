MODULE test6;

TYPE 
  ptr = POINTER TO p;
  p = RECORD
 x,y,z : INTEGER
 END;

 VAR r : p;
 ppp : ptr;
 l : INTEGER;
   PROCEDURE a(VAR t : ptr);
   BEGIN
   l := t^.x;
   END a;

BEGIN
NEW(ppp);
a(ppp);
ppp^.x := 5;
l := ppp^.x;
r.x := 1;
l := r.x;
ppp^.z := l;
r.x := ppp^.z;


(*
 r.z := 5;
 l := 8;
 r.y := 7;

 pro(r);
 l := r.x; l := r.y;
*)
END test6.



