MODULE args;
(* this module is intended to be the wrapper for different argument parsing modules from different compilers*)

IMPORT (* for oo2c *)
(*
ProgramArgs, TextRider,*)
(* for Ofront *)
Args,
(* common *)
Out := OakOut; 

CONST notfound* = "not found";

PROCEDURE argscount*() : LONGINT;
VAR (*r : TextRider.Reader;*)
l : LONGINT;
BEGIN
(* oo2c
r := TextRider.ConnectReader(ProgramArgs.args);
IF r = NIL THEN  Out.String ("failed to connect reader to 'args'"); Out.Ln END;
l := ProgramArgs.args.ArgNumber();
*)
l := Args.argc -1 ;
RETURN(l);

END argscount;


PROCEDURE arg*(l : INTEGER; VAR str : ARRAY OF CHAR);
VAR
(* oo2c *)
(*
r : TextRider.Reader;
*)
s : ARRAY 255 OF CHAR;
i : LONGINT;
BEGIN
(*r := TextRider.ConnectReader(ProgramArgs.args);
IF r = NIL THEN Out.String ("failed to connect reader to 'args'"); Out.Ln; END;
i := argscount();
IF argscount() = 0 THEN Out.String ("no arguments given"); Out.Ln; (*NEW (ps); ps^
:= "not found"; RETURN (ps)*)HALT(0) END;
FOR i := 0 TO l DO
r.ReadLine(s);
END;
(*NEW(ps);
ps^ := s;
RETURN (ps)*)
COPY (s, str)
*)
Args.Get (l, s);
COPY (s, str);
END arg;


(*
PROCEDURE argval*(o : ARRAY OF CHAR; VAR str : ARRAY OF CHAR);
VAR  i : LONGINT;
r : TextRider.Reader;
s : string;
ps : pstring;
BEGIN
r := TextRider.ConnectReader(ProgramArgs.args);
IF r = NIL THEN Out.String ("failed to connect reader to 'args'"); Out.Ln; END;
i := argscount();
IF argscount() = 1 THEN Out.String ("no arguments given"); Out.Ln; NEW (ps); ps^
:= "not found"; RETURN (ps) END;

FOR i := 0 TO argscount()-1 DO
r.ReadLine (s);
IF s = o THEN r.ReadLine(s); NEW(ps); ps^ := s; RETURN(ps) END;
END;
NEW(ps);
ps^ :=notfound;
RETURN(ps);
END argval;
*)
BEGIN


END args.
