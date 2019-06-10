MODULE foc;

IMPORT args, Out := OakOut, Strings := OakStrings, OSAP, OSAG,
(*
OS:ProcessManagement(*Rts*), Object;
*)
libc;

VAR name : ARRAY 255 OF CHAR;
main : BOOLEAN;


PROCEDURE Showhelp;
BEGIN
Out.String ("Usage:"); Out.Ln;
Out.String (" oc modulename options"); Out.Ln;
Out.String ("    options:"); Out.Ln;
Out.String (" -s  new symbol file"); Out.Ln;
Out.String (" -x  without runtime checks"); Out.Ln;
Out.String (" -m  main module"); Out.Ln;
HALT(0)
END Showhelp;





PROCEDURE CompileModule(VAR s : ARRAY OF CHAR);
VAR tmpstr : ARRAY 255 OF CHAR;
i : LONGINT;
BEGIN
   i := 0; (*main := TRUE;*)
   OSAP.Compile (s, main (* main or not *));
   (*
   COPY ('hla -sourcemode -sg ', tmpstr);*)
   (*Strings.Append (OSAG.modname, tmpstr);
   Strings.Append ('.o', tmpstr);
   Strings.Append (' ', tmpstr);*)
   Strings.Append (OSAG.asmname, tmpstr);
   (*i := Rts.System (tmpstr);*) (* ooc 1 *)
   (*i := ProcessManagement.system(Object.NewLatin1(tmpstr));*) (* oo2c 2 *)
   (*libc.system(tmpstr);*) (* ofront *)
   COPY ('as -o ', tmpstr);
   Strings.Append (OSAG.modname, tmpstr);
   Strings.Append ('.o ', tmpstr);
   Strings.Append (OSAG.modname, tmpstr);
   Strings.Append ('.s', tmpstr);
   libc.system(tmpstr);
   (*COPY ('hla -c ', tmpstr);
   Strings.Append (OSAG.asmname, tmpstr);
   libc.system(tmpstr);*)
   (*IF i = 0 THEN
      IF main THEN
      COPY ('ld -o ', tmpstr);
      Strings.Append (OSAG.modname, tmpstr);
      Strings.Append (' ', tmpstr);
      Strings.Append (OSAG.modname, tmpstr);
      Strings.Append ('.o', tmpstr);
      Strings.Append (' /usr/hla/hlalib/hlalib.a', tmpstr);
      (*
      i := ProcessManagement.system (Object.NewLatin1(tmpstr)); (* for oo2c *)
      *)
      libc.system(tmpstr); (* for ofront *)
      END
   END;*)
END CompileModule;

PROCEDURE ParseOptions;
VAR 
ch : CHAR;
options : ARRAY 255 OF CHAR;
i : INTEGER;
BEGIN
OSAP.newSF := FALSE; OSAP.check := TRUE; main := FALSE;

IF args.argscount() < 1 THEN Out.String ("Error processing: wrong number of arguments"); Out.Ln; Showhelp; HALT(0) END;
args.arg (1, name);
 IF ((name[0] = '-') OR (name[0]='/') OR (name[0]='\')) THEN 
    COPY(name, options); name := '';
    args.arg(2, name);
 ELSE
    args.arg(2, options);
 END;
i := 1;
REPEAT 
   ch := options[i];  INC( i );  
     CASE ch OF 
       "s":  OSAP.newSF := TRUE;  
     | "x":  OSAP.check := TRUE;
     | "m":  main := TRUE;
     ELSE 
     IF ch > " " THEN Out.String( "Option not found:" );  Out.Char( ch );  Out.Ln;  END
     END
UNTIL ch = 0X;

END ParseOptions;

BEGIN
COPY ("", name);
ParseOptions;
CompileModule(name);



END foc.
