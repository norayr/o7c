MODULE OSAG; (* NW 1.7.97 / 2.5.99 / 24.11.99 / 5.10.2007*)
IMPORT SYSTEM, 
(*Files := OakFiles,*) (*ofront *)
Files, (*ooc*)
TextRider, Out := OakOut,in := In, OSAS, OSAB, Strings := OakStrings, IntStr;
(* Code generator for Oberon compiler for i386 processor *)
(* HLA backend *)
CONST WordSize* = 4;
EAX = 0; EBX = 1 ; ECX = 2; EDX = 3; ESI = 4; EDI = 5; EBP = 6; ESP = 7; EIP =8; EFLAGS = 9;
MaxRegs = ESI(*EBP*);
FP* = EBP(*old 12*); SP = ESP(*old 13*); LNK = EIP(*old 14*); PC = 15;   (*dedicated registers*) (* fp is ebp, sp is esp, lnk is eip*)

WRegLim* = FP-2;  (*minimum of 2 work registers in leaf procedures*)
maxCode = 10000; maxImp = 12; ITlen = 64; STlen = 128;

C8 = 100H; C12 = 1000H; C16 = 10000H; C24 = 1000000H;

RegX = 10; CC = 11; (*internal item modes*)

MAUkey =-853891096 (*0CD1AA7E8H*); FPUkey = -1949024149 (*8BD4406BH*);

(*frequently used opcodes*)
SUB = 4; RSB = 7; ADD = 8; CMP = 21; MOV = 26; MVN = 30; STR = 16; LDR = 17;
labelmark = "_l_";
labelendmark = "_le_";
labelthenmark = "_lt_";
labelormark = "_lor_";
labelandmark = "_lor_";
labelboolmark = "_lbool_";
labelandend = '_landend_';
labelandnext = '_landnext_';
lableandfalse = '_landfalse_';
labelorend = '_lorend_';
labelornext = '_lornext_';
labelortrue = '_lortrue_';
labelnumber = '_label_n_';
condif* = 0; condelseif* = 1; condelse* = 2; condendif* = 3;

TYPE Item* = RECORD
mode*: INTEGER;
type*: OSAB.Type;
a*, b*, r*: LONGINT; (* zvezdochki vremenno *)
rdo*: BOOLEAN;  (*read only*)
label*, ilabel* : OSAS.Ident;
jnum*, jifnum*, jthen*, jor*, jand*, jbool* : LONGINT;
or*, and*, not* : BOOLEAN;
jlab* : OSAS.Ident;
op* : LONGINT;
END ;

(* Item forms and meaning of fields:
mode    r      a       b
--------------------------------
Const   -     value (proc adr)    (immediate adr)
Var     base   off     -               (direct adr)
Par     base   off0     off1         (indirect adr)
Reg    regno
RegI   regno   off     -
RegX   reg0   reg1   scale
CC       cond   Fchain  Tchain  *)

VAR (*savedregs : ARRAY MaxRegs OF LONGINT; savedregsnum  : LONGINT;*)
pc, curlev: INTEGER;   (*program counter; current level*)
entry, firstfixloc: INTEGER;   (*main entry point; first ref to const to be fixed up*)
RL: LONGINT;  (*registers variables in registers R[0] ... R[RL-1]*)
RH: LONGINT;  (*parameters in R[FP-1] ... R[H], in descending order*)
regs: SET;   (*used registers in range R[L] ... R[H-1]*)

landnext, landend, landfalse,
lornext, lorend, lortrue           : LONGINT;
labelnum : LONGINT;
para : ARRAY 21 OF Item;
paraindex : INTEGER;
imports : ARRAY 50 OF OSAS.Ident;
impind : INTEGER;
icx, scx, xrefx: INTEGER;  (*indices of integer and string constant, and ext. ref. tables*)
fixlistFP, fixlistMU: LONGINT;  (*fixup lists for modules FPU and MAU*)

cond, revcond: ARRAY 6 OF INTEGER;  (*condition codes for relations*)
branchint : LONGINT;
fixlist: ARRAY maxImp OF LONGINT;
TIC: ARRAY ITlen OF   (*Table of Integer Constants*)
RECORD val, adr: LONGINT END ;
TSC: ARRAY STlen OF    (*Table of String Constants*)
RECORD index, adr: LONGINT END ;
TXR: ARRAY ITlen OF    (*Table of external references*)
RECORD ref, adr: LONGINT END ;
code: ARRAY maxCode OF LONGINT;

main* : BOOLEAN;
modname* : OSAS.Ident;
asmname* : OSAS.Ident;
loob : LONGINT;
vvv : BOOLEAN;
(* ------------------------------------ non portable between compilers file routins ------------------------------ *)

f: Files.File; r: TextRider.Writer;
fh: Files.File; rh: TextRider.Writer;


PROCEDURE CreateFile (name : ARRAY OF CHAR);
VAR res : Files.Result;
BEGIN
f := Files.New (name, {Files.write}, res);
IF (res # Files.done) THEN
Out.String ("failed to create file "); Out.String (name); Out.Ln; HALT(0);
END;
r := TextRider.ConnectWriter (f);
IF r = NIL THEN
Out.String ("failed to connect writer"); Out.Ln; HALT(0);
END;

END CreateFile;

PROCEDURE Write (ch : ARRAY OF CHAR);
VAR i : LONGINT;
BEGIN
r.WriteString (ch);
END Write;
PROCEDURE WriteChar* (ch : CHAR);
VAR i : LONGINT;
BEGIN
r.WriteChar (ch);
END WriteChar;

PROCEDURE Writeint (l : LONGINT);
BEGIN
r.WriteLInt (l,0);
END Writeint;

PROCEDURE Writeln (ch : ARRAY OF CHAR);
VAR i : LONGINT;
BEGIN
r.WriteString (ch);
r.WriteLn
END Writeln;


PROCEDURE CloseFile;
BEGIN
f.Close;
END CloseFile;

PROCEDURE CreateFileh (name : ARRAY OF CHAR);
VAR res : Files.Result;
BEGIN
fh := Files.New (name, {Files.write}, res);
IF res # Files.done THEN
Out.String ("failed to create file "); Out.String (name); Out.Ln; HALT(0);
END;
rh := TextRider.ConnectWriter (fh);
IF rh = NIL THEN
Out.String ("failed to connect writer"); Out.Ln; HALT(0);
END
END CreateFileh;

PROCEDURE Writeh (ch : ARRAY OF CHAR);
BEGIN
(*Files.WriteString (rh, ch);*)
rh.WriteString (ch);
END Writeh;
PROCEDURE WriteCharh* (ch : CHAR);
VAR i : LONGINT;
BEGIN
rh.WriteChar (ch);
END WriteCharh;

PROCEDURE Writeinth (l : LONGINT);
BEGIN
(*Files.WriteLInt (rh,l);*)
rh.WriteLInt (l,0);
END Writeinth;

PROCEDURE Writelnh (ch : ARRAY OF CHAR);
VAR i : LONGINT;
BEGIN
(*Files.WriteString (rh, ch);
Files.Write (rh, 0AX);*)
rh.WriteString (ch);
rh.WriteLn
END Writelnh;


PROCEDURE CloseFileh;
BEGIN
fh.Close;
END CloseFileh;

(* ----------------------------------------------------- ------------------------------------------------------------------

	(* oakwood *)

f: Files.File; r: Files.Rider;
fh : Files.File; rh : Files.Rider;

PROCEDURE CreateFile (name : ARRAY OF CHAR);
BEGIN
f := Files.New (name);
IF f = NIL THEN
Out.String ("failed to create file "); Out.String (name); Out.Ln; HALT(0);
END;
Files.Set (r, f, 0);
END CreateFile;

PROCEDURE CreateFileh (name : ARRAY OF CHAR);
BEGIN
fh := Files.New (name);
IF fh = NIL THEN
Out.String ("failed to create file "); Out.String (name); Out.Ln; HALT(0);
END;
Files.Set (rh, fh, 0);
END CreateFileh;
PROCEDURE WriteChar* (ch : CHAR);
BEGIN
Files.Write (r, ch); Out.Char (ch)
END WriteChar;

PROCEDURE Write (ch : ARRAY OF CHAR);
BEGIN
Files.WriteString (r, ch); Out.String (ch)
END Write;

PROCEDURE Writeint (l : LONGINT);
BEGIN
Files.WriteLInt (r,l); Out.Int (l, 0);
END Writeint;

PROCEDURE Writeln (ch : ARRAY OF CHAR);
VAR i : LONGINT;
BEGIN
Files.WriteString (r, ch);
Files.Write (r, 0AX);
Out.Ln;
END Writeln;


PROCEDURE CloseFile;
BEGIN
Files.Register(f);
Files.Close(f);
END CloseFile;

PROCEDURE Writeh (ch : ARRAY OF CHAR);
BEGIN
Files.WriteString (rh, ch);
END Writeh;
PROCEDURE WriteCharh* (ch : CHAR);
BEGIN
Files.Write (rh, ch); Out.Char (ch)
END WriteCharh;

PROCEDURE Writeinth (l : LONGINT);
BEGIN
Files.WriteLInt (rh,l);
END Writeinth;

PROCEDURE Writelnh (ch : ARRAY OF CHAR);
VAR i : LONGINT;
BEGIN
Files.WriteString (rh, ch);
Files.Write (rh, 0AX);
END Writelnh;


PROCEDURE CloseFileh;
BEGIN
Files.Register(fh);
Files.Close(fh);
END CloseFileh;





------*)




(*------debug------------*)
PROCEDURE debug (VAR x : Item);
BEGIN
Out.String ("x.mode="); Out.Int(x.mode,0); Out.Ln;
Out.String ("x.r="); Out.Int (x.r,0); Out.Ln;
Out.String ("x.a="); Out.Int (x.a,0); Out.Ln;
Out.String ("x.b="); Out.Int (x.b,0); Out.Ln;
Out.String ("x.label="); Out.String (x.label); Out.Ln;
Out.String ("x.ilabel="); Out.String (x.ilabel); Out.Ln;
END debug;
PROCEDURE debugitem (VAR x : Item);
BEGIN
Write ("// x.mode="); Writeint(x.mode); Writeln ('');
Write ("// x.r="); Writeint (x.r); Writeln ('');
Write ("// x.a="); Writeint (x.a); Writeln ('');
Write ("// x.b="); Writeint (x.b); Writeln ('');
Write ("// x.label="); Writeln (x.label);
Write ("// x.ilabel="); Writeln (x.ilabel)
END debugitem;
PROCEDURE DebugObj( VAR obj : OSAB.Object);
BEGIN
Out.String ("obj.name = "); Out.String (obj.name); Out.Ln;
Out.String ("obj.class = "); Out.Int (obj.class, 0); Out.Ln;
Out.String ("obj.level = "); Out.Int (obj.lev, 0); Out.Ln;
Out.String ("obj.label = "); Out.String (obj.label); Out.Ln;
Out.String ("obj.ilabel = "); Out.String (obj.ilabel); Out.Ln;
Out.String ("obj.val = "); Out.Int (obj.val, 0); Out.Ln;
Out.Ln;
Out.String ("obj.type.form = "); Out.Int(obj.type.form, 0); Out.Ln;
Out.String ("obj.type.nofpar = "); Out.Int (obj.type.nofpar, 0); Out.Ln;
Out.String ("obj.type.len = "); Out.Int (obj.type.len, 0); Out.Ln;
Out.String ("obj.type.size = "); Out.Int (obj.type.size, 0); Out.Ln;

END DebugObj;

(*register management*)

PROCEDURE GetReg(VAR r: LONGINT);
VAR u: LONGINT;
BEGIN
u := RH-1;
WHILE u IN regs DO DEC(u) END ;
IF u >= RL THEN INCL(regs, u) ELSE OSAS.Mark("too few registers") END ;
r := u

END GetReg;

PROCEDURE Release(r: LONGINT);
BEGIN
(*Out.String ("Called Release "); Out.Int(r, 0); Out.Ln;*)
IF (r >= RL) & (r < RH) THEN EXCL(regs, r); (*Out.String ("Releasing "); Out.Int (r, 0); Out.Ln*)  END

END Release;

PROCEDURE GetReg1(VAR r: LONGINT; u: LONGINT);
BEGIN

 Release(u); GetReg(r)

END GetReg1;

PROCEDURE GetReg2(VAR r: LONGINT; u, v: LONGINT);
BEGIN 
Release(v); Release(u); GetReg(r)
END GetReg2;

PROCEDURE ConvertRegNumToName (ham : LONGINT; VAR name : OSAS.Ident);
BEGIN
   CASE ham OF
      0 : name := "eax"
    | 1 : name := "ebx"
    | 2 : name := "ecx"
    | 3 : name := "edx"
    | 4 : name := "esi"
    | 5 : name := "edi"
    | 15 : Out.String ("got 15?")
   END
END ConvertRegNumToName;

PROCEDURE Convert8RegNumToName (ham : LONGINT; VAR name : OSAS.Ident);
BEGIN
   CASE ham OF
      0 : name := "al"
    | 1 : name := "bl"
    | 2 : name := "cl"
    | 3 : name := "dl"
    | 4 : name := "sl" (* wrong *)
    | 5 : name := "dl" (* wrong *)
    | 15 : Out.String ("got 15?")
   END
END Convert8RegNumToName;


PROCEDURE ShowRegs;
VAR l : LONGINT;
BEGIN
l := -1;
REPEAT
INC(l);
IF l IN regs THEN Out.Int (l,0); Out.String (" Used"); Out.Ln ELSE Out.Int (l, 0); Out.String (" Free"); Out.Ln END;
UNTIL l = 5;
END ShowRegs;
(*
PROCEDURE GetThisReg(number : LONGINT);
VAR regname : OSAS.Ident;
BEGIN
ConvertRegNumToName (number, regname);
IF number IN regs THEN 
(*Write ("pushl"); Writeln (regname); savedregs[savedregsnum] := number; INC(savedregsnum)*)
(*hla*)
Write ("push ("); Write (regname);Writeln(');'); savedregs[savedregsnum] := number; INC(savedregsnum);
(*EXCL (regs, number);*)
END
END GetThisReg;
*)
(*
PROCEDURE RecoverThisRegFromStack( number : LONGINT);
VAR regname : OSAS.Ident;
BEGIN
savedregs[number] := PC;
ConvertRegNumToName (number, regname);
(*Write ("popl "); Writeln (regname);*)
Write ("pop ("); Write (regname); Writeln (');');
(*INCL (regs, number);*)
END RecoverThisRegFromStack;
(*instruction assemblers according to formats*)

PROCEDURE stacktoregs;
BEGIN
REPEAT
RecoverThisRegFromStack(savedregsnum);
DEC (savedregsnum)
UNTIL savedregsnum = 0;
END stacktoregs;
*)
PROCEDURE Push(l : INTEGER);
VAR lname : OSAS.Ident;
BEGIN
ConvertRegNumToName (l, lname);
Write ('push ('); Write (lname); Writeln (');');
END Push;

PROCEDURE Pop(l : INTEGER);
VAR lname : OSAS.Ident;
BEGIN
ConvertRegNumToName (l, lname);
Write ('pop ('); Write (lname); Writeln (');');
END Pop;


PROCEDURE Inv(op : INTEGER): INTEGER;
BEGIN
Out.String ('procedure inv, op ='); Out.Int (op, 1); Out.Ln;

CASE op OF
        
	 OSAS.eql : RETURN OSAS.neq 
       | OSAS.neq : RETURN OSAS.eql 
       | OSAS.lss : RETURN OSAS.geq 
       | OSAS.geq : RETURN OSAS.lss
       | OSAS.leq : RETURN OSAS.gtr
       | OSAS.gtr : RETURN OSAS.leq
       | OSAS.in  : Out.String ('in ? '); Out.Ln; HALT(0); 

       END;
END Inv;

PROCEDURE Put0(op, dst, src0, src1: LONGINT);   (*register operation*)
BEGIN
(*
code[pc] := (((0E00H + op)*10H + src0)*10H + dst)*1000H + src1; INC(pc)
*)
END Put0;

PROCEDURE CompareInRegAndConst (op : INTEGER; VAR x : Item; y : Item);
VAR xname : OSAS.Ident; t : INTEGER;
BEGIN
(*	Write ("cmpl $"); Writeint(y); Write(', '); ConvertRegNumToName (x.r, xname); Writeln (xname) *)
ConvertRegNumToName (x.r, xname);
(*IF y.cond = condif THEN Write ('if (');
ELSIF y.cond = condelseif THEN Write ('elseif (');
END;
Write (xname); 
    CASE op OF 
    9: (* = *) Write (" <> "); Out.String ("x.r=0"); Out.Ln;
    |  10: (* # *) Write (" = "); Out.String ("x.r=1"); Out.Ln;
    |  11: (* < *) Write (" >= "); Out.String ("x.r=2"); Out.Ln;
    |  12: (* <= *) Write (" > "); Out.String ("x.r=3"); Out.Ln;
    |  13: (* > *) Write (" <= "); Out.String ("x.r=4"); Out.Ln;
    |  14: (* >= *)Write (" < "); Out.String ("x.r=5"); Out.Ln;
    END;

Writeint (y.a); Write (')'); Writeln ('then ');
*)
Write ('cmp ('); Write (xname); Write (', '); Writeint (y.a); Writeln (');');
    IF x.not THEN t := Inv(op) ELSE t := op END;
      CASE t OF 
    9: (* = *) Write (" sete ("); Out.String ("x.r=0"); Out.Ln;
    |  10: (* # *) Write (" setne ("); Out.String ("x.r=1"); Out.Ln;
    |  11: (* < *) Write (" setl ("); Out.String ("x.r=2"); Out.Ln;
    |  12: (* <= *) Write (" setle ("); Out.String ("x.r=3"); Out.Ln;
    |  13: (* > *) Write (" setg ("); Out.String ("x.r=4"); Out.Ln;
    |  14: (* >= *)Write (" setge ("); Out.String ("x.r=5"); Out.Ln;
    END;

Convert8RegNumToName (x.r, xname);
 Write (xname); Writeln (');');
END CompareInRegAndConst;

PROCEDURE CompareInRegAndMem (VAR x: Item; ylab : OSAS.Ident); (* not used *)
VAR xname : OSAS.Ident;
BEGIN
ConvertRegNumToName (x.r, xname);
Write ("cmpl "); Write (ylab); Write (', '); Writeln (xname);
END CompareInRegAndMem;

PROCEDURE CompareInRegs(op : INTEGER; VAR x, y : Item);
VAR xname, yname : OSAS.Ident; t : INTEGER;
BEGIN
Out.String ('CompareInRegs'); Out.Ln;
Out.String ('x.not = '); IF x.not THEN Out.String ('true') ELSE Out.String ('false') END; Out.Ln; debug(x);
Out.String ('y.not = '); IF x.not THEN Out.String ('true') ELSE Out.String ('false') END; Out.Ln; debug(y);


ConvertRegNumToName (x.r, xname);
ConvertRegNumToName (y.r, yname);
(*
Write (yname);
Out.String ('CASE x.r OF '); Out.Ln; Out.String ('x.r = '); Out.Int (x.r, 0); Out.Ln;
    CASE x.r OF 
    0: (* = *) Write (" = "); Out.String ("x.r=0"); Out.Ln;
    |  1: (* # *) Write (" <> "); Out.String ("x.r=1"); Out.Ln;
    |  2: (* < *) Write (" < "); Out.String ("x.r=2"); Out.Ln;
    |  3: (* <= *) Write (" <= "); Out.String ("x.r=3"); Out.Ln;
    |  4: (* > *) Write (" > "); Out.String ("x.r=4"); Out.Ln;
    |  5: (* >= *)Write (" >= "); Out.String ("x.r=5"); Out.Ln;
		  (* or *) (* stegh kartses takiny petqa pakel *) (*
    |  9: (* =*) Write (" <> "); Out.String ("x.r=9"); Out.Ln;
    | 10: (* #*) Write (" = "); Out.String ("x.r=10"); Out.Ln;
    | 11: (* <*) Write (" >= "); Out.String ("x.r=11"); Out.Ln;
    | 12: (* <= *) Write (" > "); Out.String ("x.r=12"); Out.Ln;
    | 13: (* > *) Write (" <= "); Out.String ("x.r=13"); Out.Ln;
    | 14: (* >=*) Write (" < "); Out.String ("x.r=14"); Out.Ln;
    | 15: (* in *) IF x.not = FALSE THEN Write ("jnc ") ELSE Write ("jc ") END; *)
END;
Write (xname); Write (')'); Writeln ('then ');*)
Write ("cmp ("); Write(yname); Write (", "); Write (xname); Writeln (');');
    Out.String ("debug(x)"); Out.Ln;
    debug (x);
    Out.String ('debug(y)'); Out.Ln; debug(y);
    IF x.not THEN t := Inv(op) ELSE t := op END;
    CASE t OF
    (*0: (* = *) Write (" sete ("); Out.String ("x.r=0"); Out.Ln;
    |  1: (* # *) Write (" setne ("); Out.String ("x.r=1"); Out.Ln;
    |  2: (* < *) Write (" setl ("); Out.String ("x.r=2"); Out.Ln;
    |  3: (* <= *) Write (" setle ("); Out.String ("x.r=3"); Out.Ln;
    |  4: (* > *) Write (" setg ("); Out.String ("x.r=4"); Out.Ln;
    |  5: (* >= *)Write (" setge ("); Out.String ("x.r=5"); Out.Ln;
	
| *) 9: (* = *) Write (" sete ("); Out.String ("="); Out.Ln;
    |  10: (* # *) Write (" setne ("); Out.String ("#"); Out.Ln;
    |  11: (* < *) Write (" setl ("); Out.String ("<"); Out.Ln;
    |  12: (* <= *) Write (" setle ("); Out.String ("<="); Out.Ln;
    |  13: (* > *) Write (" setg ("); Out.String (">"); Out.Ln;
    |  14: (* >= *)Write (" setge ("); Out.String (">="); Out.Ln;
    ELSE Out.String ('op is '); Out.Int(t, 0); Out.String ('?'); Out.Ln; HALT(0);
    END;

Convert8RegNumToName (y.r, yname);
 Write (yname); Writeln (');');

END CompareInRegs;

PROCEDURE endif*;
BEGIN
Writeln ('endif;');
END endif;

PROCEDURE elsif*;
BEGIN
Writeln ('elseif');
END elsif;

PROCEDURE else*;
BEGIN
Writeln ('else');
END else;

PROCEDURE if*;
BEGIN
Write ('if ('); 
END if;
PROCEDURE SetOpInRegs (op : LONGINT; x, y : Item);
VAR xname, yname : OSAS.Ident;
BEGIN (*Out.String("SetOpInRegs"); Out.Ln; Out.String ("op = "); Out.Int(op,0); Out.Ln;*)
(*ShowRegs;*)
(*IF op = OSAS.plus THEN op0 := 24 (*or*) ELSIF op = OSAS.minus THEN op0 := 28 (*bic*)
ELSIF op = OSAS.times THEN op0 := 0 (*and*) ELSIF op = OSAS.rdiv THEN op0 := 2 (*xor*)
ELSE OSAS.Mark("not a set operator"); op0 := 0
END ;*)
IF op = (*24*) 6 THEN (*Out.String ("+ operation or"); Out.Ln;*)
   Write ("or (")
(*ELSIF op = (*28*) 7 THEN (*Out.String ("- operation and"); Out.Ln;*)
   Write ("not (") (*and will follow*)*)
ELSIF op = (*0*) 1 THEN (*Out.String (" * operation and"); Out.Ln;*)
   Write ("and (")
ELSIF op = 2 THEN (*Out.String ("/ operation xor"); Out.Ln;*)
   Write ("xor (")
END;
ConvertRegNumToName (x.r, xname);
ConvertRegNumToName (y.r, yname);
Write (yname); Write (", "); Write (xname); Writeln (');');
(*Out.String (yname); Out.String (", "); Out.String(xname); Out.Ln*)
END SetOpInRegs;

PROCEDURE SetMinus (VAR x, y : Item);
VAR xname, yname : OSAS.Ident;
BEGIN
ConvertRegNumToName (x.r, xname); 
ConvertRegNumToName (y.r, yname);
Write ("not ("); Writeln (yname); Writeln (');');
Write ("and ("); Write (yname); Write (", "); Write (xname); Writeln (');');
END SetMinus;

PROCEDURE SetOpInRegAndMem (op : LONGINT; x, y: Item);
VAR xname : OSAS.Ident;
BEGIN (*Out.String ("op="); Out.Int (op, 0); Out.Ln; debug(x); *)
(*Out.String ("-------------------------------ShowRegs"); Out.Ln; ShowRegs;*)
IF op = (*24*) 6  THEN (*Out.String ("+ operation or"); Out.Ln;*)
   Write ("or (")
(*ELSIF op = (*28*) 7 THEN (*Out.String ("- operation and"); Out.Ln;*)
   Write ("not (") (*and will follow *)*)
ELSIF op = (*0*) 1 THEN (*Out.String (" * operation and"); Out.Ln;*)
   Write ("and (");
ELSIF op = 2 THEN (*Out.String ("/ operation xor"); Out.Ln;*)
   Write ("xor (")
END;
ConvertRegNumToName (x.r, xname);
Write (y.label); Write (", "); Write (xname); Writeln(');');
END SetOpInRegAndMem;

PROCEDURE  SetOpConstAndReg (op : LONGINT; x : Item; n : LONGINT);
VAR xname : OSAS.Ident;
BEGIN
(*
Out.String ("op="); Out.Int (op, 0); Out.Ln;
Out.String ("y.a="); Out.Int (n, 0); Out.Ln; 
*)
ConvertRegNumToName(x.r, xname);
IF op = (*24*) 6  THEN (*Out.String ("+ operation or"); Out.Ln;*)
   Write ("or (")
(*ELSIF op = (*28*) 7 THEN Out.String ("- operation and"); Out.Ln;
   Write ("not (") (*and will follow *)*)
ELSIF op = (*0*) 1 THEN (*Out.String (" * operation and"); Out.Ln;*)
   Write ("and (");
ELSIF op = 2 THEN (*Out.String ("/ operation xor"); Out.Ln;*)
   Write ("xor (")
END;
(*Write ("$"); *)Writeint(n); Write(", "); Write (xname); Writeln (');');
END SetOpConstAndReg;

PROCEDURE AddVarsInRegs (xr, yr : LONGINT);
VAR rname, xname, yname : OSAS.Ident;
BEGIN
ConvertRegNumToName (xr, xname);
ConvertRegNumToName (yr, yname);
(*Write ("addl ");Write (yname); Write(","); Writeln (xname);*)
Write ("add (");Write (yname); Write(","); Write (xname); Writeln (');');
END AddVarsInRegs;

PROCEDURE AddVarsInRegAndMem ((*r : LONGINT;*) x, y : Item); (* x.reg := x.reg + y.mem *)
VAR rx, xname : OSAS.Ident;
BEGIN
(*ConvertRegNumToName (r, rx);*)
ConvertRegNumToName (x.r, xname);
(*Write ("addl "); Write (y.label); Write (", "); Writeln (xname); (* result in the sec arg reg*) *)
Write ("add ("); Write (y.label); Write (", "); Write (xname); Writeln (');'); 
END AddVarsInRegAndMem;

PROCEDURE MulVarsInRegs (xr, yr : LONGINT); (* x.reg := x.reg + y.mem *)
VAR rname, xname, yname : OSAS.Ident;
BEGIN
ConvertRegNumToName (xr, xname);
ConvertRegNumToName (yr, yname);
(*Write ("imull "); Write (yname); Write (", "); Writeln (xname); (* result in the sec arg reg*)*)
Write ("intmul ("); Write (yname); Write (", "); Write (xname); Writeln (');'); (* result in the sec arg reg*)
END MulVarsInRegs;

PROCEDURE MulVarsInRegAndMem (r : LONGINT; x, y : Item); (* x.reg := x.reg * y.mem *)
VAR rx, xname : OSAS.Ident;
BEGIN
ConvertRegNumToName (r, rx);
ConvertRegNumToName (x.r, xname);
Write ("intmul ("); Write (y.label); Write (", "); Write (xname); Writeln (');'); (* result in the sec arg reg*)
END MulVarsInRegAndMem;

PROCEDURE SubVarsInRegs (xr, yr : LONGINT); (* x.reg := x.reg - y.reg *)
VAR rname, xname, yname : OSAS.Ident;
BEGIN
(*ConvertRegNumToName (r, rname);*)
ConvertRegNumToName (xr, xname);
ConvertRegNumToName (yr, yname);
Write ("sub (");Write (yname); Write(","); Write (xname); Writeln (');');
END SubVarsInRegs;


PROCEDURE Movl (a, b : LONGINT);
VAR aname, bname : OSAS.Ident;
BEGIN
ConvertRegNumToName (a, aname);
ConvertRegNumToName (b, bname);
Write ("mov ("); Write (aname); Write (","); Write (bname); Writeln (");");
END Movl;

PROCEDURE SubVarsInRegAndMem (r : LONGINT; x, y : Item); (* x.reg := x.reg - y.mem *)
VAR rx, xname : OSAS.Ident;
BEGIN
ConvertRegNumToName (r, rx);
ConvertRegNumToName (x.r, xname);
Write ("sub ("); Write (y.label); Write (", "); Write (xname); Writeln (");"); (* result in the sec arg reg*) 
END SubVarsInRegAndMem;

PROCEDURE Put0a(op, dst, src0, src1, shmd, shcnt: LONGINT);   (*register operation with shift count*)
BEGIN
(*
code[pc] := (((((0E00H + op)*10H + src0)*10H + dst)*20H + shcnt)*8 + shmd*2)*10H + src1; INC(pc)
*)
END Put0a;

PROCEDURE Put0b(op, dst, src0, src1, shmd, shreg: LONGINT);   (*register operation with shift reg*)
BEGIN
(*code[pc] := (((((0E00H + op)*10H + src0)*10H + dst)*10H + shreg)*10H + shmd*2)*10H + src1 + 10H; INC(pc)
*)
END Put0b;

PROCEDURE Put0c(op, dst, src0, src1, src2: LONGINT);  (*multiply*)
BEGIN
(*
code[pc] := ((((0E00H + op)*10H + dst)*10H + src2)*10H + src0)*100H + src1 + 90H; INC(pc)
*)
END Put0c;

PROCEDURE Put1(op, dst, src, imm: LONGINT);  (*register operation with immediate, no shift*)
BEGIN
(*
code[pc] := (((0E20H + op)*10H + src)*10H + dst)*1000H + imm; INC(pc)
*)
END Put1;

PROCEDURE LoadRegImmediate (l, r : LONGINT);
VAR name, s : OSAS.Ident;
BEGIN
   ConvertRegNumToName (r, name);
   IntStr.IntToStr (l, s);
	Write ("mov ("); Write (s); Write (", "); Write (name); Writeln (");");
END LoadRegImmediate;

PROCEDURE negreg(r : LONGINT);
VAR name : OSAS.Ident;
BEGIN
   ConvertRegNumToName (r, name);
   Write ("neg ("); Write (name); Writeln (");");
END negreg;

PROCEDURE Put1a(op, dst, src, imm, rot: LONGINT);  (*register operation with immediate*)
BEGIN
(*
code[pc] := ((((0E20H + op)*10H + src)*10H + dst)*10H + rot)*100H + imm; INC(pc)
*)
END Put1a;

PROCEDURE LoadReg (VAR varname : Item; regnum : LONGINT);
VAR name : OSAS.Ident;
BEGIN
   ConvertRegNumToName (regnum, name);
   IF varname.type.size = 4 THEN
   Write ("mov (");
   ELSIF varname.type.size = 1 THEN
   (*Write ("movsbl ");*)
   Write ("movsx (");
   END;
   IF varname.type.form = OSAB.Proc THEN
      Write ('&');
   END;
   Write (varname.label); Write (", "); Write (name); Writeln (");");
END LoadReg;

PROCEDURE Put2(op, reg, base, offset: LONGINT);  (*Load/Store with offset literal*)
VAR temp, op1: LONGINT;
BEGIN
(*
 temp := base;
IF (base = PC) & (offset # 0) THEN offset := offset - pc*4 - 8 END ;
IF offset < 0 THEN offset := -offset; op1 := 4 ELSE INC(op, 8); op1 := 8 END ;
IF offset >= 100000H THEN OSAS.Mark("offset too large")
ELSIF offset >= C12 THEN GetReg(temp); Put1a(op1, temp, base, offset DIV 1000H, 10); Release(temp)
END ;
code[pc] := (((0E40H + op)*10H + temp)*10H + reg)*C12 + offset MOD C12; INC(pc)
*)
END Put2;

PROCEDURE StoreReg (y, x : Item);
VAR xname, yname : OSAS.Ident;
BEGIN Out.String ('StoreReg'); Out.Ln;
IF x.not THEN Out.String ('x.not = true'); Out.Ln END;
debug(x);
IF y.not THEN Out.String ('y.not = true'); Out.Ln END; debug (y);

   IF x.type.size = 4 THEN
      (*ConvertRegNumToName (x.r, xname);*)
      ConvertRegNumToName (y.r, yname);
      Write ("mov ("); 
      Write (yname); 
   ELSIF x.type.size = 1 THEN
      Convert8RegNumToName(y.r, yname);
      Write ("mov (");
      Write (yname)
   END;
      Write (', '); Write (x.label); Writeln (");");
END StoreReg;

PROCEDURE Put3(op, reg, base, offreg, shift: LONGINT);  (*Load/Store with offset from register*)
BEGIN
(*
code[pc] := ((((0E60H + op)*10H + base)*10H + reg)*20H + shift)*80H + offreg; INC(pc)
*)
END Put3;

PROCEDURE Put4(op, base: LONGINT; regs: SET);  (*Load/Store multiple*)
BEGIN
(*
code[pc] := ((0E80H + op)*10H + base)*10000H + SYSTEM.VAL(LONGINT, regs); INC(pc)
*)
END Put4;

PROCEDURE Put5(cond, offset: LONGINT);  (*Branch conditional*)
BEGIN
(*
code[pc] := (cond*10H + 10)*1000000H + offset MOD 1000000H; INC(pc)
*)
END Put5;

PROCEDURE Put5a(offset: LONGINT);  (*Branch and Link*)
BEGIN
(*
code[pc] := -352321536 (*0EB000000H*) + offset MOD 1000000H; INC(pc)
*)
END Put5a;

PROCEDURE Put6(cond, num: LONGINT);  (*SWI*)
BEGIN
(*
code[pc] := (cond*10H + 15) * 1000000H + num; INC(pc)
*)
END Put6;

PROCEDURE PutC(u: LONGINT);
BEGIN
(*
 code[pc] := u; INC(pc)
 *)
END PutC;

PROCEDURE SetCC(VAR x: Item; n: LONGINT);
BEGIN
 x.mode := CC; x.a := labelnum + 1; x.b := 0; (*x.r := n;*)
END SetCC;

PROCEDURE negated(cond: LONGINT): LONGINT;
VAR c: LONGINT;
BEGIN
IF ODD(cond) THEN c := cond-1 
ELSE Out.Ln; 
c := cond+1; 
END ;
RETURN c
END negated;

(*handling of forward reference, fixups of branch addresses and constant tables*)

PROCEDURE merged(L0, L1: LONGINT): LONGINT;
VAR L2, L3: LONGINT;
BEGIN 

IF L0 # 0 THEN L3 := L0;
REPEAT L2 := L3; L3 := code[L2] MOD C24 UNTIL L3 = 0;
code[L2] := code[L2] - L3 + L1; L1 := L0
END ;
RETURN L1

END merged;

PROCEDURE fix24(at: LONGINT);
BEGIN
(*
 code[at] := code[at] DIV C24 * C24 + ((pc - at - 2) MOD C24)
 *)
END fix24;

PROCEDURE fix12(at: LONGINT);
VAR d: LONGINT;
BEGIN
(*
 d := (pc - at - 2)*4; (*d >= 0, fixup LDR*)
IF d < C12 THEN code[at] := code[at] DIV C12 * C12 + d
ELSE OSAS.Mark("const not allocatable")
END

*)
END fix12;

PROCEDURE FixLink*(L0: LONGINT);
VAR L1: LONGINT;
BEGIN
Write ('_'); Write (modname); Write ('_');
Write (labelnumber); Writeint (L0+1); Writeln (":");
(*
WHILE L0 # 0 DO L1 := code[L0] MOD C24; fix24(L0); L0 := L1 END
*)
END FixLink;

PROCEDURE FixLinkWith(L0, dst: LONGINT);
VAR L1: LONGINT;
BEGIN
(*
WHILE L0 # 0 DO
L1 := code[L0] MOD C24;
code[L0] := code[L0] DIV C24 * C24 + ((dst - L0 - 2) MOD C24); L0 := L1
END
*)
END FixLinkWith;


PROCEDURE enterIC(k: LONGINT);  (*enter integer constant in TIC*)
BEGIN

   IF icx < ITlen THEN
      TIC[icx].val := k; TIC[icx].adr := pc; INC(icx);
         IF firstfixloc = 0 THEN firstfixloc := pc END
   ELSE OSAS.Mark("too many integer constants"); icx := 0
END

END enterIC;

PROCEDURE enterStr(k: LONGINT);  (*enter string index in TSC*)
BEGIN
(*
IF scx < ITlen THEN
TSC[scx].index := k; TSC[scx].adr := pc; INC(scx);
IF firstfixloc = 0 THEN firstfixloc := pc END
ELSE OSAS.Mark("too many strings"); scx := 0
END
*)
END enterStr;

PROCEDURE ScaleConst(VAR n, s: LONGINT);
VAR m: INTEGER;
BEGIN
(*
 m := 16;
IF n # 0 THEN
WHILE n MOD 4 = 0 DO n := SYSTEM.LSH(n, -2); DEC(m) END
END ;
s := m MOD 16
*)
END ScaleConst ;

PROCEDURE FixupConstants;
VAR i, j, k, disp, s, pc0: LONGINT;
BEGIN
(*
 i := 0;
WHILE i < icx DO
IF TIC[i].adr # 0 THEN
fix12(TIC[i].adr); j := i+1;
WHILE j < icx DO
IF (TIC[j].adr # 0) & (TIC[j].val = TIC[i].val) THEN fix12(TIC[j].adr); TIC[j].adr := 0 END ;
INC(j)
END ;
PutC(TIC[i].val)
END ;
INC(i)
END ;
i := 0;
WHILE i < xrefx DO
IF TXR[i].adr # 0 THEN
fix12(TXR[i].adr); k := TXR[i].ref; j := i+1;
WHILE j < xrefx DO
IF (TXR[j].adr # 0) & (TXR[j].ref = k) THEN fix12(TXR[j].adr); TXR[j].adr := 0 END ;
INC(j)
END ;
PutC((k MOD 10000H) * 10000H + fixlist[k DIV 10000H]); fixlist[k DIV 10000H] := pc - 1
END ;
INC(i)
END ;
pc0 := pc; OSAS.MoveStrings(code, pc, maxCode); i := 0;
WHILE i < scx DO
j := TSC[i].index; k := TSC[i].adr;
disp := (pc0 + j - k - 2) * 4;
ScaleConst(disp, s);   (*disp in words*)
IF disp < C8 THEN code[k] := s*C8 + disp + code[k]  (*fixup*)
ELSE OSAS.Mark("string not allocatable")
END ;
INC(i)
END ;
icx := 0; scx := 0; xrefx := 0; firstfixloc := 0
*)
END FixupConstants;

(* loading of operands and addresses into registers *)

PROCEDURE ExtRef(VAR x: Item; r, d: LONGINT);
BEGIN
(*
IF xrefx < ITlen THEN
TXR[xrefx].ref := - x.b * 10000H + x.a + d; TXR[xrefx].adr := pc; INC(xrefx);
IF firstfixloc = 0 THEN firstfixloc := pc END
ELSE OSAS.Mark("too many external refs"); xrefx := 0
END ;
Put2(LDR, r, PC, 0)
*)
END ExtRef;

PROCEDURE load(VAR x: Item);
VAR r, cd, n, s: LONGINT;
BEGIN (*Out.String ("entered load"); Out.Ln;*)
   IF x.mode # OSAB.Reg THEN (*Out.String ("x.mode # OSAB.Reg"); Out.Ln;*)
      IF x.type.size = 1 THEN cd := LDR+4 ELSE cd := LDR END ;
      IF x.mode = OSAB.Var THEN (*Out.String ("loading variable "); Out.String (x.label); Out.Ln;*)
         IF x.b >= 0 THEN GetReg1(r, x.r); 
	 (*Out.String ("Loading variable "); Out.String (x.label); Out.String (" to the register "); Out.Int (r, 0); Out.Ln;*)
	 LoadReg(x, r);             (* movl x.label, r *) (*if x.r = 15 (v smysle pc)   *)
	 (*Put2(cd, r, x.r, x.a)*)
         ELSE GetReg(r); ExtRef(x, r, 0); (*Put2(LDR, r, r, 0)*)
	    LoadReg (x, r);
         END
      ELSIF x.mode = OSAB.Par THEN 
         GetReg1(r, x.r); (*Put2(LDR, r, x.r, x.a); Put2(cd, r, r, x.b)*)
         LoadReg(x, r);
      ELSIF x.mode = OSAB.RegI THEN GetReg1(r, x.r); Put2(cd, r, x.r, x.a)
      ELSIF x.mode = RegX THEN GetReg2(r, x.r, x.a); Put3(cd+8, r, x.r, x.a, x.b)
      ELSIF x.mode = OSAB.Const THEN
         IF x.type.form <= OSAB.Set THEN
         GetReg(r); n := x.a;            
	    IF n = 0 THEN
	    (*Out.String ("loading 0 to the register "); Out.Int (r, 0); Out.Ln;*)
	    LoadRegImmediate (0, r);
	    (*Put1(MOV, r, 0, 0)*)
            (*
	    ELSIF (n > 0) OR (n <= -C8) THEN(*asdaras I understand this is because of arm limitations*)
               ScaleConst(n, s);
                  IF n DIV C8 = 0 THEN Put1a(MOV, r, 0, n, s)
                  ELSE enterIC(x.a); Put2(LDR, r, PC, 0)
                  END
		  *)
            ELSE (*Put1(MVN, r, 0, -n-1)*)
	      (* Out.String ("loading "); Out.Int (x.a,0); Out.String ("to the register "); Out.Int (r, 0); Out.Ln;*)
	          (*IF x.a < 0 THEN (*this may not be necessary*)
	          LoadRegImmediate (-x.a, r);
	          negreg(r);
                  ELSE*)
	          LoadRegImmediate (x.a(*-n-1*), r)
		  (*END*)
            END
	    
         ELSIF x.type.form = OSAB.Proc THEN
            GetReg(r);
            IF x.b >= 0 THEN
               (*n := pc + 2 - x.a; Put1a(SUB, r, PC, n MOD C8, 15);
                  IF n >= C8 THEN Put1a(SUB, r, r, n DIV C8, 11) END*)
	       LoadReg(x, r); 
            ELSE ExtRef(x, r, 100H);
            END ;
         ELSIF x.type.form = OSAB.NilTyp THEN GetReg(r); Put1(MOV, r, 0, 0)
         END
      ELSIF x.mode = CC THEN GetReg(r); Out.String ("IF x.mode = CC THEN GetReg(r)"); Out.Ln;
         IF (x.a = 0) & (x.b = 0) THEN Out.String ("(x.a = 0) & (x.b = 0)"); Out.Ln;
	 (*LoadReg (x, r);*)
            (*Put1((x.r+2)*100H + MOV, r, 0, 1); Put1((negated(x.r)+2)*100H + MOV, r, 0, 0)*)
         ELSE (*Put5(x.r, 1); FixLink(x.a); Put1(MOV, r, 0, 0); Put5(14, 0); FixLink(x.b); Put1(MOV, r, 0, 1)*)
         END
      ELSE OSAS.Mark("illegal object (type?)");
         (*Out.String("load"); Out.Int(x.mode, 4); Out.Int(x.a, 5); Out.Int(x.b, 5);
         Out.Ln*)
      END ;
   x.mode := OSAB.Reg; x.r := r; debug(x); (*Out.String ("x.mode := OSAB.Reg; x.r := r"); Out.Ln; debug(x);*)
   END
END load;

PROCEDURE loadAdr(VAR x: Item);
VAR n, r, s, temp: LONGINT;
BEGIN
(*
IF (x.mode = OSAB.Var) & (x.b >= 0) OR (x.mode = OSAB.RegI) THEN
IF x.r = PC THEN x.a := x.a -  LONG(pc)*4 - 8   (*global*) END ;
GetReg1(r, x.r); n := x.a;
IF n < 0 THEN  n := -n; 
ScaleConst(n, s);
Put1a(SUB, r, x.r, n MOD C8, s MOD 10H);
IF n >= C8 THEN
n := n DIV C8; Put1a(SUB, r, r, n MOD C8, (s-4) MOD 10H);
IF n >= C8 THEN
n := n DIV C8; Put1a(SUB, r, r, n MOD C8, (s-8) MOD 10H);
IF n >= C8 THEN OSAS.Mark("adr offset too large") END
END
END
ELSIF n > 0 THEN
ScaleConst(n, s);
Put1a(ADD, r, x.r, n MOD C8, s MOD 10H);
IF n >= C8 THEN
n := n DIV C8; Put1a(ADD, r, r, n MOD C8, (s-4) MOD 10H);
IF n >= C8 THEN OSAS.Mark("field offset too large") END
END ;
ELSE r := x.r
END
ELSIF x.mode = OSAB.Var THEN GetReg(r); ExtRef(x, r, 0)
ELSIF x.mode = OSAB.Par THEN GetReg1(r, x.r); Put2(LDR, r, x.r, x.a);
IF x.b # 0 THEN
n := x.b; ScaleConst(n, s); Put1a(ADD, r, r, n MOD C8, s MOD 10H);
IF n >= C8 THEN OSAS.Mark("field offset too large") END
END
ELSIF x.mode = RegX THEN GetReg2(r, x.r, x.a); Put0a(ADD, r, x.r, x.a, 0, x.b)
ELSIF x.mode = OSAB.Const THEN OSAS.Mark("variable expected")
ELSE OSAS.Mark("bad mode in loadAdr")
END ;
x.mode := OSAB.RegI; x.r := r
*)
END loadAdr;

PROCEDURE loadCC(VAR x: Item);
BEGIN Out.String ("loadCC"); Out.Ln; debug(x);
IF x.type.form = OSAB.Bool THEN Out.String ("x.type.form=OSAB.Bool"); Out.Ln;
   IF x.mode = OSAB.Const THEN Out.String ("x.mode=OSAB.Const"); Out.Ln;(*Put0(CMP, 0, 0, 0); *)
   SetCC(x, (*1-x.a*)labelnum-1); 
   ELSE load(x); 
   Release(x.r); (* comment! *)
   SetCC(x, (*1*)labelnum); 
   END
ELSE OSAS.Mark("must be Boolean"); x.a := 0; x.b := 0
END
END loadCC;

PROCEDURE loadArrayLen(VAR x: Item);
VAR z: Item;
(*
BEGIN z.type := OSAB.intType;
IF x.type.len >= 0 THEN z.mode := OSAB.Const; z.a := x.type.len
ELSE (*open array*)
IF x.mode = OSAB.Par THEN z.mode := OSAB.Var; z.r := x.r; z.a := x.a - 4
ELSE OSAS.Mark("bad array parameter")
END
END ;
load(z)
*)
END loadArrayLen;

PROCEDURE loadString(VAR x: Item);
VAR r, xL: LONGINT;
BEGIN
(*
 xL := x.a DIV 10000H; x.a := x.a MOD 10000H;
IF x.mode = OSAB.Const THEN
GetReg(r); x.mode := OSAB.Reg; x.r := r; enterStr(x.a); Put1(ADD, r, PC, 0)
ELSE loadAdr(x); x.b := xL
END
*)
      IF x.mode = OSAB.Const THEN
      GetReg(r);
      x.mode := OSAB.Reg;
      x.r := r;
      END;
END loadString;

(*
PROCEDURE AllocString*(VAR adr: LONGINT);
(*String Constant declaration; moves string into code array*)
BEGIN adr := OSAS.slen*10000H + pc*4; OSAS.MoveStrings(code, pc, maxCode)
END AllocString;
*)
(*PROCEDURE Consts*;
BEGIN
Writeln ('READONLY');
Writelnh ('READONLY');
END Consts;*)

PROCEDURE AllocString*(VAR o : OSAB.Object);
(*String Constant declaration; moves string into code array*)
VAR tmpstr : OSAS.Ident;
label : OSAS.Ident;
i, j : LONGINT;
BEGIN 
Writeln ('READONLY');
COPY('', label);
(*Strings.Append ('.', label);*)
Strings.Append (modname, label);
Strings.Append ('_', label);
Strings.Append (o.name, label);
Write (label); COPY(label, o.label);
Write (' : char[');
j := OSAS.length(OSAS.name);
Writeint (j+1);
Write ('] :='); Write ('['); 
FOR i := 0 TO j DO
      IF ORD(OSAS.name[i]) >= 32 THEN 
         Write ("'");
         WriteChar(OSAS.name[i]);
	 Write ("'");
      ELSE
        Write ('#'); Writeint (ORD(OSAS.name[i])); 
      END;
      IF i # j THEN Write (', ') END
END;
Writeln ('];');
IF o.expo THEN
Writelnh ('READONLY'); Writeh(label);
Writeh (' : char[');
j := OSAS.length(OSAS.name);
Writeinth (j+1);
Writelnh (']; external;')
END;
(*Write (OSAS.name);*)
(*
OSAS.CopyId(tmpstr);
Write (tmpstr);
*)
END AllocString;

PROCEDURE AllocVariablesInit*(gl : BOOLEAN);
BEGIN
    IF gl THEN
    Writeln ("static");
    Writelnh ('static');
    ELSE
    (*Writeln ('var')*)
    Writeln ('static')
    END
END AllocVariablesInit;    

PROCEDURE AllocVariable*(VAR o : OSAB.Object);
VAR label: OSAS.Ident;ch : CHAR;
tmpobj, tmpobj2 : OSAB.Object; b : BOOLEAN;
BEGIN
    COPY ('', label);  
(*       IF o.expo THEN
          Strings.Append (modname, label);
          Write ("       .comm   ");
       ELSE
          Write ("       .lcomm   ");
       END;*)
       (*Out.String ("generating variable for "); Out.String (o.name); Out.Ln;*)
       (*Out.Ln; Out.Ln; Out.Ln; Out.Ln;
       DebugObj(o); Out.Ln; Out.Ln; in.Char(ch); DebugObj(o.type.dsc); in.Char(ch);
       Out.Ln; DebugObj(o.type.dsc.next); in.Char(ch); Out.Ln;
       DebugObj(o.type.dsc.next.next); in.Char(ch); Out.Ln;
       DebugObj(o.type.dsc.next.next.next); in.Char(ch); Out.Ln; (*this is dereference of nil because no record fields *)   *)
    Strings.Append (modname, label);
    Strings.Append ("_", label);
    Strings.Append (o.name, label);
    Write (label); COPY (label, o.label);
    IF o.type.form = OSAB.Bool THEN
    Write (' : boolean');
     IF o.lev < 1 THEN
        Write (' := false');
     END;
     Write (';');
    ELSIF o.type.form = OSAB.Int THEN
       Write (' : int');
       Writeint (o.type.size * 8); 
          IF o.lev < 1 THEN
	  Write (' :=0');
	  END;
       Write ('; ');
    ELSIF o.type.form = OSAB.Char THEN
    Write (' : char');
       IF o.lev < 1 THEN
            Write (" := ' '")
       END;
       Write(';');
    ELSIF o.type.form = OSAB.Set THEN
       Write (' : int');
       Writeint (o.type.size * 8); 
          IF o.lev < 1 THEN 
	     Write (' := 0');
	  END;
       Write ('; ');
    ELSIF o.type.form = OSAB.Proc THEN
       (*Write (' : dword; ');*)
       (*DebugObj(o);Out.Ln; Out.Ln;  DebugObj(o.type.dsc); Out.Ln; Out.Ln; DebugObj(o.type.dsc.next); HALT(0);*)

       Write (' : procedure (');
          
	  tmpobj := o.type.dsc; b := FALSE;
	  DebugObj(tmpobj); (*in.Char(ch);*)
          REPEAT
          (*IF (o.class = OSAB.Par) OR (o.class = OSAB.Const) THEN Write (' var ') END;*)
	  tmpobj.label := tmpobj.name;
          Strings.Append ('_', tmpobj.label);
          (*tmpobj.label := modname;
          Strings.Append ('_', tmpobj.label);
          Strings.Append (obj.name, tmpobj.label);
          Strings.Append ('_', tmpobj.label);
          Strings.Append (tmpobj.name, tmpobj.label);*)
          Write (tmpobj.label); Write (' : ' ); 
           IF    tmpobj.type.form = OSAB.Int THEN Write (' int32')
	   ELSIF tmpobj.type.form = OSAB.Char THEN Write ( 'char');
	   ELSIF tmpobj.type.form = OSAB.Bool THEN Write ('boolean');
	   ELSIF tmpobj.type.form = OSAB.Set THEN Write ('int32');
	   ELSIF tmpobj.type.form = OSAB.Pointer THEN Write ('dword');
	   ELSIF tmpobj.type.form = OSAB.Proc THEN Write ('dword');
	   ELSIF tmpobj.type.form = OSAB.Array THEN Write ('dword');
	   END;
      IF tmpobj.next # NIL THEN
         tmpobj2 := tmpobj.next;
         IF tmpobj2.next.name # tmpobj2.name THEN
             tmpobj := tmpobj.next;
	     Write ('; ');
	 ELSE b := TRUE;
	 END;
              DebugObj(tmpobj); (*in.Char(ch);*)
      ELSE b := TRUE
      END
    UNTIL b;
    Writeln (');');
    ELSIF o.type.form = OSAB.Array THEN
       Out.String ('o.type.base.form is '); Out.Int (o.type.base.form, 0); Out.Ln;
       Out.String ('o.type.base.size is '); Out.Int (o.type.base.size, 0); Out.Ln;
       Out.String ('o.type.len is '); Out.Int (o.type.len, 0); Out.Ln;
       Write (' : '); 
          IF (o.type.base.form = OSAB.Char) OR (o.type.base.form = OSAB.Bool) THEN
		Write (' char');
	  ELSIF (o.type.base.form = OSAB.Int) OR (o.type.base.form = OSAB.Set) THEN
	     Write ('int'); Writeint (o.type.base.size*8);
	  END;
	  Write ('[');
	  Writeint (o.type.len);
	  Writeln ('];');
    ELSIF o.type.form = OSAB.Record THEN
      Write (' : '); Write (o.type.typobj.label); Writeln (';');
    END;
       (*IF o.expo THEN Writeln ('external;') END;*)
       Writeln ('');   
   

       IF o.expo THEN
         Writeh (label);
	IF o.type.form = OSAB.Bool THEN

    Writeh (' : boolean := false;');
    ELSIF o.type.form = OSAB.Int THEN
       Writeh (' : int');
       Writeinth (o.type.size * 8); Writeh ('; ');
    ELSIF o.type.form = OSAB.Char THEN
    Writeh (" : char ;");
    ELSIF o.type.form = OSAB.Set THEN
       Writeh (' : int');
       Writeinth (o.type.size * 8); Writeh ('; ');
    END;
 
         Writelnh ('external;');
       END;
END AllocVariable;

PROCEDURE Q(T: OSAB.Type);
VAR obj: OSAB.Object;
BEGIN
(*
IF T.base # NIL THEN
Q(T.base); obj := T.typobj;
PutC(obj.val * 10000H + fixlist[-obj.lev]); fixlist[-obj.lev] := pc-1
END
*)
END Q;

PROCEDURE AllocTD*(obj: OSAB.Object);
VAR n: INTEGER; tp: OSAB.Type;
BEGIN

 obj.lev := 0; obj.val := pc*4; tp := obj.type;
IF tp.form = OSAB.Pointer THEN tp := tp.base END ;
(*PutC(tp.size);*) (*does not include tag*)
Q(tp); n := tp.nofpar; 
WHILE n < 2 DO PutC(0); INC(n) END ;

END AllocTD;

(* Items: Conversion from constants or from Objects on the Heap to Items on the Stack*)

PROCEDURE Val*(VAR x: Item): LONGINT;
BEGIN 
(*Out.String ("x.a="); Out.Int(x.a, 0); Out.Ln;*)
RETURN x.a
END Val;

PROCEDURE Lev*(VAR x: Item): LONGINT;
BEGIN RETURN x.b
END Lev;

PROCEDURE MakeConstItem*(VAR x: Item; typ: OSAB.Type; val: LONGINT);
VAR s : ARRAY 128 OF CHAR;
BEGIN 
   x.mode := OSAB.Const; 
   x.type := typ; 
   IF x.type.form = OSAB.Char THEN
      IF OSAS.name = "" THEN Out.String ("''"); Out.Ln END;
      IF (OSAS.name = "") THEN
         x.a := ORD(CHR(val));
      ELSE
         COPY (OSAS.name, s);
	 x.a := ORD(s[0])
      END;
      (*
      IF (*val = 0*) OSAS.name # "" THEN
         COPY (OSAS.name, s);
         (*OSAS.CopyId(s);*)
         Out.String ('VNIMANIE!'); Out.Ln;
         Out.String (s); Out.Ln;
         x.a := ORD(s[0])
      ELSE
         x.a := ORD(CHR(val));
      END;*)
   ELSE
      x.a := val;
   END;
x.label := ""; x.ilabel := "";
x.and := FALSE; x.not := FALSE; x.or := FALSE
END MakeConstItem;

PROCEDURE MakeRealItem*(VAR x: Item; val: REAL);
BEGIN x.mode := OSAB.Const; x.type := OSAB.realType; x.a := SYSTEM.VAL(LONGINT, val);
x.not := FALSE; x.and := FALSE; x.or := FALSE; x.ilabel := "";
END MakeRealItem;

PROCEDURE MakeStringItem*(VAR x: Item; inx, len: LONGINT);
BEGIN x.mode := OSAB.Const; x.type := OSAB.strType; x.a := inx; x.b:= len; 
COPY (OSAS.name, x.label); x.ilabel := "";
x.not := FALSE; x.and := FALSE; x.or := FALSE; 
END MakeStringItem;

PROCEDURE MakeItem*(VAR x: Item; y: OSAB.Object);
VAR cl: INTEGER; r: LONGINT;
BEGIN cl := y.class; x.mode := cl; x.type := y.type; x.a := y.val; x.b := y.lev; x.rdo := y.rdo; 
x.not := FALSE; x.and := FALSE; x.or := FALSE;  COPY (y.label, x.label); x.ilabel := "";
   IF (y.lev >= 0) THEN
      IF y.label = '' THEN
         COPY (modname, x.label);
	 Strings.Append ('_', x.label);
	 Strings.Append (y.name, x.label)
      END;
   END;
   IF y.lev = -1 THEN
      COPY(y.ilabel, x.ilabel);
      COPY(x.ilabel, x.label);
      Strings.Append ('_', x.label);
      Strings.Append (y.name, x.label);
   END;
   IF cl = OSAB.Var THEN
      IF y.lev <= 0 THEN x.r := PC;
         IF x.type.form = OSAB.String THEN (*named string constant*) x.mode := OSAB.Var END ;
      ELSIF y.lev = curlev THEN x.r := FP
      ELSE OSAS.Mark("non-local not accessible"); x.r := FP
      END
   ELSIF cl = OSAB.Par THEN x.b := 0;
      IF y.lev <= 0 THEN x.r := PC
      ELSIF y.lev = curlev THEN x.r := FP
      ELSE OSAS.Mark("non-local not accessible"); x.r := FP
      END
   ELSIF cl = OSAB.Typ THEN x.r := PC 
   ELSIF cl = OSAB.Reg THEN x.r := y.val
   ELSIF cl = OSAB.RegI THEN x.r := y.val; x.a := 0
   END;
END MakeItem;

(* Code generation for Selectors, Variables, Constants *)

PROCEDURE AddConst(op: LONGINT; VAR x: Item; n: LONGINT);
VAR r0, r1, n0, s: LONGINT;  (*n >= 0*)
BEGIN n0 := n; 
(*ScaleConst(n0, s); *)
 GetReg1(r0, x.r);
IF n0 DIV C8 = 0 THEN Put1a(op, r0, x.r, n0, s)
ELSE enterIC(n); GetReg(r1); Put2(LDR, r1, PC, 0); Put0(op, r0, x.r, r1); Release(r1)
END ;
x.r := r0
END AddConst;

PROCEDURE AddConstToVar (VAR x : Item; n : LONGINT);
VAR r : LONGINT;
rname, nl : OSAS.Ident;
BEGIN
GetReg1 (r, x.r);
ConvertRegNumToName ((*x.r*)r, rname);
IntStr.IntToStr(n, nl);
(*Write ("addl $"); Write (nl); Write (", "); Writeln (rname)*)
Write ("add ("); Write (nl); Write (", "); Write (rname); Writeln (");");
END AddConstToVar;

PROCEDURE SubConstToVar (VAR x : Item; n : LONGINT);
VAR r : LONGINT;
rname, nl : OSAS.Ident;
BEGIN
GetReg1 (r, x.r);
ConvertRegNumToName (r, rname);
IntStr.IntToStr(n, nl);
(*Write ("subl $"); Write (nl); Write (", "); Writeln (rname)*)
Write ("sub ("); Write (nl); Write (", "); Write (rname); Writeln (");");
END SubConstToVar;

PROCEDURE MulConstToVar (VAR x : Item; n : LONGINT);
VAR r : LONGINT;
rname, nl : OSAS.Ident;
BEGIN
GetReg1 (r, x.r);
ConvertRegNumToName ((*x.r*)r, rname);
IntStr.IntToStr(n, nl);
(*Write ("imull $"); Write (nl); Write (", "); Writeln (rname)*)
Write ("intmul ("); Write (nl); Write (", "); Write (rname); Writeln (');');
END MulConstToVar;

PROCEDURE Field*(VAR x: Item; y: OSAB.Object);   (* x := x.y *)
VAR r: LONGINT;
BEGIN
(*debug(x); DebugObj(y);*)
x.type := y.type;
   IF (x.mode = OSAB.Var) OR (x.mode = OSAB.RegI) THEN x.a := x.a + y.val;
      Strings.Append ('.', x.label); Strings.Append (y.label, x.label);
   ELSIF x.mode = OSAB.Par THEN x.b := x.b +  y.val
   ELSIF x.mode = RegX THEN
      GetReg2(r, x.r, x.a); Put0a(ADD, r, x.r, x.a, 0, x.b); x.mode := OSAB.RegI; x.r := r; x.a := y.val
   ELSE OSAS.Mark("bad mode in Field")
   END ;
END Field;

PROCEDURE Index*(VAR x, y: Item; check: BOOLEAN);   (* x := x[y] *)
VAR r, r0, s, s0, n, lim: LONGINT; (* y.a is index *)
str,offset, reg0, regy, xname, yname, rname, tmpstr, tsize : OSAS.Ident;
BEGIN s := x.type.base.size; lim := x.type.len; Out.String ('Index');Out.Ln; Out.String ('x.type.base.size='); Out.Int(s,0); Out.Ln;
           Out.String ('x.type.len='); Out.Int(lim,0);
	   debug(x);
	   Out.String ('y'); Out.Ln;
           debug(y);
   IF (y.mode = OSAB.Const) & (lim >= 0) THEN
      IF (y.a >= 0) & (y.a < lim) THEN
         IF (x.mode = OSAB.Var) OR (x.mode = OSAB.RegI) THEN x.a := y.a * s + x.a;
	    (*IntStr.IntToStr(y.a*s,offset);*)
	    IntStr.IntToStr(y.a* s,offset);
	    (*Strings.Append ('+', x.label); Strings.Append(offset, x.label);*)
	    Strings.Append ('[', x.label); Strings.Append (offset, x.label); Strings.Append (']', x.label);
         ELSIF (x.mode = OSAB.Par) & (y.mode = OSAB.Const) THEN (*x.b := y.a * s + x.b;
        	    (*IntStr.IntToStr(y.a*s,offset);*)
	    IntStr.IntToStr(y.a*s,offset);
	    (*Strings.Append ('+', x.label); Strings.Append(offset, x.label);*)
	    Strings.Append ('[', x.label); Strings.Append (offset, x.label); Strings.Append (']', x.label);
       *)
		GetReg1(r, x.r); x.r := r;
		      ConvertRegNumToName (x.r, xname);
		      Write ('mov ('); Write (x.label); Write (', '); Write (xname); Writeln (');');
		      x.mode := OSAB.RegI; x.r := r; x.a := x.b;
		      (*GetReg (r);
		      ConvertRegNumToName (r, rname);
		      Write ('mov (['); Write (xname);  Write (', '); Write (rname); Writeln (');');*)
      
		      (* mov (3, (type int32 [edx+1*4])); right! *)
		      COPY ('(type ', tmpstr);
		      IF x.type.base.form = OSAB.Bool THEN
		         Strings.Append ('boolean ', tmpstr);
		      ELSIF x.type.base.form = OSAB.Char THEN
		         Strings.Append ('char ', tmpstr);
		      ELSIF x.type.base.form = OSAB.Int THEN
		          Strings.Append ('int32 ', tmpstr);
		      ELSE Out.String ('unknown type in Index, add it to if-else'); Out.Ln; 
		      Out.String ('x.type.form is '); Out.Int (x.type.form, 0); Out.Ln;
		      Out.String ('x.type.base.form is '); Out.Int (x.type.base.form, 0); Out.Ln;
		      HALT(0);
		      END;
		      Strings.Append ('[', tmpstr); Strings.Append (xname, tmpstr); Strings.Append ('+', tmpstr);
			IntStr.IntToStr (y.a, offset);
		        Strings.Append (offset, tmpstr);
		        Strings.Append ('*', tmpstr);
		        IntStr.IntToStr (s, tsize);
		        Strings.Append (tsize, tmpstr);
		        Strings.Append ('])', tmpstr);
		        COPY (tmpstr, x.label);
	 ELSIF (x.mode = OSAB.Par) & (y.mode = OSAB.Var) THEN
	      GetReg1(r, x.r); x.r := r;
	      ConvertRegNumToName (x.r, xname);
	      Write ('mov ('); Write (x.label); Write (', '); Write (xname); Writeln (');');
	      x.mode := OSAB.RegI; x.r := r; x.a := x.b;
              load(y);
	      ConvertRegNumToName (y.r, yname);
	      Write ('shl (2, '); Write (yname); Writeln (');');
	      Write ('add ('); Write (yname); Write (', '); Write (xname); Writeln (');');
	      Release (y.r); y.mode := OSAB.Var;
		COPY ('(type ', tmpstr);
	      IF x.type.base.form = OSAB.Bool THEN
	         Strings.Append ('boolean ', tmpstr);
	      ELSIF x.type.base.form = OSAB.Char THEN
	         Strings.Append ('char ', tmpstr);
	      ELSIF x.type.base.form = OSAB.Int THEN
	          Strings.Append ('int32 ', tmpstr);
	      ELSE Out.String ('unknown type in Index, add it to if-else'); Out.Ln; 
	      Out.String ('x.type.form is '); Out.Int (x.type.form, 0); Out.Ln;
	      Out.String ('x.type.base.form is '); Out.Int (x.type.base.form, 0); Out.Ln;
	      HALT(0);
	      END;
	      Strings.Append ('[', tmpstr); Strings.Append (xname, tmpstr); Strings.Append ('])', tmpstr);
	      COPY (tmpstr, x.label);
         ELSIF x.mode = RegX THEN

            Put0a(ADD, x.a, x.r, x.a, 0, x.b); Release(x.b); x.mode := OSAB.RegI; x.a := y.a * s;
         ELSE OSAS.Mark("bad mode in Index")
         END
      ELSE OSAS.Mark("index out of range")
      END ;
   ELSE (*load(y);*)
      (*IF check THEN (*check index bounds*)
         IF lim >= 0 THEN (*AddConst(CMP, y, lim)*)

         ELSE (*open array*)
            IF x.mode <= OSAB.Par THEN GetReg(lim); Put2(LDR, lim, x.r, x.a - 4); Release(lim)
            ELSIF x.mode = OSAB.RegI THEN lim := x.r - 1
            ELSE OSAS.Mark("bad mode in Index")
            END ;
            Put0(CMP, 0, y.r, lim)
         END ;
         Put6(2, 1)   (*SWI*)
      END ;*)
       
      IF (x.mode = OSAB.Par) & (y.mode = OSAB.Const) THEN
      (*   GetReg1(r, x.r); Put2(LDR, r, x.r, x.a); x.mode := OSAB.RegI; x.r := r; x.a := x.b*)
	      GetReg1(r, x.r); x.r := r;
	      ConvertRegNumToName (x.r, xname);
	      Write ('mov ('); Write (x.label); Write (', '); Write (xname); Writeln (');');
	      x.mode := OSAB.RegI; x.r := r; x.a := x.b;
	      (*GetReg (r);
	      ConvertRegNumToName (r, rname);
	      Write ('mov (['); Write (xname);  Write (', '); Write (rname); Writeln (');');*)
	      
	      (* mov (3, (type int32 [edx+1*4])); right! *)
	      COPY ('(type ', tmpstr);
	      IF x.type.base.form = OSAB.Bool THEN
	         Strings.Append ('boolean ', tmpstr);
	      ELSIF x.type.base.form = OSAB.Char THEN
	         Strings.Append ('char ', tmpstr);
	      ELSIF x.type.base.form = OSAB.Int THEN
	          Strings.Append ('int32 ', tmpstr);
	      ELSE Out.String ('unknown type in Index, add it to if-else'); Out.Ln; 
	      Out.String ('x.type.form is '); Out.Int (x.type.form, 0); Out.Ln;
	      Out.String ('x.type.base.form is '); Out.Int (x.type.base.form, 0); Out.Ln;
	      HALT(0);
	      END;
	      Strings.Append ('[', tmpstr); Strings.Append (xname, tmpstr); Strings.Append ('+', tmpstr);
		IntStr.IntToStr (y.a, offset);
	        Strings.Append (offset, tmpstr);
	        Strings.Append ('*', tmpstr);
	        IntStr.IntToStr (s, tsize);
	        Strings.Append (tsize, tmpstr);
	        Strings.Append ('])', tmpstr);
	        COPY (tmpstr, x.label);
	ELSIF (x.mode = OSAB.Par) & (y.mode = OSAB.Var) THEN
	      GetReg1(r, x.r); x.r := r;
	      ConvertRegNumToName (x.r, xname);
	      Write ('mov ('); Write (x.label); Write (', '); Write (xname); Writeln (');');
	      x.mode := OSAB.RegI; x.r := r; x.a := x.b;
              load(y);
	      ConvertRegNumToName (y.r, yname);
	      Write ('shl (2, '); Write (yname); Writeln (');');
	      Write ('add ('); Write (yname); Write (', '); Write (xname); Writeln (');');
	      Release (y.r); y.mode := OSAB.Var;
		COPY ('(type ', tmpstr);
	      IF x.type.base.form = OSAB.Bool THEN
	         Strings.Append ('boolean ', tmpstr);
	      ELSIF x.type.base.form = OSAB.Char THEN
	         Strings.Append ('char ', tmpstr);
	      ELSIF x.type.base.form = OSAB.Int THEN
	          Strings.Append ('int32 ', tmpstr);
	      ELSE Out.String ('unknown type in Index, add it to if-else'); Out.Ln; 
	      Out.String ('x.type.form is '); Out.Int (x.type.form, 0); Out.Ln;
	      Out.String ('x.type.base.form is '); Out.Int (x.type.base.form, 0); Out.Ln;
	      HALT(0);
	      END;
	      Strings.Append ('[', tmpstr); Strings.Append (xname, tmpstr); Strings.Append ('])', tmpstr);
	      COPY (tmpstr, x.label);
	ELSIF (x.mode = 2) & (y.mode = 2) THEN
              load (y);
	      ConvertRegNumToName (y.r, yname);
	      Write ('shl (2, '); Write (yname); Writeln (');');
	      COPY (x.label, tmpstr); (* test_a[edx]*)
	      Strings.Append ('[', tmpstr);
	      Strings.Append (yname, tmpstr);
	      Strings.Append (']', tmpstr);
	      COPY (tmpstr, x.label);
	      Release (y.r);
	END ;
	      
      s0 := s; n := 0;
      WHILE ~ODD(s0) DO s0 := s0 DIV 2; INC(n) END ;
      IF (s0 = 1) & (x.mode = OSAB.RegI) & (x.a = 0) THEN x.mode := RegX; x.a := y.r; x.b := n
      ELSIF x.mode IN {OSAB.Var, OSAB.RegI, RegX} THEN
         IF s0 = 1 THEN
	    (*GetReg2(r, x.r, y.r); Put0a(ADD, r, x.r, y.r, 0, n)*)
         ELSE 
	    (*GetReg(r0); s0 := s;
	    (*ScaleConst(s0, n);*)
            ConvertRegNumToName (r0, reg0);
	    Write ('lea ('); Write (reg0); Write (', '); Write (modname); Write ('_'); Write (x.label); Writeln (');');
            ConvertRegNumToName (y.r, regy);
            Write ('intmul ('); Writeint (y.a); Write (', '); Write (regy); Writeln (');');
	    Write ('add ('); Write (regy); Write (', '); Write (reg0); Write (');');
    Release (y.r);
           *) 
            (*IF s0 < C8 THEN
	       (*Put1a(MOV, r0, 0, s0, n)*)
            ELSE 
	       (*enterIC(s); Put2(LDR, r0, PC, 0)*)
            END ;*)
            Release(r0); (*GetReg2(r, x.r, y.r);*) (*Put0c(2, r, y.r, r0, x.r)*)  (*MUL*)
         END ;
         IF x.r = PC THEN x.a := x.a -  LONG(pc)*4 - 4 END ;
         x.r := r; x.mode := OSAB.RegI
      ELSE OSAS.Mark("bad mode in Index")
      END
   END ;
   x.type := x.type.base
END Index;

PROCEDURE DeRef*(VAR x: Item);
VAR r: LONGINT;
BEGIN
IF x.mode = OSAB.Var THEN x.mode := OSAB.Par; x.b := 0
ELSIF x.mode = OSAB.Par THEN
GetReg1(r, x.r); Put2(LDR, r, x.r, x.a); Put2(LDR, r, r, x.b); x.mode := OSAB.RegI; x.r := r; x.a := 0
ELSIF x.mode = OSAB.RegI THEN GetReg1(r, x.r); Put2(LDR, r, x.r, x.a); x.r := r; x.a := 0
ELSIF x.mode = OSAB.Reg THEN x.mode := OSAB.RegI; x.a := 0
ELSIF x.mode = RegX THEN
GetReg2(r, x.r, x.a); Put3(LDR, r, x.r, x.a, x.b); x.mode := OSAB.RegI; x.r := r; x.a := 0
ELSE OSAS.Mark("bad mode in DeRef"); r := 0
END ;
x.type := x.type.base
END DeRef;

PROCEDURE TypeTest*(VAR x: Item; T: OSAB.Type; varpar, isguard: INTEGER);
VAR r0: LONGINT; tdes: Item;
BEGIN
IF varpar = 1 THEN GetReg(r0); Put2(LDR, r0, FP, x.a+4)
ELSE load(x); GetReg(r0); Put2(LDR, r0, x.r, -4); x.mode := OSAB.Reg
END ;
Put2(LDR, r0, r0, T.nofpar*4);  (*TD of x*)
tdes.mode := OSAB.Var; tdes.a := T.typobj.val; tdes.b := T.typobj.lev;
MakeItem(tdes, T.typobj);  (*TD of y*) tdes.mode := OSAB.Var;
loadAdr(tdes); Put0(CMP, 0, r0, tdes.r); Release(tdes.r); Release(r0);
IF isguard # 1 THEN Release(x.r) END ;
IF isguard = 0 THEN SetCC(x, 0) ELSE Put6(1, 2) END
END TypeTest;

(* Code generation for Boolean operators *)


PROCEDURE Not*(VAR x: Item);   (* x := ~x *)
VAR t: LONGINT;
BEGIN
IF x.mode # CC THEN loadCC(x) END ;
x.not := TRUE;(*x.r := negated(x.r);*) (*x.op := Inv(x.op);*)
t := x.a; x.a := x.b; x.b := t;
END Not;

PROCEDURE And1*(VAR x: Item);   (* x := x & *)
VAR lab, num, xname : OSAS.Ident;
reg : LONGINT;
BEGIN
Writeln ('// procedure and1');(*Writeln ('//debugitem x'); debugitem (x);*)
IF x.mode # CC THEN 
Writeln("// x.mode # CC, then loadCC"); 
loadCC(x); 
Write ('cmp ('); ConvertRegNumToName (x.r, xname); Write (xname); Write (', '); Writeint (0); Writeln (');');
Write ('jnz '); 
INC (landnext);
Write ('_'); Write (modname); Write ('_'); Write (labelandnext); Writeint(landnext); Writeln (';');
Write ('jmp '); 
Write ('_'); Write (modname); Write ('_'); Write (lableandfalse); Writeint (landfalse); Writeln (';');
END ;
(*Out.String ("negated(x.r)"); Out.Ln;*)
(*Put5(negated(x.r), x.a);*);
(*IF ~x.if THEN
(*Write ("cmpl $1,");*)
ConvertRegNumToName (reg, xname);
Write ('cmp (');
Write (xname);
Write (', ');
Writeln ('true);')
END;
x.and := TRUE;
Out.String ("CASE x.r OF ");*)
(*CASE x.r OF
    0: (* = *) Write ("jne "); Out.String ("x.r=0"); Out.Ln;
 |  1: (* # *) Write ("je "); Out.String ("x.r=1"); Out.Ln;
 |  2: (* < *) Write ("jge "); Out.String ("x.r=2"); Out.Ln;
 |  3: (* <= *) Write ("jg "); Out.String ("x.r=3"); Out.Ln;
 |  4: (* > *) Write ("jle "); Out.String ("x.r=4"); Out.Ln;
 |  5: (* >= *)Write ("jl "); Out.String ("x.r=5"); Out.Ln;
 (* or *)
 |  9: (* =*) Write ("jne "); Out.String ("x.r=9"); Out.Ln;
 | 10: (* #*) Write ("je "); Out.String ("x.r=10"); Out.Ln;
 | 11: (* <*) Write ("jge "); Out.String ("x.r=11"); Out.Ln;
 | 12: (* <= *) Write ("jg "); Out.String ("x.r=12"); Out.Ln;
 | 13: (* > *) Write ("jle "); Out.String ("x.r=13"); Out.Ln;
 | 14: (* >=*) Write ("jl "); Out.String ("x.r=14"); Out.Ln;
END;*)
(*CASE x.r OF
    0: (* = *) Write ("je "); Out.String ("x.r=0"); Out.Ln;
 |  1: (* # *) Write ("jne "); Out.String ("x.r=1"); Out.Ln;
 |  2: (* < *) Write ("jl "); Out.String ("x.r=2"); Out.Ln;
 |  3: (* <= *) Write ("jle "); Out.String ("x.r=3"); Out.Ln;
 |  4: (* > *) Write ("jg "); Out.String ("x.r=4"); Out.Ln;
 |  5: (* >= *)Write ("jge "); Out.String ("x.r=5"); Out.Ln;
 (* or *)
 |  9: (* =*) Write ("je "); Out.String ("x.r=9"); Out.Ln;
 | 10: (* #*) Write ("jne "); Out.String ("x.r=10"); Out.Ln;
 | 11: (* <*) Write ("jl "); Out.String ("x.r=11"); Out.Ln;
 | 12: (* <= *) Write ("jle "); Out.String ("x.r=12"); Out.Ln;
 | 13: (* > *) Write ("jg "); Out.String ("x.r=13"); Out.Ln;
 | 14: (* >=*) Write ("jge "); Out.String ("x.r=14"); Out.Ln;
END;
IntStr.IntToStr (x.jnum, num);
COPY (labelmark, lab);
Strings.Append (num, lab);
(*Writeln (lab);*)
Write (lab); Writeln (';');
COPY (lab, x.jlab);
*)
(*debug(x); Out.String ("x.a := pc -1");
x.a := pc-1;debug(x);
Out.String ("filxlink(x.b)"); Out.Ln; FixLink(x.b); x.b := 0; Out.String ("x.b := 0"); Out.Ln;
x.and := TRUE*)
x.and := TRUE;
Writeln ('// end of procedure and1');
END And1;

PROCEDURE And2*(VAR x, y: Item);
VAR xname, yname : OSAS.Ident;
BEGIN 
Writeln ('// procedure and2');
(*Writeln ('//item x');
debugitem(x); 
Writeln ('// item y');
debugitem(y);*)
Write ('_'); Write (modname); Write ('_'); Write (labelandnext); Writeint(landnext); Writeln(':');
IF y.mode # CC THEN
loadCC(y);
Write ('cmp ('); ConvertRegNumToName (y.r, yname); Write (yname); Write (', '); Writeint (0); Writeln (');');
Write ('jnz '); 
INC (landnext);
Write ('_'); Write (modname); Write ('_'); Write (labelandnext); Writeint(landnext); Writeln (';');
Write ('jmp '); Write ('_'); Write (modname); Write ('_'); Write (lableandfalse); Writeint (landfalse); Writeln (';');
END;
(*ConvertRegNumToName (x.r, xname);
Write ('and ('); 
IF y.mode = OSAB.Const THEN
Writeint (y.a);
ELSE
ConvertRegNumToName (y.r, yname);
Write (yname);
END;
Write (', '); Write (xname); 
Writeln (');');
IF ~x.not THEN
Write ('seta (');
ELSE 
Write ('setna (')
END;
Convert8RegNumToName (x.r, xname);
 Write (xname); Writeln (');');
(*x.a := merged(y.a, x.a);*) x.b := y.b;
(*x.r := y.r;*)
Release (x.r); Release (y.r);*)
Writeln ('//end of procedure and2');
END And2;

PROCEDURE And3*(VAR y : Item);
VAR yname : OSAS.Ident;
BEGIN
Write ('_'); Write (modname); Write ('_'); Write (labelandnext); Writeint(landnext); Writeln(':');
Convert8RegNumToName (y.r, yname);
Write ('mov (1, '); Write (yname); Writeln (');');
Write ('jmp '); Write ('_'); Write (modname); Write ('_'); Write(labelandend); Writeint (landend); Writeln (';');

Write ('_'); Write (modname); Write ('_'); Write (lableandfalse); Writeint(landfalse); INC(landfalse); Writeln (':');
Write ('mov (0, '); Write (yname); Writeln (');');

Write ('_'); Write (modname); Write ('_'); Write (labelandend); Writeint (landend); Writeln(':'); INC (landend);

END And3;


PROCEDURE Or1*(VAR x: Item);   (* x := x OR *)
VAR num, lab, br, xname : OSAS.Ident;
reg : LONGINT;
BEGIN

IF x.mode # CC THEN
loadCC(x);

Write ('cmp ('); ConvertRegNumToName (x.r, xname); Write (xname); Write (', '); Writeint (0); Writeln (');');
Write ('jnz '); Write ('_'); Write (modname); Write ('_'); Write (labelortrue); Writeint (lortrue); Writeln (';');
INC (lornext);
Write ('jmp '); Write ('_'); Write (modname); Write ('_'); Write (labelornext); Writeint(lornext); Writeln (';');
END ;
(*Put5(x.r, x.b); x.b := pc-1; FixLink(x.a);*) x.a := 0
END Or1;

PROCEDURE Or2*(VAR x, y: Item);
VAR num, lab, xname, yname : OSAS.Ident;
BEGIN
Write ('_'); Write (modname); Write ('_'); Write (labelornext); Writeint(lornext); Writeln(':');


IF y.mode # CC THEN
loadCC(y);

Write ('cmp ('); ConvertRegNumToName (y.r, yname); Write (yname); Write (', '); Writeint (0); Writeln (');');
Write ('jnz '); Write ('_'); Write (modname); Write ('_'); Write (labelortrue); Writeint (lortrue); Writeln (';');
INC (lornext);
Write ('jmp '); Write ('_'); Write (modname); Write ('_'); Write (labelornext); Writeint(lornext); Writeln (';');



END ;



(*
ConvertRegNumToName (x.r, xname);
Write ('or ('); 
IF y.mode = OSAB.Const THEN
Writeint (y.a);
ELSE
ConvertRegNumToName (y.r, yname);
Write (yname);
END;
Write (', '); Write (xname); 
Writeln (');');
IF ~x.not THEN
Write ('seta (');
ELSE
Write ('setna (');
END;
Convert8RegNumToName (x.r, xname);
 Write (xname); Writeln (');');
(*x.a := merged(y.a, x.a);*) x.b := y.b; 
(*x.r := y.r;*)
Release (x.r); Release (y.r);*)

(*ConvertRegNumToName (x.r, xname);
ConvertRegNumToName (y.r, yname);
Write ('or (');
Write (yname); Write (', '); Write (xname);  Writeln (');');

x.a := y.a; x.b := merged(y.b, x.b); x.r := y.r;*)
END Or2;

PROCEDURE Or3*(VAR y : Item);
VAR yname : OSAS.Ident;
BEGIN
 Write ('_'); Write (modname); Write ('_');  Write (labelornext); Writeint(lornext); Writeln(':');
Convert8RegNumToName (y.r, yname);
Write ('mov (0, '); Write (yname); Writeln (');');
Write ('jmp '); Write ('_'); Write (modname); Write ('_');  Write(labelorend); Writeint (lorend); Writeln (';');

 Write ('_'); Write (modname); Write ('_'); Write (labelortrue); Writeint(lortrue); INC(lortrue); Writeln (':');
Write ('mov (1, '); Write (yname); Writeln (');');

 Write ('_'); Write (modname); Write ('_'); Write (labelorend); Writeint (lorend); Writeln(':'); INC (landend);

END Or3;





(* Code generation for arithmetic operators *)

PROCEDURE Neg*(VAR x: Item);   (* x := -x *)
VAR r: LONGINT;
BEGIN
Out.String ("entered Neg for "); Out.String (x.label); Out.Ln;
   IF x.type.form = OSAB.Int THEN
      IF x.mode = OSAB.Const THEN x.a := -x.a
      ELSE load(x); GetReg1(r, x.r); Out.String ("r="); Out.Int(r, 0); Out.Ln; Out.String ("x.r="); Out.Int(x.r, 0); Out.Ln; 
      negreg(r);(*Put1(RSB, r, x.r, 0);*)
      END
   ELSIF x.type.form = OSAB.Real THEN
      IF x.mode = OSAB.Const THEN
         IF x.a # 0 THEN x.a := -x.a (*+*) (*-2147483648*)  (*80000000H*) END (* ! *)
      ELSE load(x); (*Put1a(2, x.r, x.r, 2, 1)*)
      END
   ELSE (*form = Set*)
      IF x.mode = OSAB.Const THEN x.a := -x.a-1 
      ELSE load(x); GetReg1(r, x.r); (*Put0(MVN, r, 0, x.r);*) x.r := r
      END
   END
END Neg;

PROCEDURE AddOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
VAR r: LONGINT;
BEGIN (*Out.String ("AddOp BEGINs"); Out.Ln; ShowRegs;*)
   IF op = OSAS.plus THEN (*Out.String ("op = plus"); Out.Ln;*)
      IF y.mode = OSAB.Const THEN (*Out.String ("y.mode=OSAB.Const"); Out.Ln;*)
          IF x.mode = OSAB.Const THEN x.a := y.a + x.a
          ELSE load(x); (*Out.String ("load (x)"); Out.Ln; ShowRegs;*)
             IF y.a >= 0 THEN 
	     (*AddConst(ADD+1, x, y.a)*)
	     AddConstToVar (x, y.a);
	     ELSE
	     (*AddConst(SUB+1, x, -y.a)*)
	     SubConstToVar (x, y.a);
	     END
          END
      ELSIF x.mode = OSAB.Const THEN (*Out.String ("x.mode=OSAB.Const"); Out.Ln;*)
         load(y); (*Out.String ("load(y)"); Out.Ln; ShowRegs;*)
         IF x.a >= 0 THEN 
	 (*AddConst(ADD+1, y, x.a)*)
	 AddConstToVar (y, x.a);
         ELSE 
	 (*AddConst(RSB, y, -x.a);*)
	 (*Put1(RSB, y.r, y.r, 0)*)
         SubConstToVar (y, x.a)
	 (*AddConstToVar(y, x.a);*)
         END ;
      x := y
      ELSIF (x.mode = OSAB.Reg) & (y.mode=OSAB.Reg) THEN (*Out.String ("both operands are in registers now"); Out.Ln; ShowRegs;*)
      AddVarsInRegs(x.r, y.r); Release (y.r); (*Release (x.r);*) 
      (*Out.String ("y.r released"); Out.Ln; ShowRegs;*)
      ELSIF (y.mode = OSAB.Reg) & (x.mode = OSAB.Var) THEN
      Out.String ('AAAAAAAAAAAAAAAAAAAAAA'); Out.String ('y.r='); Out.Int (y.r,0); Out.Ln; Out.String ('x.r=');
      Out.Int (x.r,0); Out.Ln;
      (* Out.String ("y.mode = OSAB.Reg"); Out.Ln; Out.String ("x.mode="); Out.Int (x.mode,0); Out.Ln;*)
      (*GetReg1 (r, y.r);*) (* just commented to test procedures *)
      (*Out.String ("GetReg1 (r, x.r)"); Out.Ln; Out.String ("r="); Out.Int (r, 0); Out.Ln; Out.String ("x.r="); Out.Int (x.r,0); Out.Ln;*)
      AddVarsInRegAndMem ((*r,*) y, x);
      (*x.r := r; x.mode := OSAB.Reg;*) (* just commented and added *)
      x.r := y.r; x.mode := OSAB.Reg;
      ELSE       (*Out.String ("ELSE "); Out.Ln;*)
      
      (* for risc like 
      load(x); load(y);
      GetReg2(r, x.r, y.r);
      AddVarsInRegs(r, x.r, y.r);(*  Put0(ADD+1, r, x.r, y.r);*) 
      Out.String ("r="); Out.Int(r, 0); Out.Ln; Out.String ("x.r="); Out.Int (x.r, 0); Out.Ln; Out.String ("y.r="); Out.Int (y.r, 0); Out.Ln;
      x.r := r; Out.String ("x.r := r"); Out.Ln
      -----------------*)
      (* for cisc like *)
      load (x); 
      (*Out.String ("load(x)"); Out.Ln;*)
      GetReg1(r, x.r); 
      (*Out.String ("GetReg1(r, x.r)"); Out.Ln;
      Out.String ("r="); Out.Int(r, 0); Out.Ln; Out.String ("x.r="); Out.Int (x.r, 0); Out.Ln;*)
      AddVarsInRegAndMem((*r,*) x, y);
      (*Out.String ("AddVarsInRegAndMem(r, x, y)"); Out.Ln;*)
      x.r := r; x.mode := OSAB.Reg;
      (*Out.String ("x.r := r"); Out.Ln*)
      END
   ELSE (*op = minus*)
   (*Out.String ("op = minus"); Out.Ln;*)
      IF y.mode = OSAB.Const THEN
         IF x.mode = OSAB.Const THEN x.a := x.a - y.a
         ELSE load(x);
	 (*Out.String ("load(x)"); Out.Ln; ShowRegs;*)
            IF y.a >= 0 THEN SubConstToVar(x, y.a); (*AddConst(SUB+1, x, y.a)*) ELSE AddConstToVar(x, -y.a) (*AddConst(ADD+1, x, -y.a)*) END;
         END
      ELSIF x.mode = OSAB.Const THEN
      (*Out.String ("x.mode=OSAB.Const"); Out.Ln; ShowRegs;
      Out.String ("loading variable "); Out.String (y.label); Out.String (" into register"); Out.Ln; Out.String ("currently y.r="); Out.Int (y.r,0); Out.Ln;*)
      load(y); 
      (*Out.String ("load(y) "); Out.Ln; 
      Out.String ("variable y"); Out.String (y.label); Out.String ("have been assigned reg "); Out.Int (y.r,0); Out.Ln;*)
      ShowRegs;
         IF x.a >= 0 THEN (*AddConst(SUB+3, y, x.a)*) 
	 (*Out.String ("x.a >= 0"); Out.Ln;*)
	 load(x); (*Out.String ("load(x)"); Out.Ln; ShowRegs;*)
	 Release (x.r); Release (y.r); (*Out.String (" x.r and y.r released"); Out.Ln; ShowRegs;*)
	 SubVarsInRegs(x.r, y.r);
	 Movl (x.r, y.r);
	 x.r := y.r
         ELSE (*AddConst(ADD+1, y, -x.a); Put1(RSB, y.r, y.r, 0)*)
	 (*ShowRegs;
	 Out.String ("loading constant "); Out.Int (x.a,0); Out.String (" intro register "); Out.Ln; Out.String ("cuurently x.r="); Out.Int (x.r, 0); Out.Ln;*)
	 load (x); (*Out.String ("load(x)"); Out.Ln; ShowRegs;
	 Out.String("constant "); Out.Int (x.a, 0); Out.String ("now in register "); Out.Int (x.r, 0); Out.Ln;*)
	 (*GetReg2 (r, x.r, y.r);*) 
	 Release (x.r); Release (y.r);(* Out.String ("x.r and y.r released"); Out.Ln; ShowRegs;*)
	 SubVarsInRegs ( x.r, y.r);
	 Movl (x.r, y.r);
	 x.r := y.r;
         END ;
      x := y
      ELSIF (y.mode = OSAB.Reg) THEN 
      (*Out.String ("y.mode = OSAB.Reg"); Out.Ln; Out.String ("x.mode="); Out.Int (x.mode,0); Out.Ln;*)
      (*GetReg1 (r, y.r);*)
      (*Out.String ("GetReg1 (r, x.r)"); Out.Ln; Out.String ("r="); Out.Int(r, 0); Out.Ln; Out.String ("x.r="); Out.Int (x.r,0); Out.Ln;*)
      load(x); Release (x.r); Release (y.r);
      (*SubVarsInRegAndMem (r, y, x);*)
      SubVarsInRegs (x.r, y.r); 
      (*Movl (x.r, y.r);
      x.r := y.r; 
      x.mode := OSAB.Reg;*)
      ELSE (*load(x); load(y); GetReg2(r, x.r, y.r); Put0(SUB+1, r, x.r, y.r); x.r := r*)
      load (x); (*Out.String ("load(x)"); Out.Ln; ShowRegs;*)
      GetReg1 (r, x.r);(*Out.String("GetReg1(r, x.r)"); Out.Ln; ShowRegs;*)
      SubVarsInRegAndMem (r, x, y);
      END
   END
END AddOp;

PROCEDURE log(m: LONGINT; VAR n: LONGINT): LONGINT;
BEGIN n := 0;
WHILE ~ODD(m) DO m := m DIV 2; INC(n) END ;
RETURN m
END log;

PROCEDURE divide (VAR x, y : Item);
VAR a, d : BOOLEAN;
r : LONGINT;
xname, yname, rname, constname, constname2 : OSAS.Ident;
BEGIN
   (*GetThisReg(EAX);*) (* was it free, if not save it *)
   a := FALSE;
   IF EAX IN regs THEN
      a := TRUE;
      Push (EAX);
   END;
   regs := regs + {EAX};
   IF x.mode = OSAB.Var THEN
(*      Write ("movl ");   Write (x.label); Writeln (", %eax");*) (* loading dividend to eax *)
      Write ("mov (");   Write (x.label); Writeln (", eax);"); (* loading dividend to eax *)
   END;
   IF x.mode = OSAB.Const THEN 
(*   Write ("movl "); Write ("$"); IntStr.IntToStr(x.a, constname); Write (constname); Writeln (", %eax");*)
   Write ("mov ("); IntStr.IntToStr(x.a, constname); Write (constname); Writeln (", eax);");
   END;
   IF x.mode = OSAB.Reg THEN
(*   Write ("movl ");*)
   Write ("mov ( ");
   ConvertRegNumToName (x.r, xname);
(*   Write (xname); Writeln (", %eax"); Release (x.r);*)
   Write (xname); Writeln (", eax);"); Release (x.r);
   END;

   (*GetThisReg(EDX);*) (* was it free, if not save it *)
   d := FALSE;
   IF EDX IN regs THEN
      d := TRUE;
      Push (EDX);
   END;
   regs := regs + {EDX};
   (*Writeln ("movl $0, %edx");*) Writeln ('mov (0, edx);'); (* loading zero to edx as it must be 0 during div *)
   IF y.mode = OSAB.Var THEN
(*      Write ("idivl ");*)
      Write ("idiv (");
      Writeln (y.label); 
      Write (');');
   END;
   IF y.mode = OSAB.Const THEN
      (*Write ("$"); IntStr.IntToStr(y.a, constname2); Write(constname2)*)
      load(y); (*Out.String ("load(y)"); Out.Ln; Out.String ("y.mode="); Out.Int(y.mode,0); Out.Ln;*)
      (*Out.String ("y.r="); Out.Int(y.r,0); Out.Ln;*)
      (*Write ("idivl "); ConvertRegNumToName (y.r, yname); Writeln (yname);*)
      Write ("idiv ("); ConvertRegNumToName (y.r, yname); Write (yname); Writeln (');');
      Release (y.r); y.mode := OSAB.Const;
   END;
   IF y.mode = OSAB.Reg THEN
   (*Write ("idivl ");*)
   Write ("idiv (");
   ConvertRegNumToName(y.r, yname);
   Write (yname); Writeln (');');
   END;
   x.r := EAX; x.mode := 8;
   (*Out.String ("After the division "); Out.Ln;
   ShowRegs;*)
   (*GetReg1 (r, x.r); *)(*Out.String ("GetReg1 (r, x.r)"); Out.Ln; ShowRegs;*)
(*   Write ("movl %eax, "); ConvertRegNumToName (r, rname); Writeln (rname);*) (* hla *)
   Write ("mov (eax, "); ConvertRegNumToName (y.r, rname); Write (rname); Writeln (');');
   x.r := y.r; x.mode := 8; (* register *)
regs := regs - {EDX};
regs := regs - {EAX};
IF a THEN
Pop(EAX);
regs := regs + {EAX};
END;
IF d THEN
Pop(EDX);
regs := regs + {EDX};
END;
END divide;

PROCEDURE modulus (VAR x, y : Item);
VAR a, d : BOOLEAN;
r : LONGINT;
xname, yname, rname, constname, constname2 : OSAS.Ident;
BEGIN 
   (*GetThisReg(EAX);*)
   a := FALSE;
   IF EAX IN regs THEN
      a := TRUE;
      Push(EAX);
   END;
   regs := regs + {EAX}; (* was it free, if not save it *)
   IF x.mode = OSAB.Var THEN
      (*Write ("movl ");*)
      Write ("mov (");
      Write (x.label); 
      (*Writeln (", %eax");*) (* loading dividend to eax *)
      Writeln (", eax);"); (* loading dividend to eax *)
   END;
   IF x.mode = OSAB.Const THEN 
   (*Write ("movl ");*)
   Write ("mov (");
      (*Write ("$");*)
      IntStr.IntToStr(x.a, constname); Write (constname); 
      (*Writeln (", %eax");*)
      Writeln (", eax);");
   END;
   IF x.mode = OSAB.Reg THEN 
      (*Write ("movl ");*)
      Write ("mov (");
      ConvertRegNumToName (x.r, xname);
      Write (xname); 
      (*Writeln (", %eax"); *)
      Writeln (", eax);"); 
      Release (x.r);
      (*Out.String ("releasing "); Out.Int(x.r,0); Out.Ln;*)
   END;
   
   (*GetThisReg(EDX);*)
   d := FALSE;
   IF EDX IN regs THEN
      Push(EDX);
      d := TRUE;
   END;
   regs := regs + {EDX}; (* was it free, if not save it *)
   (*Writeln ("movl $0, %edx");*) (* loading zero to edx as it must be 0 during div *)
   Writeln ("mov ( 0, edx );"); (* loading zero to edx as it must be 0 during div *)
   IF y.mode = OSAB.Var THEN
   (*Write ("idivl ");*)
   Write ("idiv (");
      Write (y.label); Writeln (');');
   END;
      IF (y.mode = OSAB.Var) OR (y.mode = OSAB.Const)  THEN
	 load(y); 
	 (*Out.String ("load(y)"); Out.Ln; Out.String ("y.mode="); Out.Int(y.mode,0); Out.Ln;
	 Out.String ("y.r="); Out.Int(y.r,0); Out.Ln;*)
	 Write ("idiv (");
	 (*Write ("idivl ");*)
	 ConvertRegNumToName (y.r, yname); Write (yname); Writeln (');');
	 Release (y.r);
     ELSIF y.mode = OSAB.Reg THEN
   Write ("idiv (");
   (*Write ("idivl ");*)
      ConvertRegNumToName(y.r, yname);
      Write (yname); Writeln (');');
   END;
 
(*x.r := EDX; x.mode := 8;*)
(*Out.String ("After the modulus "); Out.Ln;
ShowRegs;*)
(*GetReg1 (r, x.r);*) (*Out.String ("GetReg1 (r, x.r)"); Out.Ln; ShowRegs;*)
(*Write ("movl %edx, "); *)
Write ("mov (edx, "); 
ConvertRegNumToName (y.r, rname); Write (rname); Writeln (');');
x.r := y.r; x.mode := 8; (* register *)
(*RecoverThisRegFromStack(EAX);
RecoverThisRegFromStack(EDX);*)
regs := regs - {EDX};
regs := regs - {EAX};
IF a THEN
Pop(EAX);
regs := regs + {EAX};
END;
IF d THEN
Pop(EDX);
regs := regs + {EDX};
END;
END modulus;

PROCEDURE multiply(VAR x, c: Item);  (*c.a >= 2*)
VAR i, j, m, r: LONGINT;
BEGIN GetReg1(r, x.r); (*m := c.a; i := 0;
REPEAT m := m DIV 2; INC(i) UNTIL ODD(m);
   IF m = 1 THEN
      IF ODD(c.a) THEN Put0a(ADD+1, r, x.r, x.r, 0, i) ELSE Put0a(MOV+1, r, 0, x.r, 0, i) END
   ELSE load(c); Put0c(1, r, x.r, c.r, 0); Release(c.r)
   END ;*)
   MulConstToVar (x, c.a);
x.r := r; (*Release (c.r);*)
END multiply;

PROCEDURE MulOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
VAR n, r: LONGINT;
BEGIN (*Out.String ("Entered MulOp"); Out.Ln; Out.String ("x.mode=");Out.Int (x.mode,0); Out.Ln;
      Out.String ("y.mode="); Out.Int (y.mode,0); Out.Ln;
      Out.String ("op="); Out.Int(op,0); Out.Ln;*)
   IF (x.mode = OSAB.Const) & (y.mode = OSAB.Const) THEN (*Out.String ("both are constants"); Out.Ln;*)
      IF op = OSAS.times THEN x.a := x.a * y.a; (*Out.String ("yes, it is times"); Out.Ln;*)
      ELSIF y.a = 0 THEN OSAS.Mark("division by 0")
      ELSIF op = OSAS.div THEN x.a := x.a DIV y.a
      ELSE x.a := x.a MOD y.a
      END
   ELSE
     IF op = OSAS.times THEN (*Out.String ("op = OSAS.times"); Out.Ln;*)
        IF (x.mode = OSAB.Const) (*& (x.a >= 2)*) THEN (*Out.String ("(x.mode = OSAB.Const) & (x.a > 2)"); Out.Ln;*)
           load(y); multiply(y, x); x.mode := OSAB.Reg; x.r := y.r; (*Out.String ("load(y)"); Out.Ln; Out.String ("multiply (y, x)"); Out.Ln; Out.String ("x.mode = OSAB.Reg"); Out.Ln;*)
        ELSIF (y.mode = OSAB.Const) (*& (y.a >= 2)*) THEN load(x); multiply(x, y); (*Out.String ("(y.mode = OSAB.Const) & (y.a >= 2)"); Out.Ln;
	Out.String ("load(x); multiply(x, y);"); Out.Ln;*)
	ELSIF (x.mode = OSAB.Reg) & (y.mode = OSAB.Reg) THEN (*Out.String (" (x.mode = OSAB.Reg) & (y.mode = OSAB.Reg)"); Out.Ln;*)
        MulVarsInRegs (x.r, y.r); Release (y.r); (*Out.String ("MulVarsInRegs (x.r, y.r); Release (y.r);"); Out.Ln;*)
	ELSIF (y.mode = OSAB.Reg) & (x.mode = OSAB.Var) THEN
	load(x); GetReg1(r, x.r);
	MulVarsInRegs (x.r, y.r); Release (y.r);
        ELSE load(x);(* load(y); GetReg2(r, x.r, y.r); Put0c(1, r, x.r, y.r, 0); x.r := r*) (*Out.String ("ELSE load(x)"); Out.Ln;
	   Out.String ("x.mode="); Out.Int (x.mode,0); Out.Ln;
	   Out.String ("x.label="); Out.String (x.label); Out.Ln;
	   Out.String ("y.mode="); Out.Int (y.mode,0); Out.Ln;
	   Out.String ("y.label="); Out.String (y.label); Out.Ln;*)
	   GetReg1(r, x.r); (*Out.String ("GetReg1(r, x.r)"); Out.Ln;*)
           MulVarsInRegAndMem (r, x, y); x.mode := OSAB.Reg; (*Out.String ("MulVarsInRegAndMem (r, x, y); x.mode := OSAB.Reg;"); Out.Ln;*)
	   x.r := r; (*Out.String ("After multiplication"); Out.Ln; ShowRegs;*)
        END
   ELSIF op = OSAS.div THEN (*load(x);*) (*Out.String ("this is it"); Out.Ln;
              Out.String ("x.mode="); Out.Int (x.mode,0); Out.Ln;
              Out.String ("x.label="); Out.String (x.label); Out.Ln;
	      Out.String ("x.r="); Out.Int (x.r,0); Out.Ln;
              Out.String ("y.mode="); Out.Int (y.mode,0); Out.Ln;
              Out.String ("y.label="); Out.String (y.label); Out.Ln;*)
      divide (x, y);
         (*IF (y.mode = OSAB.Const) & (y.a > 0) & (log(y.a, n) = 1) THEN
          (*GetReg1(r, x.r); Put0a(MOV+1, r, 0, x.r, 2, n); x.r := r*)
	  (*load(x);*)
	  GetReg1(r, x.r);
	  divide (x, y);
      ELSIF (x.mode = OSAB.Reg) & (y.mode = OSAB.Reg) THEN
      (*DivVarsInRegs (x.r, y.r); Release (y.r)*)
      ELSE (*load(y);*)
         (*IF x.r = FP-1 THEN op := 9
         ELSIF x.r = FP-2 THEN op := 10
         ELSE OSAS.Mark("expression too complex")
         END ;
         Release(y.r); Put5a(op * 10000H + fixlistFP); fixlistFP := pc-1*)
      divide (x, y);
      END
      *)
   ELSIF op = OSAS.mod THEN (*load(x);*)
      modulus (x, y);
      (*
      IF (y.mode = OSAB.Const) & (y.a > 0) & (log(y.a, n) = 1) THEN(*
         GetReg1(r, x.r);
         IF n <= 8 THEN Put1(0, r, x.r, y.a-1)  (*AND*)
         ELSIF n >= 24 THEN Put1a(29, r, x.r, (-y.a) DIV 1000000H MOD 100H, 4) (*BIC*)
         ELSE Put0a(MOV, r, 0, x.r, 0, (32-n) MOD 20H); Put0a(MOV+1, r, 0, r, 1, (32-n) MOD 20H)
         END ;
      x.r := r *)
      ELSE (*load(y);
         IF x.r = FP-1 THEN op := 9
         ELSIF x.r = FP-2 THEN op := 10
         ELSE OSAS.Mark("expression too complex")
         END ;
         Release(y.r); Put5a(op * 10000H + fixlistFP); fixlistFP := pc-1;
         IF x.r IN regs THEN Put0(MOV, x.r, 0, y.r) ELSE Put0(MOV, y.r, 0, x.r); x.r := y.r END*)
      modulus (x, y); 
      END*)
   END
END
END MulOp;

(* Code generation for REAL operators *)

PROCEDURE PrepOpd*(VAR x: Item);
BEGIN load(x)  (*for real division only*)
END PrepOpd;

PROCEDURE RealOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
VAR r: LONGINT;  (*x.type.form = Real*)
BEGIN load(x); load(y);
IF regs = {FP-2, FP-1} THEN
IF op = OSAS.plus THEN op := 1
ELSIF op = OSAS.minus THEN Put1a(2, y.r, y.r, 2, 1); op := 1
ELSIF op = OSAS.times THEN op := 3
ELSIF op = OSAS.rdiv THEN op := 5
END
ELSIF regs = {FP-3 .. FP-1} THEN
IF op = OSAS.plus THEN op := 2
ELSIF op = OSAS.minus THEN Put1a(2, y.r, y.r, 2, 1); op := 2
ELSIF op = OSAS.times THEN op := 4
ELSIF op = OSAS.rdiv THEN op := 6
END
ELSE OSAS.Mark("simplify expression!")
END ;
GetReg2(x.r, x.r, y.r); Put5a(op*10000H + fixlistFP); fixlistFP := pc-1
END RealOp;

(*PROCEDURE RealOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
VAR r: LONGINT;;  (*x.type.form = Real; for FPU-emulator with FPGA*)
BEGIN load(x); load(y); GetReg(r);
IF op = OSAS.plus THEN op := 0 ELSIF op = OSAS.minus THEN op := 1
ELSIF op = OSAS.times THEN op := 2 ELSIF op = OSAS.rdiv THEN op := 3
END ;
Put1a(MOV, r, 0, 23H, 4); Put2(STR, x.r, r, 0);
Put1a(MOV, r, 0, 27H, 4); Put2(STR, y.r, r, 0);
Put1a(MOV, y.r, 0, 1, 12); Put1(ADD, y.r, y.r, op); Put1a(MOV, r, 0, 22H, 4); Put2(STR, y.r, r, 0);
Put2(LDR, y.r, r, 0); Put1(16, 0, y.r, 1); Put5(0, -4);  (*wait: LDR, TST, BEQ*)
Put1a(MOV, r, 0, 23H, 4); Put2(LDR, x.r, r, 0);   (*get result*)
Release(y.r); Release(r)
END RealOp;*)

(* Code generation for set operators *)

PROCEDURE Singleton*(VAR x, y: Item);  (* x := {y} *)
VAR r: LONGINT;
BEGIN
IF y.mode = OSAB.Const THEN x.mode := OSAB.Const; x.a := ASH(1, y.a)
ELSE load(y); GetReg1(x.r, y.r); GetReg(r); Put1(MOV, r, 0, 1); Put0b(MOV+1, x.r, 0, r, 0, y.r);
Release(r); x.mode := OSAB.Reg
END
END Singleton;

PROCEDURE Set*(VAR x, y, z: Item);   (* x := {y .. z} *)
VAR r0, r1: LONGINT; rname,rrname, xname, zname, yname : OSAS.Ident;
BEGIN
   IF (y.mode = OSAB.Const) & (z.mode = OSAB.Const) THEN
      x.mode := OSAB.Const;
      IF y.a <= z.a THEN x.a := ASH(2, z.a) - ASH(1, y.a) ELSE x.a := 0 END
   ELSE x.mode := OSAB.Reg; load(z); 
      IF (y.mode = OSAB.Const) & (y.a = 0) THEN
         GetReg(r0); (*Put1(MVN, r0, 0, 1);*) 
	 Write ('mov (2, '); ConvertRegNumToName (r0, rname); Write (rname); Writeln (');');
        INC(labelnum);
         Write ('_'); Write (modname); Write ('_');
         Write (labelnumber); Writeint (labelnum); Writeln (":");
         Write ('shl (1, '); Write (rname); Writeln (');');
         Write ('dec ('); ConvertRegNumToName (z.r, zname); Write (zname); Writeln (');');
         Write ('jnz '); Write ('_'); Write (modname); Write ('_'); Write(labelnumber);  Writeint (labelnum); Writeln (';');
	 Write ('dec ('); Write (rname); Writeln (');');
	 Release(r0); 
	 (*GetReg1(x.r, z.r);*) (*Put0b(MVN+1, x.r, 0, r0, 0, z.r)*)
	 (*ConvertRegNumToName (x.r, xname);*)
	 (*Write ('mov ('); Write (rname); Write (', '); Write (xname); Writeln (');');*)
	 Release(x.r);
	 x.r := r0
      ELSE
         GetReg (r0);
         Write ('mov (2, '); ConvertRegNumToName (r0, rname); Write (rname); Writeln (');');
         INC(labelnum);
         Write ('_'); Write (modname); Write ('_');
         Write (labelnumber); Writeint (labelnum); Writeln (":");
         Write ('shl (1, '); Write (rname); Writeln (');');
         Write ('dec ('); ConvertRegNumToName (z.r, zname); Write (zname); Writeln (');');
         Write ('jnz '); Write ('_'); Write (modname); Write ('_'); Write (labelnumber); Writeint (labelnum); Writeln (';');
         load(y); ConvertRegNumToName (y.r, yname);
         Release (z.r);
	 GetReg(r1);
	 Write ('mov (1, '); ConvertRegNumToName (r1, rrname); Write (rrname); Writeln (');');
         INC(labelnum);
         Write ('_'); Write (modname); Write ('_');
         Write (labelnumber); Writeint (labelnum); Writeln (":");
         Write ('shl (1, '); Write (rrname); Writeln (');');
         Write ('dec ('); Write (yname); Writeln (');');
         Write ('jnz '); Write ('_'); Write (modname); Write ('_'); Write(labelnumber); Writeint (labelnum); Writeln (';');
	 Write ('sub ('); Write(rrname); Write (', '); Write (rname); Writeln (');');
         Release (r0); Release (r1);
	 (*GetReg2 (x.r, y.r, z.r); ConvertRegNumToName (x.r, xname);
	 Write ('mov ('); Write (rname); Write (', '); Write (xname); Writeln (');');*)
         Release (y.r); Release (x.r); x.r := r0;
         (*GetReg(r0); 
	 (*Put1(MVN, r0, 0, 1); Put0b(MVN, r0, 0, r0, 0, z.r);*)
	 GetReg(r1); (*Put1(MVN, r1, 0, 0);*)
	 Release(r1); Release(r0);
         GetReg2(x.r, y.r, z.r); (*Put0b(1, x.r, r0, r1, 0, y.r)*)*)
      END
   END
END Set;

PROCEDURE In*(VAR x, y: Item);  (* x := x IN y *)
VAR r: LONGINT;
xname, yname : OSAS.Ident;
BEGIN load(y);
   ConvertRegNumToName (y.r, yname); 
   IF x.mode = OSAB.Const THEN
(*       Write ("bt $"); Writeint(x.a); Write (", "); Writeln (yname);*)
       Write ("bt ("); Writeint(x.a); Write (", "); Write (yname); Writeln (');');
      (*Put1a(17, 0, y.r, (x.a MOD 2) + 1, (16 - (x.a DIV 2)) MOD 10H)  (*TST*)*)
   ELSE load(x); GetReg(r); ConvertRegNumToName (x.r, xname);
        (*debug(x); ShowRegs;*)
	(*Write ("bt "); Write (xname); Write (", "); Writeln (yname); *)
	Write ("bt ("); Write (xname); Write (", "); Write (yname); Writeln (');');
      (*Put1(MOV, r, 0, 1);
      Put0b(17, 0, y.r, r, 0, x.r);
      *)
      Release(r); Release(x.r)
   END ;
   IF ~x.not THEN
   Write('setc (');
   ELSE
   Write ('setnc (')
   END;
   Convert8RegNumToName (y.r, yname); Write(yname); Writeln (');');
   Release(y.r); (*SetCC(x, 1)*) (*SetCC (x, 15);*) x.mode := CC; x.a := 0; x.b := 0; x.r := 15;
END In;

PROCEDURE SetOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
VAR n, r, r0, s, op0: LONGINT;  (*x.type.form = Set*)
BEGIN (*Out.String ("SetOp"); Out.Ln;*)
IF op = OSAS.plus THEN op0 := 24 (*or*) ELSIF op = OSAS.minus THEN op0 := 28 (*bic*)
ELSIF op = OSAS.times THEN op0 := 0 (*and*) ELSIF op = OSAS.rdiv THEN op0 := 2 (*xor*)
ELSE OSAS.Mark("not a set operator"); op0 := 0
END ;
IF op0 # 28 THEN 
   IF y.mode = OSAB.Const THEN
      IF (x.mode = OSAB.Const) & (op = OSAS.plus) THEN
         x.a := SYSTEM.VAL(LONGINT, SYSTEM.VAL(SET, x.a) + SYSTEM.VAL(SET, y.a))
      ELSE load(x); n := y.a;
      (*ScaleConst(n, s);*)
      GetReg1(r, x.r); 
         (*IF n DIV C8 = 0 THEN Put1a(op0, r, x.r, n, s MOD 10H)
         ELSE  enterIC(y.a); GetReg(r0); Put2(LDR, r0, PC, 0); Put0(op0, r, x.r, r0); Release(r0)
         END*)
	 SetOpConstAndReg(op, x, n);
      END
   ELSIF x.mode = OSAB.Const THEN (*Out.String ("x.mode = OSAB.Const"); Out.Ln;
      Out.String ("load y x"); Out.Ln;*)
      load(y); load(x);
      ShowRegs;
      GetReg2 (r, x.r, y.r);
      debug(x); debug(y);
      SetOpInRegs(op, x, y);
   ELSIF (y.mode = OSAB.Reg) & (x.mode = OSAB.Var) THEN
   GetReg1 (r, y.r); SetOpInRegAndMem (op, y, x);
   x.r := r; x.mode := OSAB.Reg;
   ELSE 
      (*load(x); load(y); Out.String ("load (x); load (y);"); Out.Ln;
      GetReg2(r, x.r, y.r); (*Put0(op0, r, x.r, y.r)*); SetOpInRegs(op0, x, y);*)
      load(x); Out.String ("load(x)"); Out.Ln;
      GetReg1 (r, x.r); Out.String ("GetReg1 (r, x.r)"); Out.Ln;
      SetOpInRegAndMem (op, x, y); x.mode := OSAB.Reg;
   END ;
x.r := r
ELSE (* op0 = 28 *)
load(x); load(y); GetReg2 (r, x.r, y.r );
SetMinus (x, y);
x.r := r;
END;
END SetOp;

(* Code generation for relations *)

PROCEDURE IntRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
VAR r: LONGINT;
BEGIN Out.String ("int relation"); Out.Ln;
Out.String ('x.not = '); IF x.not THEN Out.String ('true') ELSE Out.String ('false') END; Out.Ln; debug(x);
Out.String ('y.not = '); IF x.not THEN Out.String ('true') ELSE Out.String ('false') END; Out.Ln; debug(y);

   (*IF (y.mode = OSAB.Const) & (y.type.form # OSAB.Proc) THEN Out.String ("y.mode=OSAB.Const"); Out.Ln;
      load(x);
      (*IF y.a >= 0 THEN AddConst(CMP, x, y.a) ELSE AddConst(CMP+2, x, -y.a) END ;*)
      CompareInRegAndConst1 (x, y.a);
      Release(x.r); SetCC(x, cond[op - OSAS.eql])
   ELSE*)
   IF (x.mode = OSAB.Const) & (x.type.form # OSAB.Proc) THEN Out.String ("x.mode=OSAB.Const"); Out.Ln;
      load(y);  (*CMPI*)
      (*IF x.a >= 0 THEN 
      AddConst(CMP, y, x.a) ELSE AddConst(CMP+2, y, -x.a) END ;*)
      CompareInRegAndConst (op, y, x);
      Release(y.r); (*SetCC(x, revcond[op - OSAS.eql])*) (* bug? *) SetCC (x, op);
   ELSE
   load(x); load(y); (*Put0(CMP, r, x.r, y.r);*) CompareInRegs (op, y, x);
      Release(y.r); Release(x.r); debug(x);
      (*SetCC(x, cond[op - OSAS.eql])*) (* bug? *)
      SetCC (x, op);
   END
END IntRelation;

PROCEDURE SetRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
VAR k: LONGINT;
BEGIN load(x); load(y);
IF op = OSAS.eql THEN Put0(19, 0, x.r, y.r); k := 0  (*TEQ*)
ELSIF op = OSAS.neq THEN Put0(19, 0, x.r, y.r); k := 1  (*TEQ*)
ELSIF (op = OSAS.lss) OR (op = OSAS.leq) THEN Put0(29, LNK, x.r, y.r); k := 0
ELSE (*gtr OR geq*) Put0(29, LNK, y.r, x.r); k := 0  (*BIC; LNK used as dummy*)
END ;
Release(y.r); Release(x.r); SetCC(x, k);
END SetRelation;

PROCEDURE RealRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
BEGIN load(x);
IF (y.mode = OSAB.Const) & (y.a = 0) THEN Put1(CMP, 0, x.r, 0)
ELSE load(y); Put0(17, 0, x.r, y.r);  (*TST*)
Put0(61EH, x.r, 0, x.r); Put0(61EH, y.r, 0, y.r); Put0(CMP, 0, x.r, y.r);  (*MVN, CMP*)
Release(y.r)
END ;
Release(x.r); SetCC(x, cond[op - OSAS.eql])
END RealRelation;

PROCEDURE StringRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
VAR r0, r1: LONGINT;  (*x, y are char arrays or strings*)
BEGIN
IF x.type.form = OSAB.String THEN loadString(x) ELSE loadAdr(x) END ;
IF y.type.form = OSAB.String THEN loadString(y) ELSE loadAdr(y) END ;
GetReg(r0); Put2(7, r0, x.r, 1);
GetReg(r1); Put2(7, r1, y.r, 1); 
Put0(CMP, 0, r0, r1);
Put5(1, 1);   (* BNE +3 *)
Put1(CMP, 0, r0, 0);
Put5(1, -7);   (* BNE -5 *)
Release(x.r); Release(y.r); Release(r0); Release(r1);
SetCC(x, cond[op - OSAS.eql])
END StringRelation;

(* Code generation of Assignments *)

PROCEDURE PrepStore*(VAR x: Item);
BEGIN
IF (x.mode = RegX) & (x.type.form = OSAB.Real) THEN
(*Put0a(ADD, x.r, x.r, x.a, 0, x.b);*) Release(x.a); x.a := 0; x.mode := OSAB.RegI
END
END PrepStore;

PROCEDURE Store*(VAR x, y: Item); (* x := y *)
VAR cd, ra: LONGINT;
r : LONGINT; rname : OSAS.Ident;
BEGIN 
Out.String ('x.type.form is '); Out.Int (x.type.form, 0); Out.Ln;
debug (x); debug(y);
IF (y.mode = OSAB.Const) & (x.mode = 10) THEN 
   Write ('mov ('); Writeint (y.a); Write (', '); Write (x.label); Writeln (');');
   Release (y.r); Release (x.r);
ELSIF (y.mode = OSAB.Var) & (x.mode = 10) THEN
(*   Write ('mov ('); Write (y.label); Write (', '); Write (x.label); Writeln (');');
   Release (x.r); Release (y.r);*)
    IF x.type.size >= 4 THEN 
      Write ('mov ('); Write (y.label); Write (', '); Write (x.label); Writeln (');');
    ELSE (* 1 *)
      GetReg1(r, x.r); x.r := r; Convert8RegNumToName (r, rname);
      Write ('mov ('); Write (y.label); Write (', '); Write (rname); Writeln (');');
      Write ('mov ('); Write (rname); Write (', '); Write (x.label); Writeln (');');
    END;
    Release (x.r); Release (y.r);
ELSIF (y.mode = OSAB.Const) & (x.mode = 9) THEN
   Write ('mov ('); Writeint (y.a); Write (', '); Write (x.label); Writeln (');');
ELSIF (y.mode = OSAB.Var) & (x.mode = 9) THEN
    IF x.type.size >=4 THEN
       Write ('mov ('); Write (y.label); Write (', '); Write (x.label); Writeln (');');
    ELSE (* 1 *)
      GetReg1(r, x.r); x.r := r; Convert8RegNumToName (r, rname);
      Write ('mov ('); Write (y.label); Write (', '); Write (rname); Writeln (');');
      Write ('mov ('); Write (rname); Write (', '); Write (x.label); Writeln (');');
    END;
    Release (x.r); Release (y.r);
ELSIF (y.mode = 9) & (x.mode = 2) THEN
   IF x.type.size >= 4 THEN
      Write ('mov ('); Write (y.label); Write (', '); Write (x.label); Writeln (');');
   ELSE (* 1 *)
      GetReg1(r, x.r); x.r := r; Convert8RegNumToName (r, rname);
      Write ('mov ('); Write (y.label); Write (', '); Write (rname); Writeln (');');
      Write ('mov ('); Write (rname); Write (', '); Write (x.label); Writeln (');');
   END;
   Release (x.r); Release (y.r);
ELSIF (y.mode = 10) & (x.mode = 2) THEN
    IF x.type.size >= 4 THEN 
      Write ('mov ('); Write (y.label); Write (', '); Write (x.label); Writeln (');');
    ELSE (* 1 *)
      GetReg1(r, x.r); x.r := r; Convert8RegNumToName (r, rname);
      Write ('mov ('); Write (y.label); Write (', '); Write (rname); Writeln (');');
      Write ('mov ('); Write (rname); Write (', '); Write (x.label); Writeln (');');
    END;
    Release (x.r); Release (y.r);
ELSE
   load(y);
   (*Out.String ("PROCEDURE Store"); Out.Ln;
	debug(x); debug(y);*)
	(*IF x.mode = CC THEN Out.String ("x.mode = CC"); Out.Ln END;
	IF y.mode = CC THEN Out.String ("y.mode = CC"); Out.Ln END;
	IF x.type = OSAB.boolType THEN Out.String ("x.type = OSAB.boolType"); Out.Ln END;
	IF y.type = OSAB.boolType THEN Out.String ("y.type = OSAB.boolType"); Out.Ln END;*)
	(*IF y.and THEN
	IntStr.IntToStr (y.jnum, num);
	COPY (labelmark, lab);
	Strings.Append (num, lab);
	Write (lab); Writeln (":");
	END;*)
   IF x.mode = OSAB.Reg THEN Out.String ("x.mode = 8"); Out.Ln;
      cd := code[pc-1];
      IF (y.r < RH) & (y.r >= RL) & (cd DIV 4000000H = -8) & (cd DIV 10H MOD 10H # 9) 
      OR (cd DIV 4000000H = -7) & ODD(cd DIV 100000H) THEN (*fixup*)
         code[pc-1] := cd DIV 10000H * 10000H + x.r * 1000H + cd MOD 1000H
      ELSE Put0(MOV, x.r, 0, y.r)
      END
   ELSE
      (*IF x.type.size = 1 THEN cd := STR+4 ELSE cd := STR END ;*)
      IF x.mode = OSAB.Var THEN
         StoreReg(y, x); (*IF savedregsnum > 0 THEN stacktoregs END;*)
         (* Put2(cd, y.r, x.r, x.a)*)
      ELSIF x.mode = OSAB.Par THEN
         (*GetReg(ra);*)
	 (*Put2(LDR, ra, x.r, x.a); Put2(cd, y.r, ra, x.b);*)
	 StoreReg(y, x);
	 (*Release(ra)*)
      ELSIF x.mode = RegX THEN Put3(cd+8, y.r, x.r, x.a, x.b); Release(x.a)
      ELSIF x.mode = OSAB.RegI THEN Put2(cd, y.r, x.r, x.a); Release(x.a)
      ELSE OSAS.Mark("bad mode in Store");
      Out.String("Store"); Out.Int(x.mode, 4); Out.Int(y.mode, 4);
      Out.Ln
      END ;
   Release(x.r)
   END ;
   Release(y.r);
 END;
END Store;

PROCEDURE SetAndLab(VAR y : Item);
VAR 
   num, lab : OSAS.Ident;
   BEGIN
      IntStr.IntToStr (y.jnum, num);
      COPY (labelmark, lab);
      Strings.Append (num, lab);
      Write ('_'); Write (modname); Write ('_');   Write (lab); Writeln (":");
END SetAndLab;

PROCEDURE CopyRecord*(VAR x, y: Item);  (* x := y *)
VAR cnt, data, s: LONGINT; pc0: INTEGER; xL, yL: Item;
BEGIN s := x.type.size DIV 4;
IF s > 0 THEN
loadAdr(x); loadAdr(y); GetReg(cnt); GetReg(data);
IF s < C8 THEN Put1(MOV, cnt, 0, s) ELSE enterIC(s); Put2(LDR, cnt, PC, 0) END ;
pc0 := pc; Put2(3, data, y.r, 4); Put2(2, data, x.r, 4); Put1(SUB+1, cnt, cnt, 1); Put5(1, pc0 - pc -2);
Release(cnt); Release(data); Release(x.r); Release(y.r)
END
END CopyRecord;

PROCEDURE CopyArray*(VAR x, y: Item);  (* x := y *)
VAR cnt, data: LONGINT; pc0: INTEGER; xL, yL: Item;
BEGIN
IF (x.type.form = OSAB.Array) & (x.type.len >= 0) THEN MakeConstItem(xL, OSAB.intType, x.type.len)
ELSE xL.mode := OSAB.Var; xL.type := OSAB.intType; xL.r := FP; xL.a := x.a - 4
END ;
loadAdr(x); load(xL);
IF (y.type.form = OSAB.Array) & (y.type.len >= 0) THEN MakeConstItem(yL, OSAB.intType, y.type.len)
ELSE yL.mode := OSAB.Var; yL.type := OSAB.intType; yL.r := FP; yL.a := y.a - 4
END ;
loadAdr(y); load(yL); Put0(CMP, 0, xL.r, yL.r); Put6(11, 3);  (*length check*)
(*determine word count*)
cnt := yL.r; data := xL.r;  (*convert length of y to nofwords, size is in byte, len always a multiple of 4*)
IF y.type.base.size < 4 THEN Put0a(MOV, cnt, 0, cnt, 1, 2)  (* /4, byte array*)
ELSIF y.type.base.size > 4 THEN (*wordcnt = len * elementsize/4*)
Put1a(MOV, xL.r, 0, y.type.base.size, 1); Put0c(0, cnt, cnt, xL.r, 0)
END ;
pc0 := pc; Put2(3, data, y.r, 4); Put2(2, data, x.r, 4); Put1(SUB+1, cnt, cnt, 1); Put5(1, pc0 - pc -2);
Release(xL.r); Release(yL.r); Release(x.r); Release(y.r)
END CopyArray;

PROCEDURE CopyString*(VAR x, y: Item);  (* x := y *)
VAR r: LONGINT; pc0: INTEGER;
i : LONGINT;
BEGIN 
(*GetReg(r);*)
debug(x); debug(y);
IF x.type.len = -1 THEN (*open dest array*)
(*Put2(LDR, r, x.r, x.a+4); Put1(CMP, 0, r, y.b); Put6(11, 6)*)
ELSIF x.type.len < y.b THEN 
   Out.String ('x.type.len '); Out.Int(x.type.len, 0); Out.Ln;
   Out.String ('y.b '); Out.Int (y.b, 0); Out.Ln;
   OSAS.Mark("string too long")
END ;
(*loadAdr(x); loadString(y);
pc0 := pc; Put2(3, r, y.r, 4); Put2(2, r, x.r, 4); Put1a(1, r, r, 0FFH, 4); Put5(1, pc0 - pc -2);*);
 FOR i := 0 TO y.b DO
    Write ("mov ("); 
    IF ORD(y.label[i]) >= 32 THEN
       Write ("'"); WriteChar (y.label[i]); 
       Write ("'");
    ELSE
       Write ('#'); Writeint (ORD(y.label[i]));
    END;
   Write (", "); 
   Write (x.label); Write ('['); Writeint (i); Write (']);'); Writeln("");
 END;
Release(r); Release(y.r); Release(x.r)
END CopyString;

(* Code generation for parameters *)

PROCEDURE InitParamArray*;
BEGIN
paraindex := -1;
END InitParamArray;

PROCEDURE VarParam*(VAR x: Item; ftype: OSAB.Type);
VAR x0, tdes: Item;
BEGIN 
   (*x0 := x; loadAdr(x);
   IF (ftype.form = OSAB.Array) & (ftype.len < 0) THEN loadArrayLen(x0)
   ELSIF ftype.form = OSAB.Record THEN
      IF x.type.typobj = NIL THEN OSAS.Mark("anonymous type for VAR-Param") END ;
      MakeItem(tdes, x.type.typobj); tdes.mode := OSAB.Var; loadAdr(tdes)
   END*)
   INC(paraindex);
   para[paraindex] := x;
END VarParam;

PROCEDURE ValueParam*(VAR x: Item);
VAR r: LONGINT;
BEGIN
   IF (x.mode = OSAB.Reg) & ((x.r < RL-1) OR (x.r >= RH)) THEN
      GetReg(r); Put0(MOV, r, 0, x.r)  (*register variable*)
   ELSE 
   INC(paraindex);
   para[paraindex] := x;
   (*load(x)*)
   END ;
END ValueParam;

PROCEDURE StringParam*(VAR x: Item);
VAR r: LONGINT;
BEGIN (*string constant?*)
(*loadString(x); *)
INC(paraindex);
para[paraindex] := x;
(*GetReg(r); ConvertRegNumToName (r, rname); x.r := r;
Write ('mov '); Write(x.label); Write (', '); Write (rname); Writeln (');');*)
(*Put1(MOV, r, 0, x.b)*) (*len*)
END StringParam;

PROCEDURE ByteParam*(VAR x: Item);  (*formal param of type SYSTEM.BYTES*)
VAR y, tdes: Item; adr, r0, r1, elsize: LONGINT;
BEGIN adr := x.a; loadAdr(x); (*find size in bytes*)
IF (x.type.form = OSAB.Array) & (x.type.len < 0) THEN (*open array*)
GetReg(r0); Put2(LDR, r0, FP, x.a+4); elsize := x.type.base.size;
IF elsize = 4 THEN Put0a(MOV, r0, 0, r0, 0, 2)
ELSIF elsize # 1 THEN GetReg(r1); Put1(MOV, r1, 0, elsize); Put0c(0, r0, r0, r1, 0); Release(r1)
END
ELSIF x.type.form = OSAB.Record THEN
MakeItem(tdes, x.type.typobj); tdes.mode := OSAB.Var; load(tdes)
(*first word of type descriptor is length*)
ELSE MakeConstItem(y, OSAB.intType, x.type.size); load(y);
END
END ByteParam;

(* For and Case Statements*)

PROCEDURE EnterLabel*(at: LONGINT);
VAR d: LONGINT;
BEGIN
IF code[at] = 0 THEN code[at] := -369098752 (*0EA000000H*) + pc - at -2  (*BR*)
ELSE OSAS.Mark("multiple def of label")
END
END EnterLabel;

PROCEDURE Case*(VAR x: Item; n: LONGINT; VAR L: LONGINT);
VAR k: LONGINT; xname : OSAS.Ident;
BEGIN load(x);
   ConvertRegNumToName (x.r, xname);
   Write ('cmp ('); Write(xname); Write (', '); Writeint (n); Writeln (');'); 
   Write ('setle (');
   Convert8RegNumToName (x.r, xname);
   Write (xname); Writeln(');');
   Write ('test ('); Write (xname); Write(', '); Write(xname); Writeln (');');
   Write ('je '); Write ('_'); Write (modname); Write ('_'); Write (labelnumber); Writeint (labelnum); Writeln (';'); 
(*	Put1(CMP, 0, x.r, n);
	Put0a(ADD+500H, PC, PC, x.r, 0, 2); *)
	Release(x.r);
	Put6(14, 4);   (*SWI; case out of range*)
	(*L := pc;*)
   L := labelnum; INC(labelnum);
   Write ('_'); Write (modname); Write ('_');
   Write (labelnumber); Writeint (labelnum); Writeln (":");
	k := 0;  (*branch table*)
   WHILE k < n DO
      PutC(0);
      INC(k)
   END
END Case;

PROCEDURE For0*(VAR x, y: Item);
VAR yname, xname : OSAS.Ident;
BEGIN
 Out.String ('FORRRRR'); Out.Ln;
debug (x); debug (y);
   IF x.mode = OSAB.Reg THEN 
      Store(x, y); (* not tested *)
      y.r := x.r 
   ELSE 
      load(y);
      ConvertRegNumToName (y.r, yname);
      Write ('mov ('); Write (yname); Write (','); Write (x.label); Writeln (');');
   END
END For0;

PROCEDURE For1*(VAR x, y, z, w: Item; VAR L: LONGINT);
VAR r: LONGINT;
    yname, zname : OSAS.Ident;
BEGIN 
   Out.String ('x'); Out.Ln;debug (x); Out.String ('y'); Out.Ln; debug (y);Out.String ('z'); Out.Ln; debug (z); debug (w);
   r := y.r; 
   (*IntRelation(OSAS.leq, y, z);*)
   load (z);
   ConvertRegNumToName (y.r, yname);
   ConvertRegNumToName (z.r, zname);
   Write ('cmp ('); Write (yname); Write (', '); Write (zname); Writeln (');');
   Write ('jg '); 
   (*L := pc;*); L := labelnum; INC (labelnum);
   Write ('_'); Write (modname); Write ('_');  Write (labelnumber); Writeint (L+1); Writeln (';');
   Release (y.r); Release (z.r);
   IF (r >= RL) & (r < RH) THEN INCL(regs, r) END ;
   IF w.a > 0 THEN (*Put5(12, 0)*)
   ELSIF w.a < 0 THEN (*Put5(11, 0)*)
   ELSE OSAS.Mark("zero increment"); (*Put5(0, 0)*)
   END ;
   IF x.mode # OSAB.Reg THEN (*Put(STR, r, x.r, x.a);*) Release(r) END
END For1;

PROCEDURE For2*(VAR x, y, w: Item);
VAR n: LONGINT; rr : OSAS.Ident;
BEGIN n := w.a; load(x); 
   ConvertRegNumToName(x.r, rr);  
   Release(x.r);
IF (n >= 0) & (n < 100H) THEN (*Put1(ADD, x.r, x.r, n)*) 
   Write ('add (');
   Writeint (n); Write (', '); Write (rr); Writeln (');');
ELSIF (n < 0) & (n >= -100H) THEN (* to be changed to max longint *)
	Write ('sub (-'); Writeint (n); Write (', '); Write (rr); Writeln (');'); 
	(*Put1(SUB, x.r, x.r, -n)*)
ELSE OSAS.Mark("step too large")
END;
Write ('mov ('); Write (rr); Write (', '); Write (x.label); Writeln (');');
END For2;

(* Branches, procedure calls, procedure prolog and epilog *)

PROCEDURE Here*(VAR L: LONGINT);
BEGIN (*L := pc*) L := labelnum; INC(labelnum);
Write ('_'); Write (modname); Write ('_');
Write (labelnumber); Writeint ((*L*)(*!*)labelnum); Writeln (":");
END Here;

PROCEDURE FJump*(VAR x : Item);
VAR lab, num : OSAS.Ident;
n : LONGINT;
BEGIN 
(*Put5(14, L);*)
(*L := pc-1*)
Out.String ("FJump"); Out.Ln; Out.String(" x.a = "); Out.Int(x.a, 0); Out.Ln;
Out.String ("x.r="); Out.Int (x.r, 0); Out.Ln;
Out.String ("x.b="); Out.Int (x.b, 0); Out.Ln;
Out.String ("x.jnum="); Out.Int (x.jnum, 0); Out.Ln;
(*Put5( negated(x.r), x.a); FixLink(x.b);*) x.a := pc-1;
(*Write ("JNA "); *)
Out.String ("CASE x.r OF ");
CASE x.r OF
    0: (* = *) Write ("je "); Out.String ("x.r=0"); Out.Ln;
 |  1: (* # *) Write ("jne "); Out.String ("x.r=1"); Out.Ln;
 |  2: (* < *) Write ("jl "); Out.String ("x.r=2"); Out.Ln;
 |  3: (* <= *) Write ("jle "); Out.String ("x.r=3"); Out.Ln;
 |  4: (* > *) Write ("jg "); Out.String ("x.r=4"); Out.Ln;
 |  5: (* >= *)Write ("jge "); Out.String ("x.r=5"); Out.Ln;
 (* or *)
 |  9: (* =*) Write ("je "); Out.String ("x.r=9"); Out.Ln;
 | 10: (* #*) Write ("jne "); Out.String ("x.r=10"); Out.Ln;
 | 11: (* <*) Write ("jl "); Out.String ("x.r=11"); Out.Ln;
 | 12: (* <= *) Write ("jle "); Out.String ("x.r=12"); Out.Ln;
 | 13: (* > *) Write ("jg "); Out.String ("x.r=13"); Out.Ln;
 | 14: (* >=*) Write ("jge "); Out.String ("x.r=14"); Out.Ln;
END;
(*INC (x.jnum);*)
IntStr.IntToStr (x.jnum, num);
COPY (labelmark, lab);
Strings.Append (num, lab);
 Write ('_'); Write (modname); Write ('_'); Writeln (lab); COPY (lab, x.jlab);
END FJump;

PROCEDURE Setlabel* (num : LONGINT);
BEGIN
Write ('_'); Write (modname); Write ('_');
Write (labelmark); Writeint (num); Writeln (":");
END Setlabel;

PROCEDURE Setthen* (VAR num : LONGINT);
BEGIN
Write ('_'); Write (modname); Write ('_');
Write (labelthenmark); Writeint(num); Writeln (":");
END Setthen;

PROCEDURE Setendlabel*(VAR num : LONGINT);
BEGIN
Write ('_'); Write (modname); Write ('_');
Write (labelendmark); Writeint(num); Writeln (":");
END Setendlabel;

PROCEDURE CFJump*(VAR x: Item);
VAR lab, num : OSAS.Ident;
reg : LONGINT;
regname : OSAS.Ident;
BEGIN
IF x.mode # CC THEN loadCC(x) END ;
Out.String ("CFJump"); Out.Ln; Out.String(" x.a = "); Out.Int(x.a, 0); Out.Ln;
Out.String ("x.r="); Out.Int (x.r, 0); Out.Ln;
Out.String ("x.b="); Out.Int (x.b, 0); Out.Ln;
Out.String ("x.jnum="); Out.Int (x.jnum, 0); Out.Ln;
(*Put5( negated(x.r), x.a); FixLink(x.b);*) x.a := pc-1;
(*Write ("JNA "); *)
(*Out.String ("CASE x.r OF ");

	CASE x.r OF 
    0: (* = *) Write ("jne "); Out.String ("x.r=0"); Out.Ln;
 |  1: (* # *) Write ("je "); Out.String ("x.r=1"); Out.Ln;
 |  2: (* < *) Write ("jge "); Out.String ("x.r=2"); Out.Ln;
 |  3: (* <= *) Write ("jg "); Out.String ("x.r=3"); Out.Ln;
 |  4: (* > *) Write ("jle "); Out.String ("x.r=4"); Out.Ln;
 |  5: (* >= *)Write ("jl "); Out.String ("x.r=5"); Out.Ln;
 (* or *) (* stegh kartses takiny petqa pakel *)
 |  9: (* =*) Write ("jne "); Out.String ("x.r=9"); Out.Ln;
 | 10: (* #*) Write ("je "); Out.String ("x.r=10"); Out.Ln;
 | 11: (* <*) Write ("jge "); Out.String ("x.r=11"); Out.Ln;
 | 12: (* <= *) Write ("jg "); Out.String ("x.r=12"); Out.Ln;
 | 13: (* > *) Write ("jle "); Out.String ("x.r=13"); Out.Ln;
 | 14: (* >=*) Write ("jl "); Out.String ("x.r=14"); Out.Ln;
 | 15: (* in *) IF x.not = FALSE THEN Write ("jnc ") ELSE Write ("jc ") END;
END;*)
(*Write ('jf (');*)
Write ('test (');
GetReg(reg);
Convert8RegNumToName (reg, regname);
Release (reg);
Write (regname); 
(*---*)
Write (', '); Write (regname);
(*---*)

Write (') ');

(*---*)
Writeln (';');
Write ('je ');
(*---*)
(*INC (x.jnum);*)
   IntStr.IntToStr (x.jnum, num);
   COPY (labelmark, lab);
   Strings.Append (num, lab);
 Write ('_'); Write (modname); Write ('_');
 Write (lab); Writeln (';');
   (*Writeln (lab); *)
   COPY (lab, x.jlab);
END CFJump;

PROCEDURE wCFJump*(VAR x: Item);
VAR lab, num : OSAS.Ident;
reg : LONGINT;
regname : OSAS.Ident;
BEGIN
IF x.mode # CC THEN loadCC(x) END ;
Out.String ("wCFJump"); Out.Ln; Out.String(" x.a = "); Out.Int(x.a, 0); Out.Ln;
Out.String ("x.r="); Out.Int (x.r, 0); Out.Ln;
Out.String ("x.b="); Out.Int (x.b, 0); Out.Ln;
Out.String ("x.jnum="); Out.Int (x.jnum, 0); Out.Ln;
(*Write ('jf (');*)
Write ('test (');
GetReg(reg);
Convert8RegNumToName (reg, regname);
Release (reg);
Write (regname); 
(*---*)
Write (', '); Write (regname);
(*---*)
Write (') ');
(*---*)
Writeln (';');
Write ('je ');
(*---*)
   (*IntStr.IntToStr (x.jnum, num);
   COPY (labelmark, lab);
   Strings.Append (num, lab);
 Write ('_'); Write (modname); Write ('_');
 Write (lab); Writeln (';');
   (*Writeln (lab); *)
   COPY (lab, x.jlab);*)
   Write ('_'); Write (modname); Write ('_'); Write (labelnumber); 
   INC(labelnum);(*!*)Writeint (labelnum); Writeln (';')
END wCFJump;



PROCEDURE Jump* (VAR j : LONGINT);
VAR num, lab : OSAS.Ident;
BEGIN
Write ("jmp ");
IntStr.IntToStr (j, num);
COPY (labelendmark, lab);
Strings.Append (num, lab);
Write ('_'); Write (modname); Write ('_');
Write (lab); Writeln (';');
(*Writeln (lab); (*COPY (lab, x.jlab);*)*)
END Jump;

PROCEDURE BJump*(L: LONGINT);
BEGIN (*Put5(14, L-pc-2)*)
Write ('jmp ');
Write ('_'); Write (modname); Write ('_');
Write (labelnumber); Writeint ((*L*)(*!*)(*labelnum-1*)L+1); Writeln (';');
END BJump;

PROCEDURE CBJump*(VAR x: Item; L: LONGINT);
VAR reg : LONGINT; regname : OSAS.Ident;
BEGIN
IF x.mode # CC THEN loadCC(x) END ;
Write ('test (');
GetReg(reg);
Convert8RegNumToName (reg, regname);
Release (reg);
Write (regname); 
(*---*)
Write (', '); Write (regname);
(*---*)

Write (') ');

(*---*)
Writeln (';');
Write ('je ');
(*---*)
(*INC (x.jnum);*)
 Write ('_'); Write (modname); Write ('_');
 Write (labelnumber);  Writeint (L+1(*x.a*)); Writeln (';');
 


(*Put5(negated(x.r), L-pc-2);*)
(*FixLink(x.b);
FixLinkWith(x.a, L)*)
END CBJump;

PROCEDURE Fixup*(VAR x: Item);
VAR lab, num : OSAS.Ident;
BEGIN (*FixLink(x.a)*)
(*   IntStr.IntToStr (x.jnum, num);
   COPY (labelmark, lab);
   Strings.Append (num, lab);
 Write ('_'); Write (modname); Write ('_');
 Write (lab); Writeln (':');*)
Write ('_'); Write (modname); Write ('_'); (*INC(labelnum);*) Write (labelnumber); Writeint (x.a); Writeln (':');
END Fixup;


PROCEDURE Call*(VAR x: Item; rs: SET);
VAR pc0, r: LONGINT;
k,j : INTEGER;
s : OSAS.Ident;
BEGIN RL := 0;
   IF x.type.form = OSAB.Proc THEN
      IF x.mode = OSAB.Const THEN
         IF x.b >= 0 THEN
	    (*Write ('call ');*) (*Write (modname); Write ('_');*) Write (x.label);
	    IF paraindex > -1(*x.type.nofpar # 0*) THEN
	       Write ('(');
	       k := -1;
               REPEAT
	          INC(k);
		  Out.String ('PARAMETER'); Out.Ln;
                  debug(para[k]); Out.String ('LLLLLLLLLLLLLLLLLLLLLLLLLLLLLL'); Out.Ln;
		      Out.String ('para.type.form '); Out.Int ( para[k].type.form, 0); Out.Ln;

		  IF para[k].mode = OSAB.Const THEN
                     IF para[k].type.form = OSAB.Char THEN
                        Write("'"); 
			s[0] := CHR(para[k].a); s[1] := 0X;
			Write (s);
			Write ("'");
		     ELSE
		        Writeint (para[k].a);
		     END;
		  ELSIF para[k].mode = OSAB.Var THEN
		     IF para[k].type.form = OSAB.Array THEN Write ('&') END;
		     IF para[k].type.form = OSAB.String THEN Write ('&') END;
		     IF para[k].type.form = OSAB.Record THEN Write ('&') END;
		     
		     Write (para[k].label)
		  END;
		  IF k # paraindex THEN Write (', ') END;
	       UNTIL k = paraindex;
	       Write (')');
	    END;
	    Writeln (';');
	 (*Put5a(x.a-pc-2)*)
         ELSE (*imported*) (*Put5a(x.a*10000H + fixlist[-x.b]); fixlist[-x.b] := pc-1*)
	    IF x.type.nofpar = 0 THEN Write ('call '); END;
	    Write (x.label);
	    IF x.type.nofpar = 0 THEN
	       Writeln (';')
	    ELSE
            IF paraindex > -1(*x.type.nofpar # 0*) THEN
	       Write ('(');
	       k := -1;
               REPEAT
	          INC(k);
		  Out.String ('PARAMETER'); Out.Ln;
                  debug(para[k]);
		      Out.String ('para.type.form '); Out.Int ( para[k].type.form, 0); Out.Ln;

		  IF para[k].mode = OSAB.Const THEN
                     IF para[k].type.form = OSAB.Char THEN
                        Write("'"); 
			s[0] := CHR(para[k].a); s[1] := 0X;
			Write (s);
			Write ("'");
		     ELSIF para[k].type.form = OSAB.String THEN
		        (*Write ('&');*); Write ('"'); Write (para[k].label); Write ('"');
		     ELSIF para[k].type.form = OSAB.Array THEN
                        Write ('&'); Write (para[k].label);
 
		     ELSE
		        Writeint (para[k].a);
		     END;
		  ELSIF para[k].mode = OSAB.Var THEN
		     IF para[k].type.form = OSAB.Array THEN Write ('&') END;
		     IF para[k].type.form = OSAB.String THEN Write ('&') END;
		     IF para[k].type.form = OSAB.Record THEN Write ('&') END;
		     
		     Write (para[k].label)
		  END;
		  IF k # paraindex THEN Write (', ') END;
	       UNTIL k = paraindex;
	       Write (')');
	    END;
	    Writeln (';');
	       
	    END;
         END
      ELSE 
         (*load(x);*)
         IF x.type.nofpar = 0 THEN Write ('call '); END;
	    Write (x.label);
	    IF x.type.nofpar = 0 THEN
	       Writeln (';')
	    ELSE
            IF paraindex > -1(*x.type.nofpar # 0*) THEN
	       Write ('(');
	       k := -1;
               REPEAT
	          INC(k);
		  Out.String ('PARAMETER'); Out.Ln;
                  debug(para[k]);
		      Out.String ('para.type.form '); Out.Int ( para[k].type.form, 0); Out.Ln;

		  IF para[k].mode = OSAB.Const THEN
                     IF para[k].type.form = OSAB.Char THEN
                        Write("'"); 
			s[0] := CHR(para[k].a); s[1] := 0X;
			Write (s);
			Write ("'");
		     ELSIF para[k].type.form = OSAB.String THEN
		        Write ('&'); Write (para[k].label);
		     ELSIF para[k].type.form = OSAB.Array THEN
                        Write ('&'); Write (para[k].label);
 
		     ELSE
		        Writeint (para[k].a);
		     END;
		  ELSIF para[k].mode = OSAB.Var THEN
		     IF para[k].type.form = OSAB.Array THEN Write ('&') END;
		     IF para[k].type.form = OSAB.String THEN Write ('&') END;
		     IF para[k].type.form = OSAB.Record THEN Write ('&') END;
		     
		     Write (para[k].label)
		  END;
		  IF k # paraindex THEN Write (', ') END;
	       UNTIL k = paraindex;
	       Write (')');
	    END;
	    Writeln (';');
         END
         (*Put0(MOV, LNK, 0, PC); Put0(MOV, PC, 0, x.r);*)
	 (*Release(x.r)*)
      END ;
      IF x.type.base.form # OSAB.NoTyp THEN
         IF rs # {} THEN
	    j := 0;
	    regs := rs; GetReg(r); x.mode := OSAB.Reg; x.r := r;
	    FOR j := 4 TO 0 DO
		IF (j IN regs) THEN Pop(j) END
	    END;
            (*Put0(MOV, r, 0, FP-1); Put4(11, SP, rs)*)  (*restore registers*)
         ELSE
	    regs := {EAX}(*{FP-1}*); 
	    (*r := FP-1*); r := EAX;
         END ;
         x.mode := OSAB.Reg; x.r := r
      ELSE 
      regs := {}
      END
   ELSE OSAS.Mark("not a procedure"); regs := {}
   END
END Call;

PROCEDURE Header*;
VAR i: INTEGER;
BEGIN (*entry := pc; Put4(18, SP, {FP, LNK});  (*STM*)
Put0(MOV, FP, 0, SP)*)  (*FP := SP*)
END Header;

PROCEDURE Type*;
BEGIN
Write (' type ');
END Type;

PROCEDURE TypeName*(VAR obj : OSAB.Object);
BEGIN
Write (obj.name); Write (' : ');
END TypeName;

PROCEDURE ProcedureType* (VAR o : OSAB.Object);
VAR tmpobj, tmpobj2 : OSAB.Object; b : BOOLEAN; ch : CHAR;
BEGIN
IF o # NIL THEN 
   Write (o.name); Write (' : '); Writeln ('dword; '); (* Write ('procedure ');*)
END
(*Out.Ln; Out.Ln;DebugObj(o); in.Char(ch); Out.Ln; DebugObj(o.type.dsc); in.Char(ch); Out.Ln; DebugObj(o.type.dsc.next); in.Char(ch); 
       Write (' procedure '); Write (o.name); Write ('(');
          
	  tmpobj := o.type.dsc; b := FALSE;
	  DebugObj(tmpobj); in.Char(ch);
          REPEAT
          IF (o.class = OSAB.Par) OR (o.class = OSAB.Const) THEN Write (' var ') END;
	  tmpobj.label := tmpobj.name;
          Strings.Append ('_', tmpobj.label);
          (*tmpobj.label := modname;
          Strings.Append ('_', tmpobj.label);
          Strings.Append (obj.name, tmpobj.label);
          Strings.Append ('_', tmpobj.label);
          Strings.Append (tmpobj.name, tmpobj.label);*)
          Write (tmpobj.label); Write (' : ' ); 
           IF    tmpobj.type.form = OSAB.Int THEN Write (' int32')
	   ELSIF tmpobj.type.form = OSAB.Char THEN Write ( 'char');
	   ELSIF tmpobj.type.form = OSAB.Bool THEN Write ('boolean');
	   ELSIF tmpobj.type.form = OSAB.Set THEN Write ('int32');
	   ELSIF tmpobj.type.form = OSAB.Pointer THEN Write ('dword');
	   ELSIF tmpobj.type.form = OSAB.Proc THEN Write ('dword');
	   ELSIF tmpobj.type.form = OSAB.Array THEN Write ('dword');
	   END;
      IF tmpobj.next # NIL THEN
         tmpobj2 := tmpobj.next;
         IF tmpobj2.next.name # tmpobj2.name THEN
             tmpobj := tmpobj.next;
	     Write ('; ');
	 ELSE b := TRUE;
	 END;
              DebugObj(tmpobj); in.Char(ch);
      ELSE b := TRUE
      END
    UNTIL b;
    Writeln (');');*)
END ProcedureType;


PROCEDURE RecordType* (VAR obj : OSAB.Object);
VAR tmpobj : OSAB.Object; b : BOOLEAN;
BEGIN
Out.String ("obj"); Out.Ln; Out.Ln;
DebugObj(obj); Out.Ln;
Out.String ("obj.type.dsc"); Out.Ln; Out.Ln;
DebugObj(obj.type.dsc); Out.Ln;
(*Out.String ("obj.type.typobj"); Out.Ln; Out.Ln;
DebugObj(obj.type.typobj); Out.Ln;*)
Out.String ("obj.type.dsc.next"); Out.Ln; Out.Ln;
DebugObj(obj.type.dsc.next); Out.Ln;
obj.label := modname;
Strings.Append ('_', obj.label);
Strings.Append (obj.name, obj.label);
Write (obj.label); Writeln (' : record');
   tmpobj := obj.type.dsc; b := FALSE;
   REPEAT
      tmpobj.label := tmpobj.name;
      Strings.Append ('_', tmpobj.label);
      (*tmpobj.label := modname;
      Strings.Append ('_', tmpobj.label);
      Strings.Append (obj.name, tmpobj.label);
      Strings.Append ('_', tmpobj.label);
      Strings.Append (tmpobj.name, tmpobj.label);*)
      Write (tmpobj.label); Write (' : ' ); 
           IF    tmpobj.type.form = OSAB.Int THEN Writeln (' int32;')
	   ELSIF tmpobj.type.form = OSAB.Char THEN Writeln ( 'char;');
	   ELSIF tmpobj.type.form = OSAB.Bool THEN Writeln ('boolean;');
	   ELSIF tmpobj.type.form = OSAB.Set THEN Writeln ('int32;');
	   ELSIF tmpobj.type.form = OSAB.Pointer THEN Writeln ('dword;');
	   ELSIF tmpobj.type.form = OSAB.Proc THEN Writeln ('dword;');
	   ELSIF tmpobj.type.form = OSAB.Array THEN Writeln ('array is not implemented');
	   END;
      IF tmpobj.next # NIL THEN
       tmpobj := tmpobj.next
      ELSE b := TRUE
      END
    UNTIL b;
    Writeln ('endrecord;');



END RecordType;

PROCEDURE Procedure*(VAR proc : OSAB.Object);
BEGIN
(*DebugObj(proc); Out.String ('a seychas'); Out.Ln; DebugObj(proc.type.dsc); Out.String('aa'); Out.Ln; DebugObj(proc.type.dsc.next);*)
vvv := TRUE;
Write ('procedure '); Write (modname); Write ('_'); Write (proc.name);
IF proc.expo THEN
Writeh ('procedure '); Writeh (modname); Writeh ('_'); Writeh (proc.name);
END
END Procedure;

PROCEDURE Procpar*(e : BOOLEAN);
BEGIN
IF vvv THEN
   Write ('(');
   IF e THEN Writeh('('); END
END
END Procpar;

PROCEDURE Procvar*(VAR obj : OSAB.Object; e : BOOLEAN);
BEGIN
IF  vvv THEN
  IF (obj.class = OSAB.Par) OR (obj.class = OSAB.Const) THEN 
      Write ('var '); 
      IF e THEN Writeh ('var ') END;
  END;
  COPY(modname, obj.label);
  Strings.Append ('_', obj.label); Strings.Append (obj.name, obj.label);
  Write (obj.label);
  Write (' : '); 
  IF e THEN Writeh (obj.label); Writeh (' : ') END;
  IF obj.type.form = OSAB.Bool THEN 
     Write ('boolean');
     IF e THEN Writeh ('boolean') END;
  ELSIF obj.type.form = OSAB.Char THEN 
     Write ('char');
     IF e THEN Writeh ('char') END;
  ELSIF (obj.type.form = OSAB.Int) OR (obj.type.form = OSAB.Set) THEN 
     Write ('int32');
     IF e THEN Writeh ('int32') END;
  ELSIF obj.type.form = 12 THEN
     Write ('dword '); (* pointer to array *)
     IF e THEN Writeh ('dword '); END;
     (*IF obj.type.base.form = OSAB.Bool THEN Write ('boolean')
     ELSIF obj.type.base.form = OSAB.Char THEN Write ('char')
     ELSIF obj.type.base.form = OSAB.Int THEN Write ('int32')
     END;*)
  ELSE Out.String ('type not implemented'); Out.Ln;
  END;
END
END Procvar;

PROCEDURE Procvarnext*(e : BOOLEAN);
BEGIN
IF vvv THEN
   Write ('; ');
   IF e THEN Writeh('; ') END
END
END Procvarnext;

PROCEDURE Procvarnext2*(e: BOOLEAN);
BEGIN
IF vvv THEN
Writeln ('; ');
IF e THEN Writelnh ('; external;') END;
END
END Procvarnext2;

PROCEDURE Procvarend*(e : BOOLEAN);
BEGIN
IF vvv THEN
   Writeln (');'); IF e THEN Writelnh ('); external;') END;
END
END Procvarend;

PROCEDURE function* (v : INTEGER);
BEGIN
(*IF (v >= OSAB.Byte) & (v <= OSAB.Char) THEN
Writeln ('RETURNS ("al");');
ELSE 
Writeln ('RETURNS ( "eax");');
END;*)
END function;

PROCEDURE SaveRegs*(VAR rs: SET);
VAR k : INTEGER;
BEGIN rs := regs;
IF regs # {} THEN
(*Put4(18, SP, regs);*)
   FOR k := 0 TO 4 DO
   IF k IN regs THEN Push(k) END; 
   END;
regs := {} 
END  (*STM*)
END SaveRegs;

PROCEDURE Procbegin*(VAR s : OSAS.Ident);
VAR k : INTEGER;
BEGIN
(*Writeln ('');*)
Write ('begin '); Write (modname); Write ('_'); Write(s); Writeln (';');
(*IF regs # {} THEN
   FOR k := 0 TO 4 DO
      IF k IN regs THEN Push(k); END;
   END;
END*)
END Procbegin;

PROCEDURE Procend* (VAR s : OSAS.Ident);
VAR k : INTEGER;
BEGIN
Write ('end '); Write (modname); Write ('_'); Write(s); Writeln (';');
(*FOR k := 4 TO 0 DO
   IF k IN regs THEN Pop(k); END;
END;*)
END Procend;

PROCEDURE Exported*;
BEGIN
(*Writeln (' external;');*)
Writelnh (' external;');
END Exported;

PROCEDURE nl*;
BEGIN
Writeln ('');
END nl;


PROCEDURE Enter*(leaf, int: BOOLEAN; level, regvarno: INTEGER; parsize, varsize: LONGINT);
VAR n, nofregs: LONGINT;
BEGIN curlev := level; RL := regvarno; RH := ESI(*FP*); nofregs := parsize DIV 4;
IF ~int THEN (*procedure prolog*)
IF ~leaf THEN
Put4(18, SP, {FP-nofregs .. FP, LNK});  (*save parameters, FP, LNK*)
Put1(ADD, FP, SP, nofregs*4)    (*FP := SP + parsize*)
ELSE Put4(18, SP, {FP, LNK}); Put0(MOV, FP, 0, SP); RH := FP - nofregs
END
ELSE (*interrupt procedure*)
IF ~leaf THEN (*normal interrupt handler prolog*)
Put4(18, SP, {0 .. FP, LNK});  (*save R0 - R11, FP, LNK*)
Put0(MOV, FP, 0, SP); RL := 0   (*FP := SP; no code for fast interrupt*)
END
END ;
IF varsize > 0 THEN  (*SP := SP - varsize*)
Put1(SUB, SP, SP, varsize MOD C8); varsize := varsize DIV C8;
IF varsize > 0 THEN Put1a(SUB, SP, SP, varsize, 12) END
END
END Enter;

PROCEDURE Return*(leaf, int: BOOLEAN; offset, resreg, form: INTEGER; VAR x: Item);
VAR res: Item; xname : OSAS.Ident;
BEGIN 
Out.String ('PROCEDURE Returnn'); Out.Ln;
Out.String ('leaf='); IF leaf THEN Out.String ('true') ELSE Out.String ('false') END;
Out.Ln;
Out.String ('int=');  IF leaf THEN Out.String ('true') ELSE Out.String ('false') END;
Out.Ln;
Out.String ('offset='); Out.Int (offset,0); Out.Ln;
Out.String ('resreg='); Out.Int (resreg,0); Out.Ln;
Out.String ('form='); Out.Int (form,0); Out.Ln;
debug(x);

resreg :=EAX;(* FP -resreg - 1;*)
IF (form # OSAB.NoTyp) & ~((x.mode = OSAB.Reg) & (x.r = resreg)) THEN
Out.String ('aaa'); Out.Ln;
res.mode := OSAB.Reg; res.r := resreg; res.type := x.type;
Out.String ('Store'); Out.Ln;
ConvertRegNumToName (x.r, xname);
Write ('mov ('); Write (xname); Writeln (', eax);');
(*
Store(res, x)*)
END ;

IF ~int THEN (*procedure epilog*)
(*Put0(MOV, SP, 0, FP);   (*SP := FP*)
Put4(11, SP, {FP, PC})*)    (*restore FP, PC*)

END;(*
ELSE
IF ~leaf THEN (*normal interupt handler epilog*)
Put0(MOV, SP, 0, FP); Put4(11, SP, {0 .. FP, LNK});  (*restore R0 - R11, FP, LNK*)
END ;
Put1(SUB+1, PC, LNK, offset)    (*PC := LNK - offset*)
END ;*)
RL := 0;  RH := ESI(*FP*); regs := {}; (*FixupConstants*)
END Return;

PROCEDURE Import*(module : OSAS.Ident);
BEGIN
INC(impind);
imports[impind] := module;
IF module # 'SYSTEM' THEN
Write ('#include ("'); Write (module); Write ('.hhf'); Writeln ('")')
ELSE
Writeln ('#include ("stdlib.hhf")');
END
END Import;

(* In-line code procedures*)

PROCEDURE Increment*(upordown: LONGINT; VAR x, y: Item);
VAR op, inc, r, r1: LONGINT; rname : OSAS.Ident;
BEGIN
      debug(x); debug(y);
      IF upordown = 0 THEN op := ADD ELSE op := SUB END ;
      IF y.type.form = OSAB.NoTyp THEN inc := 1
      ELSE (*integer constant*) inc := y.a;
         IF (inc < 0) OR (inc >= (*100*)1000H) THEN OSAS.Mark("bad increment") END
      END ;
      IF x.mode = OSAB.Reg THEN (*Put1(op, x.r, x.r, inc)*) ConvertRegNumToName (x.r, rname); 
                                                            Write ('inc ('); Write (rname); Writeln (');');
      ELSE GetReg(r); ConvertRegNumToName (r, rname);
         IF (x.mode = OSAB.Var) OR (x.mode = OSAB.RegI) THEN
            (*Put2(LDR, r, x.r, x.a); Put1(op, r, r, inc); Put2(STR, r, x.r, x.a)*)
	    Write ('inc ('); Write (x.label); Writeln (');');
         ELSIF x.mode = OSAB.Par THEN
            (*Put2(LDR, r, x.r, x.a); GetReg(r1); Put2(LDR, r1, r, x.b);
            Put1(op, r1, r1, inc); Put2(STR, r1, r , x.b); Release(r1)*)
	     Write ('inc ('); Write (x.label); Writeln (');');
         ELSIF x.mode = RegX THEN
           (*Put3(25, r, x.r, x.a, x.b); Put1(op, r, r, inc); Put3(24, r, x.r, x.a, x.b);*)
           Write ('inc ('); Write (x.label); Writeln (');');
	   Release(x.a)
         ELSE OSAS.Mark("not a variable")
         END ;
      Release(r); Release(x.r)
      END
END Increment;

PROCEDURE Assert*(VAR x, y: Item);
VAR k: LONGINT;
BEGIN k := y.a MOD 10000H;
IF x.mode # CC THEN loadCC(x) END ;
IF x.b = 0 THEN Put6(x.r, k) ELSE CFJump(x); Put6(14, k) END ;
Fixup(x)
END Assert; 

PROCEDURE New*(VAR x, y: Item);
VAR T, z: Item; base: LONGINT;
BEGIN loadAdr(x); 
IF x.type.base = NIL THEN OSAS.Mark("no pointer base")
ELSE MakeItem(T, x.type.base.typobj); T.mode := OSAB.Var; loadAdr(T); Release(T.r)
END ;
Put5a(fixlistMU + 20000H); fixlistMU := pc - 1; Release(x.r)
END New;

PROCEDURE Pack*(VAR x, y: Item);
VAR z: Item;
BEGIN z := x; load(x); load(y);
Put0a(ADD, x.r, x.r, y.r, 0, 23); Store(z, x); Release(y.r)
END Pack;

PROCEDURE Unpk*(VAR x, y: Item);
VAR z, e0: Item;
BEGIN z := x; load(x); e0.mode := OSAB.Reg; GetReg(e0.r);
Put0a(MOV, e0.r, x.r, x.r, 1, 23);
Put1(SUB, e0.r, e0.r, 127); Store(y, e0); Put0a(SUB, x.r, x.r, e0.r, 0, 23);
Store(z, x) ; Release(e0.r)
END Unpk;

PROCEDURE Get*(VAR x, y: Item);
VAR cd: LONGINT;
BEGIN
IF y.type.size = 1 THEN cd := LDR+4 ELSE cd := LDR END ;
load(x); x.a := 0;
IF y.mode IN {OSAB.Var, OSAB.Par, OSAB.RegI, RegX} THEN
Put2(cd, x.r, x.r, x.a); x.mode := OSAB.Reg; x.type := y.type; Store(y, x)
ELSIF (y.mode = OSAB.Reg) & (y.r < RL) THEN Put2(cd, y.r, x.r, x.a); Release(x.r)
ELSE OSAS.Mark("not a variable"); Release(x.r)
END
END Get;

PROCEDURE Put*(VAR x, y: Item);
VAR cd: LONGINT;
BEGIN
IF y.type.size = 1 THEN cd := STR+4 ELSE cd := STR END ;
load(x); load(y); Put2(cd, y.r, x.r, 0); Release(y.r); Release(x.r)
END Put;

PROCEDURE PSR*(op: LONGINT; VAR msk, x: Item);  (*Program Status Reister*)
VAR r: LONGINT; z: Item;
BEGIN
IF op = 0 THEN (*LDPSR*)
load(x); Put0((msk.a MOD 2)*4 + 18, 15, 9, x.r); Release(x.r)   (*MSR*)
ELSIF op = 1 THEN (*STPSR*)
GetReg(r); Put0((msk.a MOD 2)*4 + 16, r, 15, 0);   (*MRS*)
z.mode := OSAB.Reg; z.r := r; z.type := OSAB.intType; Store(x, z)
END
END PSR;

PROCEDURE CPR*(op: LONGINT; VAR cpno, cpreg, x: Item);  (*Coprocessor Register*)
VAR y: Item;
BEGIN
IF op = 0 THEN (*LDCPR*)
load(x); Put1(0C0H, x.r, cpreg.a MOD 10H, cpno.a + 10H); Release(x.r)   (*MCR*)
ELSIF op = 1 THEN (*STCPR*)
GetReg(y.r); Put1(0C1H, y.r, cpreg.a MOD 10H, cpno.a + 10H);   (*MRC*)
y.mode := OSAB.Reg; y.type := OSAB.intType; Store(x, y)
END
END CPR;

PROCEDURE Flush*(VAR x: Item);  (*flush caches*)
BEGIN PutC(-301527296(*0EE070F00H*) + x.a); (*MCR*)
PutC(-509607936(*0E1A00000H*)); PutC(-509607936(*0E1A00000H*)); PutC(-509607936(*0E1A00000H*)); PutC(-509607936(*0E1A00000H*));  (*NOP*)
END Flush;

PROCEDURE AddC*(VAR x, y, z: Item);
BEGIN Put0(11, x.r, y.r, z.r)  (*add with carry; x := y + z; must be register variables*)
END AddC;

PROCEDURE MulD*(VAR x, y, z: Item);
BEGIN Put0c(8, x.r+1, y.r, z.r, x.r)  (*long mult; xx := y * z; must be register variables*)
END MulD;

(*In-line code functions*)

PROCEDURE Abs*(VAR x: Item);
VAR r, cnt: LONGINT;
BEGIN
IF x.type.form = OSAB.Int THEN
IF (x.mode = OSAB.Reg) & ((x.r < RL) OR (x.r >= RH)) THEN (*reg var*)
GetReg(r); Put0(MOV+1, r, 0, x.r)
ELSE load(x); r := x.r; Put1(CMP, 0, r, 0)
END ;
Put1(RSB+0D00H, r, r, 0); x.r := r
ELSIF x.type.form = OSAB.Real THEN load(x); r := x.r; Put1a(28, r, x.r, 2, 1); x.r := r  (*BIC sign*)
ELSE (*Set*)
IF (x.mode = OSAB.Reg) & ((x.r < RL) OR (x.r >= RH)) THEN (*reg var*)
GetReg(r); Put0(MOV, r, 0, x.r)
ELSE load(x); r := x.r
END ;
GetReg(cnt); Put1(MOV, cnt, 0, 0);
Put0a(MOV+1, r, 0, r, 1, 1); Put1(ADD+2, cnt, cnt, 0);  (*LSR x,1; ADDC*)
Put5(1, -4); Put0(MOV, r, 0, cnt); Release(cnt); x.r := r; x.type := OSAB.intType
END
END Abs;

PROCEDURE Odd*(VAR x: Item);
BEGIN load(x); Put1(17, 0, x.r, 1); Release(x.r); SetCC(x, 1)  (*TST*)
END Odd;

PROCEDURE Floor*(VAR x: Item);
BEGIN load(x);
IF regs = {FP-1} THEN Put5a(70000H + fixlistFP); fixlistFP := pc-1
ELSE OSAS.Mark("simplify expression!")
END
END Floor;

PROCEDURE Float*(VAR x: Item);
BEGIN load(x);
IF regs = {FP-1} THEN Put5a(80000H + fixlistFP); fixlistFP := pc-1
ELSE OSAS.Mark("simplify expression!")
END 
END Float;

PROCEDURE Ord*(VAR x: Item);
BEGIN
IF x.mode IN {OSAB.Var, OSAB.Par, OSAB.RegI, RegX} THEN load(x) END
END Ord;

PROCEDURE Len*(VAR x: Item);
BEGIN
IF x.type.len >= 0 THEN x.mode := OSAB.Const; x.a := x.type.len
ELSIF x.mode = OSAB.Par THEN (*openarray*) x.mode := OSAB.Var; DEC(x.a, 4)
ELSE (*x.mode = OSAB.RegI*) x.mode := OSAB.Reg; DEC(x.r)
END
END Len;

PROCEDURE Shift*(fct: LONGINT; VAR x, y: Item);
VAR r: LONGINT;
BEGIN (*LSL, LSR, ASR, ROR*) load(x);
IF y.mode = OSAB.Const THEN GetReg1(r, x.r); Put0a(MOV+1, r, 0, x.r, fct, y.a MOD 20H)
ELSE load(y); GetReg2(r, x.r, y.r); Put0b(MOV+1, r, 0, x.r, fct, y.r)
END ;
x.r := r
END Shift;

PROCEDURE Adr*(VAR x: Item);
VAR r: LONGINT;
BEGIN
IF x.mode IN {OSAB.Var, OSAB.Par, OSAB.RegI, RegX} THEN loadAdr(x); x.mode := OSAB.Reg
ELSIF (x.mode = OSAB.Const) & (x.type.form = OSAB.Proc) THEN load(x)
ELSE OSAS.Mark("not addressable")
END
END Adr;

PROCEDURE Bit*(VAR x, y: Item);
VAR r: LONGINT;
BEGIN
IF y.mode = OSAB.Const THEN
load(x); x.a := 0; GetReg(r); Put2(LDR, r, x.r, x.a); Put0a(MOV+1, r, 0, r, 3, y.a+1);
Release(r); Release (x.r); SetCC(x, 4)
ELSE OSAS.Mark("Bit no. must be a constant")
END
END Bit;

PROCEDURE Xor*(VAR x, y: Item);
VAR r: LONGINT;
BEGIN load(x);
IF y.mode = OSAB.Const THEN GetReg(r); Put1(2, r, x.r, y.a)  (*!*)
ELSE load(y); GetReg2(r, x.r, y.r); Put0(2, r, x.r, y.r)
END ;
x.r := r
END Xor;

PROCEDURE Overflow*(VAR x: Item);
BEGIN (*x.mode = Const*)
IF x.a = 0 THEN x.r := 2 ELSE x.r := 6 END ;
x.mode := CC; x.a := 0; x.b := 0
END Overflow;

PROCEDURE Null*(VAR x: Item);
VAR r: LONGINT;
BEGIN load(x); GetReg(r); Put1a(29, r, x.r, 2, 1); Release(r); Release(x.r);  (*BIC*)
x.mode := CC; x.r := 0; x.a := 0; x.b := 0
END Null;

PROCEDURE CheckRegs*;
VAR pc0: INTEGER;
BEGIN
IF regs # {} THEN
OSAS.Mark("compiler error; reg stack not empty"); regs := {}
END ;
(*IF pc >= maxCode-100H THEN OSAS.Mark("program too long"); pc := 0 END ;
IF (firstfixloc # 0) & (pc - firstfixloc >= 220) THEN
pc0 := pc; INC(pc); FixupConstants; code[pc0] := -369098752(*0EA000000H*) + pc - pc0 - 2
END ;
regs := {}*)
END CheckRegs;

PROCEDURE Open* (VAR flname : ARRAY OF CHAR; modid : OSAS.Ident);
VAR i: INTEGER;
name, hname : OSAS.Ident;
BEGIN curlev := 0; pc := 0; icx := 0; scx := 0; xrefx := 0; RL := 0; RH := ESI(*FP*); regs := {}; (*savedregsnum := 0; 
FOR i := 0 TO LEN(savedregs)-1 DO savedregs[i] := PC END;*)
firstfixloc := 0; fixlistFP := 0; fixlistMU := 0;
(*
FOR i := 0 TO maxImp-1 DO fixlist[i] := 0 END ;
PutC(0);  (*avoid fixups at address 0!*)
*)
(* inserted by me *)
regs := {};
COPY (modid, modname);
OSAB.MakeFileName(modid, name, ".hla"); (*write code file*)
OSAB.MakeFileName(modid, hname, '.hhf');
Out.String ("name is "); Out.String (name); Out.Ln;
(*F := Files.New(name); Files.Set(R, F, 0);*)
CreateFile(name); CreateFileh(hname);
 Out.String ("file created"); Out.Ln;
IF main THEN
Write ('program '); Write (modname); Write ("_ "); Writeln (';');
ELSE
Write ('unit '); Write (modname); Write ("_ "); Writeln (';');
END;
COPY (name, asmname);
Write ('#include ("'); Write (modname); Write ('.hhf'); Writeln ('")')
END Open;

(*
PROCEDURE Open*;
VAR i: INTEGER;
BEGIN curlev := 0; pc := 0; icx := 0; scx := 0; xrefx := 0; RL := 0; RH := FP; regs := {};
firstfixloc := 0; fixlistFP := 0; fixlistMU := 0;
FOR i := 0 TO maxImp-1 DO fixlist[i] := 0 END ;
PutC(0);  (*avoid fixups at address 0!*)
END Open;
*)

PROCEDURE Begin*;
VAR i : INTEGER;
BEGIN
Write ('begin '); Write (modname); Write ('_ '); Writeln (';');
FOR i := 0 TO impind DO
   Write ('call '); Write (imports[i]); Writeln ('_Init_ ;'); 
END
END Begin;

PROCEDURE Init*;
VAR i : INTEGER;
BEGIN
Write ('procedure '); Write (modname); Write ('_'); Write ('Init'); Writeln ('_;');
Writeh ('procedure '); Writeh (modname); Writeh ('_'); Writeh ('Init'); Writelnh ('_; external;');
Write ('begin '); Write (modname); Write ('_'); Writeln ('Init_;');
FOR i := 0 TO impind DO
   Write ('call '); Write (imports[i]); Writeln ('_Init_ ;'); 
END
END Init;

PROCEDURE Close*(VAR modid: OSAS.Ident; key, datasize: LONGINT);
VAR obj: OSAB.Object; i, nofentries: INTEGER;
name: OSAS.Ident;
(*F: Files.File; R: Files.Rider;*)
BEGIN Put0(MOV, SP, 0, FP); Put4(11, SP, {FP, PC}); (*MOV, LDM*)
FixupConstants;
(*
OSAB.MakeFileName(modid, name, ".arm"); (*write code file*)
F := Files.New(name); Files.Set(R, F, 0); Files.WriteString(R, modid); Files.WriteLInt(R, key);
Files.WriteLInt(R, fixlist[0]);  (*self*)
obj := OSAB.topScope.next;
WHILE obj.class = OSAB.Mod DO  (*list of imported modules*)
IF obj.name # "SYSTEM" THEN
Files.WriteString(R, obj(OSAB.Module).name1); Files.WriteLInt(R, obj.val); Files.WriteLInt(R, fixlist[obj.lev])
END ;
obj := obj.next
END ;
IF fixlistFP > 0 THEN 
Files.WriteString(R, "FPU"); Files.WriteLInt(R, FPUkey); Files.WriteLInt(R, fixlistFP)
END ;
IF fixlistMU > 0 THEN 
Files.WriteString(R, "MAU"); Files.WriteLInt(R, MAUkey); Files.WriteLInt(R, fixlistMU)
END ;
Files.Write(R, CHR(0));
obj := OSAB.topScope.next; nofentries := 0;
WHILE obj # OSAB.guard DO  (*list of commands*)
IF obj.expo THEN
IF (obj.class = OSAB.Const) & (obj.type.form = OSAB.Proc)
 & (obj.type.nofpar = 0) & (obj.type.base = OSAB.noType) THEN
Files.WriteString(R, obj.name); Files.WriteLInt(R, obj.val)
END ;
IF (obj.class = OSAB.Const) & (obj.type.form = OSAB.Proc) OR (obj.class = OSAB.Typ) 
OR (obj.class = OSAB.Var) THEN INC(nofentries)
END
END ;
obj := obj.next
END ;
Files.Write(R, CHR(0));
Files.WriteLInt(R, nofentries); Files.WriteLInt(R, entry);
obj := OSAB.topScope.next;
WHILE obj # OSAB.guard DO  (*list of exported procedures (entry points) and variables*)
IF obj.expo THEN
IF (obj.class = OSAB.Const) & (obj.type.form = OSAB.Proc) OR (obj.class = OSAB.Typ) 
OR (obj.class = OSAB.Var) THEN Files.WriteLInt(R, obj.val);
END
END ;
obj := obj.next
END ;
Files.WriteLInt(R, datasize); Files.WriteLInt(R, pc); i := 0;
WHILE i < pc DO Files.WriteLInt(R, code[i]); INC(i) END ;
*)
IF ~ main THEN Write ('end '); Write (modname); Write ('_'); Writeln ('Init_;') ;END;
Write ('end '); Write (modid); Write ("_ "); Writeln (";");
CloseFile;
CloseFileh;
END Close;

BEGIN Out.Open; loob := 0;
cond[0] := 0; cond[1] := 1; cond[2] := 11; cond[3] := 13; cond[4] := 12; cond[5] := 10;
revcond[0] := 0; revcond[1] := 1; revcond[2] := 12; revcond[3] := 10; revcond[4] := 11; revcond[5] := 13;
landnext :=0; landend := 0; landfalse := 0;
lornext :=0; lorend := 0; lortrue := 0;
labelnum := 0; impind := -1;
vvv := FALSE;
END OSAG.
