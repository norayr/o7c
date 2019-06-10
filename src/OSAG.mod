MODULE OSAG; (* NW 1.7.97 / 2.5.99 / 24.11.99 / 5.10.2007*)
IMPORT SYSTEM, 
(*Files := OakFiles,*)(* ofront *)
Files, TextRider,   (* ooc *)
Out := OakOut,OSAS, OSAB, Strings := OakStrings, IntStr;
(*in := In;*) (*ooc, for debug purposes *)

(* Code generator for Oberon compiler for i386 processor *)
(* gas backend *)

CONST WordSize* = 4;

R0 = 0;  R1 = 1;    R2 = 2;  R3 = 3;  R4 = 4;  R5 = 5;  R6 = 6;  R7 = 7;  R8 = 8;  R9 = 9;

EAX = 0; EBX = 1 ; ECX = 2; EDX = 3; ESI = 4; EDI = 5; EBP = 6; ESP = 7; EIP = 8; EFLAGS = 9;

r0 = '%eax';
r1 = '%ebx'; 
r2 = '%ecx';
r3 = '%edx';
r4 = '%esi';
r5 = '%edi';
r6 = '%ebp';
r7 = '%esp';
r8 = '%eip';
r9 = '%eflags';
(* 8 bit registers *)

rr0 = '%al';
rr1 = '%bl';
rr2 = '%cl';
rr3 = '%dl';

MaxRegs = ESI(*EBP*);

(*dedicated registers*) 
FP* = EBP (*12*);  SP = ESP  (*13*); LNK = EIP (*14*); PC = 15;  (*means memory*) (* fp is ebp, sp is esp, lnk is eip*)

WRegLim* = FP-2;  (*minimum of 2 work registers in leaf procedures*)

(*maxCode = 10000; REMOVE? *) maxImp = 12; ITlen = 64; STlen = 128;

(*C8 = 100H; C12 = 1000H; C16 = 10000H; C24 = 1000000H;*)

RegX = 10; CC = 11; (*internal item modes*)

MAUkey =-853891096 (*0CD1AA7E8H*); FPUkey = -1949024149 (*8BD4406BH*); (* REMOVE? *)

(*frequently used opcodes*)
SUB = 4; RSB = 7; ADD = 8; CMP = 21; MOV = 26; MVN = 30; STR = 16; LDR = 17; (* REMOVE? *)

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
labelstring = '_label_string_';
labelmod    = '_label_mod';
labelfloat  = '_label_float_';

TYPE Item* = RECORD
mode*: INTEGER;
type*: OSAB.Type;
a*, b*, r* : LONGINT;    (* REMOVE zvezdochki? *)
rdo*: BOOLEAN;  (*read only*)
name*,
label*, 
ilabel* : OSAS.Ident; (* label is item label for assembly *)
jnum*, jifnum*, jthen*, jor*, jand*, jbool* : LONGINT;
or*, 
and*, 
not* : BOOLEAN;
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

VAR pc, curlev: INTEGER;   (*program counter; current level*)                            (*REMOVE?*)
entry, firstfixloc: INTEGER;   (*main entry point; first ref to const to be fixed up*)   (*REMOVE?*)
RL: LONGINT;  (*registers variables in registers R[0] ... R[RL-1]*)
RH: LONGINT;  (*parameters in R[FP-1] ... R[H], in descending order*)
regs: SET;   (*used registers in range R[L] ... R[H-1]*)

landnext, landend, landfalse,
lornext, lorend, lortrue           : LONGINT; 

labelnum, labelstr, labmodnum : LONGINT;
labelfloatinx : LONGINT;
para : ARRAY 21 OF Item;
paraindex : INTEGER;

imports : ARRAY maxImp+1 OF OSAS.Ident;
impind : INTEGER;

icx, scx, xrefx: INTEGER;  (*indices of integer and string constant, and ext. ref. tables*)
fixlistFP, fixlistMU: LONGINT;  (*fixup lists for modules FPU and MAU*)

cond, revcond: ARRAY 6 OF INTEGER;  (*condition codes for relations*)
fixlist: ARRAY maxImp OF LONGINT;
TIC: ARRAY ITlen OF   (*Table of Integer Constants*)
	       RECORD val, adr: LONGINT END ;
TSC: ARRAY STlen OF    (*Table of String Constants*)
	       RECORD index, adr: LONGINT END ;
TXR: ARRAY ITlen OF    (*Table of external references*)
	       RECORD ref, adr: LONGINT END ;
main*, dbg*, wasbegin, check* : BOOLEAN;
modname* : OSAS.Ident;
asmname* : OSAS.Ident;
vvv : BOOLEAN; (*REMOVE when procedures changed*)
(* ------------------------------------ non portable between compilers file routins ------------------------------ *)
(*
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
(*rh.WriteString (ch);
rh.WriteLn*)
END Writelnh;


PROCEDURE CloseFileh;
BEGIN
fh.Close;
END CloseFileh;
*)
(* ----------------------------------------------------- ------------------------------------------------------------------*)

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





(*------*)




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
(*Write ("// x.mode="); Writeint(x.mode); Writeln ('');
Write ("// x.r="); Writeint (x.r); Writeln ('');
Write ("// x.a="); Writeint (x.a); Writeln ('');
Write ("// x.b="); Writeint (x.b); Writeln ('');
Write ("// x.label="); Writeln (x.label);
Write ("// x.ilabel="); Writeln (x.ilabel);*)
END debugitem;
PROCEDURE DebugObj( VAR obj : OSAB.Object);
BEGIN
(*Out.String ("obj.name = "); Out.String (obj.name); Out.Ln;
Out.String ("obj.class = "); Out.Int (obj.class, 0); Out.Ln;
Out.String ("obj.level = "); Out.Int (obj.lev, 0); Out.Ln;
Out.String ("obj.label = "); Out.String (obj.label); Out.Ln;
Out.String ("obj.ilabel = "); Out.String (obj.ilabel); Out.Ln;
Out.String ("obj.val = "); Out.Int (obj.val, 0); Out.Ln;
Out.Ln;
Out.String ("obj.class = "); Out.Int (obj.class, 0); Out.Ln;
Out.String ("obj.type.form = "); Out.Int(obj.type.form, 0); Out.Ln;
Out.String ("obj.type.nofpar = "); Out.Int (obj.type.nofpar, 0); Out.Ln;
Out.String ("obj.type.len = "); Out.Int (obj.type.len, 0); Out.Ln;
Out.String ("obj.type.size = "); Out.Int (obj.type.size, 0); Out.Ln;
*)
END DebugObj;

PROCEDURE ShowRegs;
VAR l : LONGINT;
BEGIN
l := -1;
REPEAT
 INC(l);
 IF l IN regs THEN 
    Out.Int (l,0);
    Out.String (" Used")
 ELSE
    Out.Int (l, 0);
    Out.String (" Free")
 END;
 Out.Ln
UNTIL l = 5;
END ShowRegs;

PROCEDURE ConvertRegNumToName (ham : LONGINT; VAR name : OSAS.Ident);
BEGIN
CASE ham OF
0 : name := r0
| 1 : name := r1
| 2 : name := r2
| 3 : name := r3
| 4 : name := r4
| 5 : name := r5
| 6 : name := r6
| 7 : name := r7
| 8 : name := r8
| 9 : name := r9
| 10: name := 'ankap'
| 15 : Out.String ("got 15 - memory!") (* REMOVE?*)
END
END ConvertRegNumToName;

PROCEDURE Convert8RegNumToName (ham : LONGINT; VAR name : OSAS.Ident);
BEGIN
CASE ham OF
0 : name := rr0
| 1 : name := rr1
| 2 : name := rr2
| 3 : name := rr3
| 15 : Out.String (" memory???") (*REMOVE*)
END
END Convert8RegNumToName;

PROCEDURE Movl (a, b : LONGINT);        (* mov instructions to load use movsx for bytes, to store, movb for bytes *)
VAR aname, bname : OSAS.Ident;
BEGIN
ConvertRegNumToName (a, aname);
ConvertRegNumToName (b, bname);   
(*Write ("mov ("); Write (aname); Write (","); Write (bname); Writeln (");");*) (* hla syntax *)
Write ("movl "); Write (aname); Write (","); Write (bname); Writeln (";");      (* gas syntax   *)
END Movl;

PROCEDURE MovConstToReg (a, r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN
ConvertRegNumToName (r, rname);
Write ("movl $"); Writeint (a); Write (", "); Writeln (rname);
END MovConstToReg;

PROCEDURE Mov8ConstToReg (a, r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN
Convert8RegNumToName (r, rname);
Write ("movb $"); Writeint (a); Write (", "); Writeln (rname)
END Mov8ConstToReg;


PROCEDURE MovConstToVar (a : LONGINT; x : Item);
VAR
BEGIN
IF x.type.size > 1 THEN
Write ('movl $');
ELSIF x.type.size = 1 THEN
Write ('movb $');
END;
Writeint (a);
Write (', ');
Writeln (x.label);
END MovConstToVar;

PROCEDURE MovVarToReg (VAR y : Item; r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN
IF y.type.size > 1 THEN
ConvertRegNumToName (r, rname);
Write ('movl ');
ELSIF y.type.size = 1 THEN
ConvertRegNumToName (r, rname);
(*Convert8RegNumToName (r, rname);*)
Write ('movsbl '); (* movb *)
END;
Write (y.label); Write (', '); Writeln (rname);
END MovVarToReg;

PROCEDURE MovParamToReg (fp, offset, r : LONGINT; VAR x : Item);
VAR fpname, rname : OSAS.Ident;
BEGIN
ConvertRegNumToName (fp, fpname);
IF x.type.size = 1 THEN
ConvertRegNumToName (r, rname);
(*Convert8RegNumToName (r, rname);*)
(*MovConstToReg (0, r);*)
Write ('movsbl ');
ELSIF x.type.size >= 4 THEN
ConvertRegNumToName (r, rname);
Write ('movl ');
END;
Writeint (offset);
Write('('); Write (fpname); Write('), ');
Writeln (rname);
END MovParamToReg;

PROCEDURE MovRegToVar (r : LONGINT; VAR y : Item);
VAR rname : OSAS.Ident;
BEGIN
IF y.type.size > 1 THEN
ConvertRegNumToName (r, rname);
Write ('movl ');
ELSIF y.type.size = 1 THEN
Convert8RegNumToName (r, rname);
Write ('movb ');
END;
Write (rname); Write (', '); Writeln (y.label);
END MovRegToVar;

PROCEDURE MovRegToAdr (a : LONGINT; b : Item);
VAR aname, bname : OSAS.Ident; s : LONGINT;
BEGIN
ConvertRegNumToName (b.r, bname);
s := b.type.size;
IF (b.type.form = OSAB.Array) THEN s := b.type.base.size END;
IF s > 1 THEN
ConvertRegNumToName (a, aname);
Write ('movl ')
ELSIF s = 1 THEN
Convert8RegNumToName (a, aname);
Write ('movb ')
END;
Write (aname); Write (', ('); Write(bname); Writeln (')');
END MovRegToAdr;

PROCEDURE MovRegToAdr2 (a, b, size : LONGINT);
VAR aname, bname : OSAS.Ident; s : LONGINT;
BEGIN
ConvertRegNumToName (b, bname);
IF size > 1 THEN
ConvertRegNumToName (a, aname);
Write ('movl ')
ELSIF size = 1 THEN
Convert8RegNumToName (a, aname);
Write ('movb ')
END;
Write (aname); Write (', ('); Write(bname); Writeln (')');
END MovRegToAdr2;

PROCEDURE MovConstToAdr (a, b, size : LONGINT);
VAR aname, bname : OSAS.Ident; s : LONGINT;
BEGIN
ConvertRegNumToName (b, bname);
IF size > 1 THEN
Write ('movl $')
ELSIF size = 1 THEN
Write ('movb $')
END;
Writeint (a); Write (', ('); Write(bname); Writeln (')');
END MovConstToAdr;

PROCEDURE MovAdrToReg (a, b, size : LONGINT);
VAR aname, bname : OSAS.Ident;
BEGIN
ConvertRegNumToName (a, aname);
IF size > 1 THEN
Write ('movl ')
ELSIF size = 1 THEN
Write ('movsbl ') (* here was movb *)
END;
Write ('('); Write(aname); Write('), ');
(*IF size = 1 THEN
Convert8RegNumToName (b, bname)
ELSE*)
ConvertRegNumToName (b, bname);
(*END;*)
Writeln (bname)
END MovAdrToReg;

PROCEDURE lea(VAR x : Item; a : LONGINT);
VAR aname : OSAS.Ident;
BEGIN
ConvertRegNumToName (a, aname);
Write ('leal '); Write (x.label); Write (', '); Writeln (aname)
END lea;

PROCEDURE CmpConstWithReg (a, r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN
ConvertRegNumToName (r, rname);
(*Write ('cmp ('); Write (xname); Write (', '); Writeint (y.a); Writeln (');');*) (*hla syntax *)
Write ('cmpl $'); Writeint (a); Write (", "); Writeln (rname);     (*gas syntax *)
END CmpConstWithReg;
(*
PROCEDURE CmpVarWithReg (a, r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN
ConvertRegNumToName (r, rname);
(*Write ('cmp ('); Write (xname); Write (', '); Writeint (y.a); Writeln (');');*) (*hla syntax *)
Write ('cmpl $'); Writeint (a); Writeln (xname);     (*gas syntax *)
END CmpVarWithReg;
*)

PROCEDURE CmpInRegs (a, b : LONGINT);
VAR aname, bname : OSAS.Ident;
BEGIN
ConvertRegNumToName (a, aname);
ConvertRegNumToName (b, bname);
Write ('cmpl '); Write(aname); Write (', '); Writeln (bname)
END CmpInRegs;

PROCEDURE setc (r : LONGINT);
VAR r0 : OSAS.Ident;
BEGIN
Convert8RegNumToName (r, r0);
Write ('setc ');
Writeln (r0);
END setc;

PROCEDURE setnc (r : LONGINT);
VAR r0 : OSAS.Ident;
BEGIN
Convert8RegNumToName (r, r0);
Write ('setnc ');
Writeln (r0);
END setnc;

PROCEDURE sete (r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN
Convert8RegNumToName (r, rname);
Write ('sete '); Writeln (rname)
END sete;

PROCEDURE setne (r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN
Convert8RegNumToName (r, rname);
Write ('setne '); Writeln (rname)
END setne;

PROCEDURE setl (r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN
Convert8RegNumToName (r, rname);
Write ('setl '); Writeln (rname)
END setl;

PROCEDURE setle (r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN
Convert8RegNumToName (r, rname);
Write ('setle '); Writeln (rname)
END setle;

PROCEDURE setg (r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN
Convert8RegNumToName (r, rname);
Write ('setg '); Writeln (rname)
END setg;

PROCEDURE setge (r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN
Convert8RegNumToName (r, rname);
Write ('setge '); Writeln (rname)
END setge;

PROCEDURE Push(l : LONGINT);
VAR lname : OSAS.Ident;
BEGIN
ConvertRegNumToName (l, lname);
(*Write ('push ('); Write (lname); Writeln (');');*) (* hla syntax *)
Write ('pushl '); Write (lname); Writeln (';');  (* gas syntax *)
END Push;

PROCEDURE Push8(l : LONGINT);
VAR lname : OSAS.Ident;
BEGIN
ConvertRegNumToName (l, lname);
(*Write ('push ('); Write (lname); Writeln (');');*) (* hla syntax *)
Write ('push '); Write (lname); Writeln (';');  (* gas syntax *)
END Push8;

PROCEDURE PushConst8 (VAR a : LONGINT);
BEGIN
Write ('push $'); Writeint (a); Writeln ('');
END PushConst8;

PROCEDURE PushConst (VAR a : LONGINT);
BEGIN
Write ('pushl $'); Writeint (a); Writeln ('');
END PushConst;

PROCEDURE PushMem8 (VAR a : Item);
BEGIN
Write ('push '); Writeln (a.label)
END PushMem8;

PROCEDURE PushMem (VAR a : Item);
BEGIN
Write ('pushl '); Writeln (a.label)
END PushMem;

PROCEDURE Pop(l : LONGINT);
VAR lname : OSAS.Ident;
BEGIN
ConvertRegNumToName (l, lname);
(*Write ('pop ('); Write (lname); Writeln (');');*) (* hla syntax *)
Write ('popl '); Write (lname); Writeln (';');  (* gas syntax *)
END Pop;

PROCEDURE Pop8(l : LONGINT);
VAR lname : OSAS.Ident;
BEGIN
ConvertRegNumToName (l, lname);
(*Write ('pop ('); Write (lname); Writeln (');');*) (* hla syntax *)
Write ('popb '); Write (lname); Writeln (';');  (* gas syntax *)
END Pop8;

PROCEDURE or(x, y : LONGINT);
VAR xname, yname : OSAS.Ident;
BEGIN
Write ('or '); 
ConvertRegNumToName (x, xname);
ConvertRegNumToName (y, yname);
Write (xname); Write (', '); Writeln (yname);
END or;

PROCEDURE and(x, y : LONGINT);
VAR xname, yname : OSAS.Ident;
BEGIN
Write ('and '); 
ConvertRegNumToName (x, xname);
ConvertRegNumToName (y, yname);
Write (xname); Write (', '); Writeln (yname);
END and;

PROCEDURE xor (x, y : LONGINT);
VAR xname, yname : OSAS.Ident;
BEGIN
Write ('xor '); 
ConvertRegNumToName (x, xname);
ConvertRegNumToName (y, yname);
Write (xname); Write (', '); Writeln (yname);
END xor;

PROCEDURE ormemreg(VAR y : Item; r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN
Write ('or '); 
ConvertRegNumToName (r, rname);
Write (y.label); Write (', '); Writeln (rname);
END ormemreg;

PROCEDURE andmemreg(VAR y : Item; r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN
Write ('and '); 
ConvertRegNumToName (r, rname);
Write (y.label); Write (', '); Writeln (rname);
END andmemreg;

PROCEDURE xormemreg(VAR y : Item; r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN
Write ('or '); 
ConvertRegNumToName (r, rname);
Write (y.label); Write (', '); Writeln (rname);
END xormemreg;

PROCEDURE orconstreg(a, r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN
Write ('or $'); 
ConvertRegNumToName (r, rname);
Writeint (a); Write (', '); Writeln (rname);
END orconstreg;

PROCEDURE andconstreg(a, r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN
Write ('and $'); 
ConvertRegNumToName (r, rname);
Writeint (a); Write (', '); Writeln (rname);
END andconstreg;

PROCEDURE xorconstreg(a, r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN
Write ('xor $'); 
ConvertRegNumToName (r, rname);
Writeint (a); Write (', '); Writeln (rname);
END xorconstreg;

PROCEDURE not (y : LONGINT);
VAR yname : OSAS.Ident;
BEGIN
Write ('not '); 
ConvertRegNumToName (y, yname);
Writeln (yname);
END not;

PROCEDURE jmp (a : OSAS.Ident);
BEGIN
Write ('jmp '); Writeln (a)
END jmp;

PROCEDURE jnz (a : OSAS.Ident);
BEGIN
Write ('jnz '); Writeln (a)
END jnz;

PROCEDURE je (a : OSAS.Ident);
BEGIN
Write ('je '); Writeln (a);
END je;

PROCEDURE jne (a : OSAS.Ident);
BEGIN
Write ('jne '); Writeln (a);
END jne;

PROCEDURE jl (a : OSAS.Ident);
BEGIN
Write ('jl '); Writeln (a);
END jl;

PROCEDURE jle (a : OSAS.Ident);
BEGIN
Write ('jle '); Writeln (a);
END jle;

PROCEDURE jg (a : OSAS.Ident);
BEGIN
Write ('jg '); Writeln (a);
END jg;

PROCEDURE jge (a : OSAS.Ident);
BEGIN
Write ('jge '); Writeln (a);
END jge;

PROCEDURE AddConstToReg (a, r : LONGINT); (* r := r + a *)
VAR rname : OSAS.Ident;
BEGIN
ConvertRegNumToName (r, rname);
Write ('addl $'); Writeint (a); Write (', '); Writeln (rname);
END AddConstToReg;


PROCEDURE AddRegs (x, y : LONGINT); (* y.r := y.r + x.mem *)
VAR rname, xname, yname : OSAS.Ident;
BEGIN
ConvertRegNumToName (x, xname);
ConvertRegNumToName (y, yname);

Write ("addl ");Write (xname); Write(","); Writeln (yname);          (* gas syntax *)
(*Write ("add (");Write (yname); Write(","); Write (xname); Writeln (');');*)   (* hla syntax *)
END AddRegs;

PROCEDURE AddMemToReg (x : Item; r : LONGINT); (* r := r + x.mem *)
VAR rrr : OSAS.Ident;
BEGIN
ConvertRegNumToName (r, rrr);
Write ("addl "); Write (x.label); Write (", "); Writeln (rrr);
END AddMemToReg;

PROCEDURE MulRegs (x, y : LONGINT); (* y.reg := y.reg * x.reg *)
VAR rname, xname, yname : OSAS.Ident;
BEGIN
ConvertRegNumToName (x, xname);
ConvertRegNumToName (y, yname);
Write ("imull "); Write (xname); Write (", "); Writeln (yname);
END MulRegs;

PROCEDURE MulMemToReg (a: Item; r : LONGINT); (* r := r * a.mem *)
VAR rname : OSAS.Ident;
BEGIN
ConvertRegNumToName (r, rname);
(*Write ("intmul ("); Write (y.label); Write (", "); Write (xname); Writeln (');');*) (* hla syntax*)
Write ('imull '); Write (a.label); Write (', '); Write (rname); Writeln (';'); (* gas syntax *)
END MulMemToReg;

PROCEDURE MulConstWithReg (a, r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN

ConvertRegNumToName (r, rname);
Write ("imull $"); Writeint (a); Write (", "); Writeln (rname)
END MulConstWithReg;

PROCEDURE SubRegWithConst (a, r : LONGINT); (* r := r - a*)
VAR q : OSAS.Ident;
BEGIN
ConvertRegNumToName (r, q);
Write ('subl $'); Writeint(a); Write(', '); Writeln (q);
END SubRegWithConst;

PROCEDURE SubRegs (x, y : LONGINT); (* x.reg := x.reg - y.reg *)
VAR rname, xname, yname : OSAS.Ident;
BEGIN
(*ConvertRegNumToName (r, rname);*)
ConvertRegNumToName (x, xname);
ConvertRegNumToName (y, yname);
(*Write ("sub (");Write (yname); Write(","); Write (xname); Writeln (');');*) (* hla syntax *)
Write ("subl ");Write (xname); Write(","); Writeln (yname);          (* gas syntax *)
END SubRegs;

PROCEDURE SubRegWithMem (r : LONGINT; x : Item); (* x.reg := x.reg - y.mem *)
VAR rname : OSAS.Ident;
BEGIN
ConvertRegNumToName (r, rname);
(*Write ("sub ("); Write (y.label); Write (", "); Write (xname); Writeln (");"); *)  (* hla syntax *)
Write ("subl "); Write (x.label); Write (", "); Write (rname); Writeln (";");            (* gas syntax *)
END SubRegWithMem;

PROCEDURE LoadConstToReg (l, r : LONGINT);
VAR name : OSAS.Ident;
BEGIN
ConvertRegNumToName (r, name);
Write ("movl $"); Writeint (l); Write (", "); Writeln (name)
END LoadConstToReg;

PROCEDURE LoadReg (VAR varname : Item; regnum : LONGINT);
VAR name : OSAS.Ident;
BEGIN
ConvertRegNumToName (regnum, name);
IF varname.type.size > 1 THEN
(*Write ("mov (");*) (* hla syntax *)
Write ("movl ");         (* gas syntax *)
ELSIF varname.type.size = 1 THEN
(*Write ("movsbl ");*)
(*Write ("movsx (");*) (* hla syntax*)
Write ("movsbl ");            (*          gas syntax *)
END;
IF varname.type.form = OSAB.Proc THEN                  (* TRANSLATE*)
Write ('&');
END;
(*Write (varname.label); Write (", "); Write (name); Writeln (");");*) (* hla syntax *)
Write (varname.label); Write (", "); Write (name); Writeln (";");                              (* gas syntax*)
END LoadReg;
(*
PROCEDURE LoadParamToReg (VAR p : Item; r : LONGINT);
VAR rname : OSAS.Ident;
BEGIN (* here we assume that p.label already contains location of variable in stack like 8(%ebp) *)
MovVarToReg(p, r);
ConvertRegNumToName (r, rname);
IF p.type.size = 1 THEN
 Write ("movb ")
ELSIF p.type.size >= 4 THEN
 Write ("movl ")
END;
Write ("("); Write(rname); Write ("), ");
IF p.type.size = 1 THEN
 Convert8RegNumToName (r, rname)
END;
Writeln(rname)
END LoadParamToReg;
*)
PROCEDURE LoadVarParamToReg (fp, offset, r : LONGINT; VAR x : Item);
VAR fpname, rname : OSAS.Ident;
BEGIN
ConvertRegNumToName (fp, fpname);
(*   IF x.type.size = 1 THEN
Convert8RegNumToName (r, rname);
MovConstToReg (0, r);
Write ('movb ');
ELSIF x.type.size >= 4 THEN*)
ConvertRegNumToName (r, rname);
Write ('movl ');
(*END;*)
Writeint (-offset);
Write('('); Write (fpname); Write('), ');
Writeln (rname);
(*
IF x.type.form = OSAB.Array THEN
AddConstToReg (x.
END;
*)

IF x.type.size = 1 THEN
Write ("movsbl ") (* movb *)
ELSIF x.type.size >=4 THEN
Write ("movl ");
END;
Write("("); Write(rname); Write ("),"); 
IF x.type.size = 1 THEN
ConvertRegNumToName(r, rname)
(*Convert8RegNumToName(r, rname)*)
END;
Writeln (rname);
END LoadVarParamToReg;
(*
PROCEDURE LoadParamToReg (fp, offset, r : LONGINT; VAR x : Item);
VAR fpname, rname : OSAS.Ident;
BEGIN
ConvertRegNumToName (fp, fpname);
(*IF x.type.size = 1 THEN
Convert8RegNumToName (r, rname);
MovConstToReg (0, r);
Write ('movb ');
ELSIF x.type.size >= 4 THEN*)
ConvertRegNumToName (r, rname);
Write ('movl ');
(*END;*)
Writeint (-offset);
Write('('); Write (fpname); Write('), ');
Writeln (rname);
END LoadParamToReg;
*)
PROCEDURE LoadParamToReg (fp, offset, r : LONGINT; VAR x : Item);
VAR fpname, rname : OSAS.Ident;
BEGIN
ConvertRegNumToName (fp, fpname);
IF x.type.size = 1 THEN
ConvertRegNumToName (r, rname);
(*Convert8RegNumToName (r, rname);*)
Write ('movsbl '); (* movb *)
ELSIF x.type.size >= 4 THEN
ConvertRegNumToName (r, rname);
Write ('movl ');
END;
Writeint (-offset);
Write('('); Write (fpname); Write('), ');
(*IF x.type.size = 1 THEN
Convert8RegNumToName (r, rname);
END;*)
Writeln (rname);
END LoadParamToReg;

PROCEDURE LoadParamAdr (fp, offset, r : LONGINT; VAR x : Item);
VAR fpname, rname : OSAS.Ident;
BEGIN
ConvertRegNumToName (fp, fpname);
ConvertRegNumToName (r, rname);
Write ('movl ');
Writeint (-offset);
Write('('); Write (fpname); Write('), ');
Writeln (rname);
END LoadParamAdr;

PROCEDURE negreg(r : LONGINT);
VAR name : OSAS.Ident;
BEGIN
ConvertRegNumToName (r, name);
(*Write ("neg ("); Write (name); Writeln (");");*) (* hla syntax *)
Write ("negl "); Write (name); Writeln (";");                        (* gas syntax *)
END negreg;

PROCEDURE ShiftL (a, r : LONGINT);
VAR rr : OSAS.Ident;
BEGIN
ConvertRegNumToName (r, rr);
Write ('shll $'); Writeint (a); Write (', '); Writeln (rr)
END ShiftL;

PROCEDURE BitTest (a, r : LONGINT);
VAR rr : OSAS.Ident;
BEGIN
ConvertRegNumToName (r, rr);
Write ("bt $"); Writeint(a); Write (", "); Writeln (rr); (* gas syntax *)
(*Write ("bt ("); Writeint(x.a); Write (", "); Write (yname); Writeln (');');*) (* hla syntax *)
END BitTest;

PROCEDURE BitTestRegs (x, y : LONGINT);
VAR r0, r1 : OSAS.Ident;
BEGIN
ConvertRegNumToName (x, r0);
ConvertRegNumToName (y, r1);
Write ('bt '); Write (r0); Write (', '); Writeln (r1); 
END BitTestRegs;

PROCEDURE Test (r0, r1 : LONGINT);
VAR r0name, r1name : OSAS.Ident;
BEGIN
Convert8RegNumToName (r0, r0name);
Convert8RegNumToName (r1, r1name);
Write ('test '); Write (r0name); Write(', '); Writeln (r1name)
END Test;

PROCEDURE DecReg (a : LONGINT);
VAR r : OSAS.Ident;
BEGIN
ConvertRegNumToName (a, r);
Write ('decl '); Writeln (r);
END DecReg;

PROCEDURE IncReg(a : LONGINT);
VAR r : OSAS.Ident;
BEGIN
ConvertRegNumToName (a, r);
Write ("incl "); Writeln (r);
END IncReg;

PROCEDURE IncVar(a : Item);
BEGIN
Write ('incl '); Writeln (a.label);
END IncVar;

PROCEDURE DecVar(a : Item);
BEGIN
Write ('decl '); Writeln (a.label);
END DecVar;

(* os dependent *)
PROCEDURE PutStart;
BEGIN
Writeln (".section .text");
Writeln (".globl _start");
Writeln ("_start:");
END PutStart;

PROCEDURE PutExit(i : INTEGER);
BEGIN
jmp ('OBERON_NOERROR_');
Writeln (".globl OBERON_ERRORCODE_");
Writeln ("OBERON_ERRORCODE_:");
MovConstToReg(-1, R1);
jmp ('OBERON_MAIN_EXIT_');
Writeln ('OBERON_NOERROR_:');
MovConstToReg (i, R1);       (* error code number *)
Writeln ('OBERON_MAIN_EXIT_:');
MovConstToReg (1, R0);     (* linux system call to exit *)
Writeln ("int $0x80");        (* kernel interrupt *)
END PutExit;

PROCEDURE PutInit;
BEGIN
Writeln (".section .text");
Write (".globl "); Write (modname); Writeln ("_init_");

Write (modname);
Writeln ("_init_:");
END PutInit;

PROCEDURE Ret;
BEGIN
Writeln ('ret');
END Ret;

PROCEDURE ArrayCheck(i, l : LONGINT);
VAR iname, lname : OSAS.Ident;
BEGIN
(*movl (i, ESI);
movl (l, EDI);*)
CmpInRegs (i, l);
jg ("OBERON_ARRAY_FAULT_");

END ArrayCheck;

PROCEDURE PutArrayCheck;
BEGIN
Writeln ("OBERON_ARRAY_FAULT_:");
Writeln (".section .data");
Writeln ('OBERON_ARRAY_FAULT_MSG_: .ascii "out of array index bounds\n"');
Writeln (".section .text");
MovConstToReg (4, EAX);
MovConstToReg (1, EBX);
Writeln ("leal OBERON_ARRAY_FAULT_MSG_, %ecx");
MovConstToReg(26, EDX);
Writeln ("int $0x80");
Writeln ("jmp OBERON_ERRORCODE_");
END PutArrayCheck;

PROCEDURE PutMemallo(size : LONGINT);
BEGIN
Writeln ('.section .text');
Write ('movl $'); Writeint(size); Writeln (', mmap_+4');
Writeln ('movl $90, %eax        # mmap');
Writeln ('leal mmap_, %ebx');
Writeln ('int $0x80');
Writeln ('#now pointer to new location is in %eax');
END PutMemallo;

PROCEDURE PutMemMan;
BEGIN
Writeln ('.section .data');
Writeln ('.comm mmap_, 24');
Writeln ('.section .text');
Writeln ('movl $0, mmap_');
Writeln ('#movl $65535, mmap_+4  # length of requested memory');
Writeln ('movl $3, mmap_+8      # read, write, PROT_WRITE | PROT_READ, 0x02, 0x01');
Writeln ('movl $34, mmap_+12  # map anonymously (0x20), map_private (0x2)');
Writeln ('movl $-1, mmap_+16   # fd, -1 for portability');
Writeln ('movl $0, mmap_+20    # offset is ignored');
END PutMemMan;

PROCEDURE PutLabel(s : OSAS.Ident);
BEGIN
Write(s); Writeln(":")
END PutLabel;

(*register management*)

PROCEDURE GetReg(VAR r: LONGINT);
VAR u: LONGINT;
BEGIN
u := RH;
WHILE u IN regs DO DEC(u) END ;
IF u >= RL THEN INCL(regs, u) ELSE OSAS.Mark("too few registers") END ;
r := u
END GetReg;

PROCEDURE Release(r: LONGINT);
BEGIN
IF (r >= RL) & (r <= RH) THEN EXCL(regs, r); (*Out.String ("Releasing "); Out.Int (r, 0); Out.Ln*)  END
END Release;

PROCEDURE GetReg1(VAR r: LONGINT; u: LONGINT);
BEGIN
Release(u); GetReg(r)
END GetReg1;

PROCEDURE GetReg2(VAR r: LONGINT; u, v: LONGINT);
BEGIN 
Release(v); Release(u); GetReg(r)
END GetReg2;

PROCEDURE Inv(op : INTEGER): INTEGER;
BEGIN

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
CmpConstWithReg (y.a, x.r);
IF x.not THEN t := Inv(op) ELSE t := op END;
 CASE t OF 
    9: (* = *)   sete(x.r)          (*x.r=0*)
|  10: (* # *)   setne(x.r)          (*x.r=1*)
|  11: (* < *)   setl(x.r)           (*x.r=2*)
|  12: (* <= *)  setle(x.r)          (*x.r=3*)
|  13: (* > *)   setg(x.r)          (*x.r=4*)
|  14: (* >= *)  setge(x.r)          (*x.r=5*)
END;
END CompareInRegAndConst;
(*
PROCEDURE CompareInRegAndMem (VAR x: Item; ylab : OSAS.Ident); (* not used, REMOVE??? *)
VAR xname : OSAS.Ident;
BEGIN
ConvertRegNumToName (x.r, xname);
Write ("cmpl "); Write (ylab); Write (', '); Writeln (xname); (*gas syntax*)
END CompareInRegAndMem;
*)
PROCEDURE CompareInRegs(op : INTEGER; VAR x, y : Item);
VAR xname, yname : OSAS.Ident; t : INTEGER;
BEGIN
CmpInRegs (x.r, y.r);

IF x.not THEN t := Inv(op) ELSE t := op END;
  CASE t OF
(*0: (* = *) Write (" sete ("); Out.String ("x.r=0"); Out.Ln;
|  1: (* # *) Write (" setne ("); Out.String ("x.r=1"); Out.Ln;
|  2: (* < *) Write (" setl ("); Out.String ("x.r=2"); Out.Ln;
|  3: (* <= *) Write (" setle ("); Out.String ("x.r=3"); Out.Ln;
|  4: (* > *) Write (" setg ("); Out.String ("x.r=4"); Out.Ln;
|  5: (* >= *)Write (" setge ("); Out.String ("x.r=5"); Out.Ln;

| *) 
9: (* = *) sete (y.r)
|  10: (* # *) setne (y.r)
|  11: (* < *) setl (y.r)
|  12: (* <= *) setle (y.r)
|  13: (* > *) setg (y.r)
|  14: (* >= *) setge (y.r)
ELSE Out.String ('op is '); Out.Int(t, 0); Out.String ('?'); Out.Ln; HALT(0); (* REMOVE?*)
END;
Release (y.r); Release (x.r);
END CompareInRegs;

PROCEDURE SetOpInRegs (op : LONGINT; x, y : Item);
VAR xname, yname : OSAS.Ident;
BEGIN

(*IF op = OSAS.plus THEN op0 := 24 (*or*) ELSIF op = OSAS.minus THEN op0 := 28 (*bic*)
ELSIF op = OSAS.times THEN op0 := 0 (*and*) ELSIF op = OSAS.rdiv THEN op0 := 2 (*xor*)
ELSE OSAS.Mark("not a set operator"); op0 := 0
END ;*)

IF op = (*24*) 6 THEN (* + operation or *)
or (y.r, x.r);
(*ELSIF op = (*28*) 7 THEN (*Out.String ("- operation and"); Out.Ln;*)
Write ("not (") (*and will follow*)*)
ELSIF op = (*0*) 1 THEN (* * operation and *)
and (y.r, x.r)
ELSIF op = 2 THEN (* / operation xor *)
xor (y.r, x.r);
END;
END SetOpInRegs;

PROCEDURE SetMinus (VAR x, y : Item);
VAR xname, yname : OSAS.Ident;
BEGIN
not (y.r);
and (y.r, x.r);
END SetMinus;

PROCEDURE SetOpInRegAndMem (op : LONGINT; x, y: Item);
VAR xname : OSAS.Ident;
BEGIN
IF op = (*24*) 6  THEN (* + operation or *)
ormemreg (y, x.r);
  (*ELSIF op = (*28*) 7 THEN (*Out.String ("- operation and"); Out.Ln;*)
  Write ("not (") (*and will follow *)*)
ELSIF op = (*0*) 1 THEN (* * operation and *)
andmemreg (y, x.r);
ELSIF op = 2 THEN (* / operation xor *)
xormemreg (y, x.r)
END;
END SetOpInRegAndMem;

PROCEDURE  SetOpConstAndReg (op : LONGINT; x : Item; n : LONGINT);
VAR xname : OSAS.Ident;
BEGIN
IF op = (*24*) 6  THEN         (* + operation or *)
orconstreg (n, x.r)
      (*ELSIF op = (*28*) 7 THEN Out.String ("- operation and"); Out.Ln;
      Write ("not (") (*and will follow *)*)
ELSIF op = (*0*) 1 THEN        (* operation and *)
andconstreg (n, x.r)
ELSIF op = 2 THEN              (* / operation xor *)
xorconstreg (n, x.r)
END;
END SetOpConstAndReg;

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

PROCEDURE Put1a(op, dst, src, imm, rot: LONGINT);  (*register operation with immediate*)
BEGIN
(*
code[pc] := ((((0E20H + op)*10H + src)*10H + dst)*10H + rot)*100H + imm; INC(pc)
*)
END Put1a;

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

PROCEDURE merged(L0, L1: LONGINT): LONGINT;                   (* REMOVE??? *)
VAR L2, L3: LONGINT;
BEGIN 
(*
IF L0 # 0 THEN L3 := L0;
REPEAT L2 := L3; L3 := code[L2] MOD C24 UNTIL L3 = 0;
code[L2] := code[L2] - L3 + L1; L1 := L0
END ;*)
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
VAR r, cd, n, s, k: LONGINT; rname : OSAS.Ident;
BEGIN (*Out.String ("entered load"); Out.Ln;*)
IF (x.mode # OSAB.Reg) (*& (x.mode # OSAB.RegI)*) THEN
IF x.type.size = 1 THEN cd := LDR+4 ELSE cd := LDR END ;   (* REMOVE*)
(*IF x.r = FP THEN
    GetReg(r);
    k := x.a - 4;
    
    MovStackToReg (FP, k, r, x);*)
(*ELS*)
IF x.mode = OSAB.Var THEN
 IF x.b >= 0 THEN 
    GetReg1(r, x.r); 
       IF x.r = EBP THEN (* local variable or parameter *)
	  IF x.a < 0 THEN (* parameter *)
	     k := x.a - 4; (* mov 8(%ebp), %edx *)
	     LoadParamToReg(EBP, k, r, x)
	  ELSIF x.a > 0 THEN
	     k := x.a;       (* mov -4(%ebp), %edx *)  (* local variable*)
		(*LoadParamToReg(EBP, k, r, x)*) (* eto rabotaet , no ne dlya array *)
	     MovVarToReg(x, r);
	  END;
       ELSE
	  LoadReg(x, r);             (* movl x.label, r *)
       END
 ELSE 
    GetReg(r); 
    ExtRef(x, r, 0); 
    LoadReg (x, r);
 END
ELSIF x.mode = OSAB.Par THEN 
    GetReg(r);
    k := x.a -4;
    (*IF x.type.form IN {OSAB.Array, OSAB.String} THEN*)
    IF x.type.form # OSAB.Pointer THEN
       LoadVarParamToReg(EBP, k, r, x);
    ELSE
       LoadParamToReg(EBP, k, r, x)
    END;
    (*ELSIF x.type.form IN {OSAB.Record} THEN
       
    ELSIF x.type.form IN {OSAB.Char, OSAB.Int, OSAB.Bool} THEN

    END*)
ELSIF x.mode = OSAB.RegI THEN
     IF x.type.size = 1 THEN
       MovAdrToReg(x.r, x.r, 1);
    ELSE
       MovAdrToReg(x.r, x.r, 4)
    END;
    r := x.r;
    x.mode := OSAB.Reg;
(*GetReg1(r, x.r);
 k := x.a - 4;
 LoadVarParamToReg(EBP, k, r, x);*)
 (*Put2(cd, r, x.r, x.a)*)
ELSIF x.mode = RegX THEN
 GetReg2(r, x.r, x.a);
 (*Put3(cd+8, r, x.r, x.a, x.b)*)
ELSIF x.mode = OSAB.Const THEN
 IF x.type.form <= OSAB.Set THEN
    GetReg(r); n := x.a;            
    (*IF x.r = EBP THEN
       IF x.type.size = 1 THEN
	  Mov8ConstToReg(x.a, r)
       ELSE
	  MovConstToReg(x.a, r)
       END*)
    (*ELSE*)
       IF n = 0 THEN
	  LoadConstToReg (0, r);
       ELSE 
	  LoadConstToReg (x.a, r)         (* zdes dobavit if y.r = 6 to esli y.type.size=1 to movb const to reg *)
       END;
    (*END*)
 ELSIF x.type.form = OSAB.Proc THEN
    GetReg(r);
    IF x.b >= 0 THEN
       LoadReg(x, r); 
    ELSE 
       ExtRef(x, r, 100H);
    END ;
 ELSIF x.type.form = OSAB.NilTyp THEN 
    GetReg(r); (*Put1(MOV, r, 0, 0)*) MovConstToReg(0, r);
 END
ELSIF x.mode = CC THEN 
   (*IF ~(EAX IN regs) THEN
      regs := regs + {EAX};
      x.r := EAX;
      x.mode := OSAB.Reg
   ELSE
      OSAS.Mark ('too few registers')
   END;*)
   GetReg(r);
   Out.String ("x.a=");Out.Int(x.a, 0); Out.Ln;
   Out.String ("x.b="); Out.Int (x.b, 0); Out.Ln;
 IF (x.a = 0) & (x.b = 0) THEN
 ELSIF (x.a = 0) & (x.b = 1) THEN (* boolean? *)
 (*Convert8RegNumToName(r, rname);*)
 (*Write ('cmpb $0, '); Writeln (rname);
 Write ('sete '); Writeln (rname);*)


 (*LoadReg (x, r);*)
    (*Put1((x.r+2)*100H + MOV, r, 0, 1); Put1((negated(x.r)+2)*100H + MOV, r, 0, 0)*)
 ELSE (*Put5(x.r, 1); FixLink(x.a); Put1(MOV, r, 0, 0); Put5(14, 0); FixLink(x.b); Put1(MOV, r, 0, 1)*)
 END
ELSE OSAS.Mark("illegal object (type?)");
END ;
x.mode := OSAB.Reg; x.r := r;
END
END load;

PROCEDURE loadAdr(VAR x: Item);
VAR n, r, s, temp: LONGINT;
BEGIN
GetReg(r);
lea (x, r);
x.r := r;
(*
IF (x.mode = OSAB.Var) & (x.b >= 0) OR (x.mode = OSAB.RegI) THEN
IF x.r = PC THEN 
x.a := x.a -  LONG(pc)*4 - 8   (*global*) END ;
GetReg1(r, x.r); n := x.a;
IF n < 0 THEN
 n := -n; 
 ScaleConst(n, s);
 Put1a(SUB, r, x.r, n MOD C8, s MOD 10H);
 IF n >= C8 THEN
    n := n DIV C8;
    Put1a(SUB, r, r, n MOD C8, (s-4) MOD 10H);
    IF n >= C8 THEN
       n := n DIV C8;
       Put1a(SUB, r, r, n MOD C8, (s-8) MOD 10H);
       IF n >= C8 THEN 
	  OSAS.Mark("adr offset too large") 
       END
    END
  END
ELSIF n > 0 THEN
  ScaleConst(n, s);
  Put1a(ADD, r, x.r, n MOD C8, s MOD 10H);
  IF n >= C8 THEN
      n := n DIV C8;
      Put1a(ADD, r, r, n MOD C8, (s-4) MOD 10H);
      IF n >= C8 THEN 
	 OSAS.Mark("field offset too large") 
      END
  END ;
ELSE 
  r := x.r
END
ELSIF x.mode = OSAB.Var THEN 
GetReg(r); 
ExtRef(x, r, 0)
ELSIF x.mode = OSAB.Par THEN 
GetReg1(r, x.r); 
Put2(LDR, r, x.r, x.a);
IF x.b # 0 THEN
n := x.b; 
ScaleConst(n, s); 
Put1a(ADD, r, r, n MOD C8, s MOD 10H);
IF n >= C8 THEN 
 OSAS.Mark("field offset too large") 
END
END
ELSIF x.mode = RegX THEN 
GetReg2(r, x.r, x.a); 
Put0a(ADD, r, x.r, x.a, 0, x.b)
ELSIF x.mode = OSAB.Const THEN
OSAS.Mark("variable expected")
ELSE 
OSAS.Mark("bad mode in loadAdr")
END ;
x.mode := OSAB.RegI; x.r := r
*)
END loadAdr;

PROCEDURE loadCC(VAR x: Item);
BEGIN
IF x.type.form = OSAB.Bool THEN 
IF x.mode = OSAB.Const THEN 
load(x);
SetCC(x, (*1-x.a*)labelnum-1);
Release(x.r);
ELSE
load(x); 
Release(x.r);
SetCC(x, (*1*)labelnum); 
END
ELSE 
OSAS.Mark("must be Boolean"); 
x.a := 0; x.b := 0
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
VAR r, xL, k : LONGINT; num, string : OSAS.Ident;
BEGIN
(*
xL := x.a DIV 10000H; x.a := x.a MOD 10000H;
IF x.mode = OSAB.Const THEN
GetReg(r); x.mode := OSAB.Reg; x.r := r; enterStr(x.a); Put1(ADD, r, PC, 0)
ELSE loadAdr(x); x.b := xL
END
*)
IF x.mode = OSAB.Const THEN
 string := x.label;
 Strings.Append (0X, string);
 Writeln ('.section .rodata');
 Writeln ('.globl');
 COPY (modname, x.label);
 Strings.Append ('_', x.label);
 INC(labelstr);
 IntStr.IntToStr(labelstr, num);
 Strings.Append (labelstring, x.label);
 Strings.Append (num, x.label);
 Write (x.label); Write (': .ascii "');
 Write (string); Write('\0'); Writeln ('"');
 Writeln ('.section .text');
 k := 0;
 WHILE string[k] # 0X DO INC(k) END;
 x.type.len := k;
END;
GetReg(r);
x.r := r;
x.mode := OSAB.Reg;
lea (x, r);
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
COPY('', label);
(*Strings.Append ('.', label);*)
Strings.Append (modname, label);
Strings.Append ('_', label);
Strings.Append (o.name, label);
Writeln (".section .rodata");
IF o.expo THEN
Write (".globl "); Writeln (label)
END;
Write (label); COPY(label, o.label);
Write (": "); Write ('.ascii ');
Write ('"');
(*tmpstr := OSAS.name;*)
(*OSAS.CopyId(tmpstr);*)
Write (OSAS.name);
Writeln ('\0"');
o.type.len := OSAS.slen+1;
END AllocString;

PROCEDURE AllocVariablesInit*(gl : BOOLEAN);
BEGIN
(*IF gl THEN
Writeln ("static");                           (* hla syntax *)
Writelnh ('static');
ELSE
(*Writeln ('var')*)
Writeln ('static')
END*)

Writeln ('.section .bss');                                            (* gas syntax *)
END AllocVariablesInit;    

PROCEDURE AllocVariable*(VAR o : OSAB.Object);
VAR label: OSAS.Ident;ch : CHAR; s : LONGINT;
tmpobj, tmpobj2 : OSAB.Object; b : BOOLEAN;
BEGIN
IF o.lev = 0 THEN
COPY ('', label);  
(*       IF o.expo THEN
  Strings.Append (modname, label);
  Write ("       .comm   ");
ELSE
  Write ("       .lcomm   ");
END;*)

(*Out.Ln; Out.Ln; Out.Ln; Out.Ln;
DebugObj(o); Out.Ln; Out.Ln; in.Char(ch); DebugObj(o.type.dsc); in.Char(ch);
Out.Ln; DebugObj(o.type.dsc.next); in.Char(ch); Out.Ln;
DebugObj(o.type.dsc.next.next); in.Char(ch); Out.Ln;
DebugObj(o.type.dsc.next.next.next); in.Char(ch); Out.Ln; (*this is dereference of nil because no record fields *)   *)
Writeln ('.section .bss');   
IF o.expo THEN 
Write ('.comm  ')
ELSE
Write ('.lcomm ')
END;

Strings.Append (modname, label);
Strings.Append ("_", label);
Strings.Append (o.name, label);
Write (label); COPY (label, o.label);

Write (', '); 
s := o.type.size;
IF o.type.form = OSAB.Array THEN
s := o.type.size MOD 4;
IF s > 0 THEN 
  s := o.type.size + 4 - s;
END
END;
Writeint (s(*o.type.size*));  Writeln (';');
END
END AllocVariable;

PROCEDURE Q(T: OSAB.Type);
VAR obj: OSAB.Object;
BEGIN
IF T.base # NIL THEN
Q(T.base); 
obj := T.typobj;
(*PutC(obj.val * 10000H + fixlist[-obj.lev]); fixlist[-obj.lev] := pc-1*);
Write (', ');
Write (modname); Write ('_'); Write(obj.name); Write('_'); Write('typedsc'); Write ('_');
END
END Q;

PROCEDURE AllocTD*(obj: OSAB.Object);
VAR n: INTEGER; tp: OSAB.Type;
BEGIN
(*DebugObj(obj);
Out.String ("obj.type.dsc"); Out.Ln;
DebugObj(obj.type.dsc);*)
obj.lev := 0; tp := obj.type;
IF tp.form = OSAB.Pointer THEN tp := tp.base END;
Writeln ('.section .data');
Write (modname); Write ('_'); Write(obj.name); Write('_'); Write('typedsc'); Writeln ('_:');
Write ('.long ');
Writeint (tp.size+4); (* tp.size is without td*)
Q(tp); n := tp.nofpar; (*Out.String ('n = '); Out.Int(n,0); Out.Ln;*)
(*
obj.lev := 0; obj.val := pc*4; tp := obj.type;
IF tp.form = OSAB.Pointer THEN tp := tp.base END ;
(*PutC(tp.size);*) (*does not include tag*)
Q(tp); n := tp.nofpar; 
WHILE n < 2 DO PutC(0); INC(n) END ;*)
Writeln ('');
END AllocTD;

(* Items: Conversion from constants or from Objects on the Heap to Items on the Stack*)

PROCEDURE Val*(VAR x: Item): LONGINT;
BEGIN 
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
IF OSAS.name = "" THEN (*Out.String ("''"); Out.Ln*) END;
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
VAR cl: INTEGER; r: LONGINT; k, kr : LONGINT; kname, krname : OSAS.Ident;
BEGIN cl := y.class; x.mode := cl; x.type := y.type; x.a := y.val; x.b := y.lev; x.rdo := y.rdo; 
x.not := FALSE; x.and := FALSE; x.or := FALSE;  COPY (y.label, x.label); x.ilabel := "";
COPY(y.name, x.name);
IF (y.lev = 0) THEN
IF y.label = '' THEN
 COPY (modname, x.label);
 Strings.Append ('_', x.label);
 Strings.Append (y.name, x.label)
END;
END;
IF y.lev <= -1 THEN
COPY(y.ilabel, x.ilabel);
COPY(x.ilabel, x.label);
Strings.Append ('_', x.label);
Strings.Append (y.name, x.label);
END;
IF cl = OSAB.Var THEN
IF y.lev <= 0 THEN x.r := PC;
 IF x.type.form = OSAB.String THEN
    (*named string constant*)
    x.mode := OSAB.Var 
 END ;
ELSIF y.lev = curlev THEN
 x.r := FP;
       IF x.a < 0 THEN (* parameter *)
	  k := x.a - 4; (* in WIrth's version offset of parameters always 4 less *)
	ELSIF x.a > 0 THEN (* local variable *)
	   k := x.a;
	ELSE Out.String ('x.a is 0'); Out.Ln; HALT(0);
	END;
	  IntStr.IntToStr(-k, kname);
	  ConvertRegNumToName (FP, krname);
	  x.label := kname;
	  Strings.Append ("(", x.label);
	  Strings.Append (krname, x.label);
	  Strings.Append (")", x.label);
	  (*mov -k(%ebp), r*)
ELSE OSAS.Mark("non-local not accessible"); x.r := FP
END
ELSIF cl = OSAB.Par THEN x.b := 0; (* var parameter *)
	IF x.a < 0 THEN (* parameter *)
	  k := x.a - 4; (* in WIrth's version offset of parameters always 4 less *)
	ELSIF x.a > 0 THEN (* local variable *)
	   k := x.a;
	ELSE Out.String ('  x.a is   0'); Out.Ln; HALT(0);
	END;
	  IntStr.IntToStr(-k, kname);
	  ConvertRegNumToName (FP, krname);
	  x.label := kname;
	  Strings.Append ("(", x.label);
	  Strings.Append (krname, x.label);
	  Strings.Append (")", x.label);
	  (*mov -k(%ebp), r*)
IF y.lev <= 0 THEN x.r := PC
ELSIF y.lev = curlev THEN 
 x.r := FP;
ELSE OSAS.Mark("non-local not accessible"); x.r := FP
END
ELSIF cl = OSAB.Typ THEN x.r := PC 
ELSIF cl = OSAB.Reg THEN x.r := y.val
ELSIF cl = OSAB.RegI THEN x.r := y.val; x.a := 0
END;
END MakeItem;

(* Code generation for Selectors, Variables, Constants *)

PROCEDURE Field*(VAR x: Item; y: OSAB.Object);   (* x := x.y *)
VAR r, ti: LONGINT; t : OSAS.Ident;
BEGIN
(*debug(x); 
Out.Int(x.type.form, 0);
Out.Ln;
DebugObj(y);*) (*HALT(0);*)
x.type := y.type; 
IF x.mode = OSAB.Typ THEN
GetReg(r);
MovVarToReg(x, r); 
AddConstToReg(y.val+4, r);
x.mode := OSAB.RegI;
x.r := r;
ELSIF (x.mode = OSAB.Var) OR (x.mode = OSAB.RegI) THEN x.a := x.a + y.val;
IntStr.IntToStr(y.val+4, t);
Strings.Append ('+', x.label);
Strings.Append (t, x.label);
ELSIF x.mode = OSAB.Par THEN x.b := x.b +  y.val;
 GetReg(r);
 LoadParamAdr(EBP, x.a-4, r, x); (* vtoroy -4 dlya type desc *)
 AddConstToReg(y.val+4, r);
 x.mode := OSAB.RegI;
 x.r := r;
(*x.label := '';
ti := -ti + 4;
ti := ti + y.val;
IntStr.IntToStr(ti, t);
Strings.Append(t, x.label);
Strings.Append('(%ebp)', x.label);
x.mode := OSAB.Var;*)
ELSIF x.mode = RegX THEN
GetReg2(r, x.r, x.a); Put0a(ADD, r, x.r, x.a, 0, x.b); x.mode := OSAB.RegI; x.r := r; x.a := y.val
ELSE OSAS.Mark("bad mode in Field")
END ;
END Field;

PROCEDURE Index*(VAR x, y: Item; notleaf: BOOLEAN);   (* x := x[y] *)
VAR r, r0, rr, rrr, s, s0, n, lim: LONGINT; (* y.a is index *)
str,offset, reg0, regy, xname, yname, rname, tmpstr, tsize : OSAS.Ident;
BEGIN 
s := x.type.base.size; lim := x.type.len;

IF (y.mode = OSAB.Const) & (lim >= 0) THEN
   IF (y.a >= 0) & (y.a < lim) THEN
      IF (x.mode = OSAB.Var) OR (x.mode = OSAB.RegI) THEN
          IF (x.r = EBP) & (x.a < 0) THEN (* parameter *)
             loadAdr(x);
             IF y.a # 0 THEN 
	        AddConstToReg(y.a*s, x.r);
             END;
             x.mode := OSAB.RegI
          ELSIF (x.r = EBP) & (x.a > 0) THEN (* local array *)
             x.label := '';
             IntStr.IntToStr(-x.a+y.a*s, offset);
             Strings.Append (offset, x.label);
             Strings.Append ("(%ebp)", x.label);
             x.mode := OSAB.Var;
          ELSE
             x.a := y.a * s + x.a;
             IntStr.IntToStr(y.a* s,offset);
             Strings.Append ('+', x.label); Strings.Append(offset, x.label);
             x.mode := OSAB.Var;
          END;
      ELSIF (x.mode = OSAB.Par) THEN 
          x.b := y.a * s + x.b;
      ELSIF x.mode = RegX THEN
          Release(x.b);
          x.mode := OSAB.RegI;
          x.a := y.a * s;
      ELSE OSAS.Mark("bad mode in Index")
      END
   ELSE OSAS.Mark("index out of range")
   END ;
ELSE 
         IF check THEN (*check index bounds*) Out.String ("check!"); Out.Ln;
            IF lim >= 0 THEN                   Out.String ("lim >= 0"); Out.Ln;
             (*AddConst(CMP, y, lim)*)
	     GetReg(rr);
	     MovConstToReg(lim, rr);
	     GetReg(rrr);
	     IF y.mode = OSAB.Var THEN
	        MovVarToReg(y, rrr);
             ELSE
	        Out.String ("why it so happened that y.mode here is not OSAB.Var? Check it, something strange happen"); Out.Ln; HALT(0);
             END;
	     ArrayCheck(rr, rrr);
	     Release (rrr); Release (rr);
            ELSE (*open array*)                   Out.String ("lim < 0"); Out.Ln;
               IF x.mode <= OSAB.Par THEN          Out.String ("x.mode <= OSAB.Par"); Out.Ln;
                 GetReg(rr);
                 LoadParamToReg(EBP, x.a-8, rr, x);
                 GetReg(rrr);
		 IF y.mode = OSAB.Const THEN
		    MovConstToReg(y.a, rrr);
		 ELSIF y.mode = OSAB.Var THEN
                    MovVarToReg(y, rrr);
		 END;
		 ArrayCheck(rr, rrr);
                 (*Put2(LDR, lim, x.r, x.a - 4);*)
                 Release(rrr); Release(rr);
               ELSIF x.mode = OSAB.RegI THEN       Out.String ("x.mode = OSAB.RegI"); Out.Ln;
                 lim := x.r - 1
               ELSE 
                 OSAS.Mark("bad mode in Index")
               END ;
               (*Put0(CMP, 0, y.r, lim)*)
            END ;
            (*Put6(2, 1)*)   (*SWI*)
         END ;



   IF (y.mode = OSAB.Const) & (y.a = 0) THEN
      IF (x.mode = OSAB.Par) THEN
         GetReg1(r, x.r);
         (*lea(x, r);*)
         LoadParamAdr(EBP, x.a-4, r, x);
         r0 := x.r; x.r := r;
         x.mode := OSAB.RegI;
         x.r := r;
         x.a := x.b;
      ELSIF (x.r = EBP) & (x.a > 0) THEN
         x.label := '';
         IntStr.IntToStr(-x.a(*+y.a*s*), offset);
         Strings.Append (offset, x.label);
         Strings.Append ("(%ebp)", x.label);
         x.mode := OSAB.Var;
      ELSIF x.mode IN {OSAB.Var, OSAB.RegI, RegX} THEN
         loadAdr(x);
         x.mode := OSAB.RegI
      END ;
   ELSIF (y.mode = OSAB.Const) & (y.a > 0 ) THEN
      IF x.mode = OSAB.Par THEN
         GetReg1(r, x.r);
         LoadParamAdr(EBP, x.a-4, r, x);
         (*lea(x, r);*)
         AddConstToReg(y.a, r);
         x.r := r;
         x.mode := OSAB.RegI;
         x.a := x.b;
      ELSIF (x.r = EBP) & (x.a > 0) THEN (* local var *)
         x.label := '';
         IntStr.IntToStr(-x.a+y.a*s, offset);
         Strings.Append(offset, x.label);
         Strings.Append ("(%ebp)", x.label);
         x.mode := OSAB.Var;
      ELSIF x.mode IN {OSAB.Var, OSAB.RegI, RegX} THEN
         loadAdr(x);
         x.mode := OSAB.RegI;
      END;
   ELSE
      load(y);
      (*
      IF check THEN (*check index bounds*) Out.String ("mtav"); Out.Ln; Out.String ("lim = "); Out.Int(lim, 0); Out.Ln;
         IF lim >= 0 THEN 
             (*AddConst(CMP, y, lim)*)
         ELSE (*open array*)
            IF x.mode <= OSAB.Par THEN
               GetReg(lim);
               LoadParamToReg(EBP, x.a-8, lim, x);
               ArrayCheck(y.r, lim);
               (*Put2(LDR, lim, x.r, x.a - 4);*)
               Release(lim)
            ELSIF x.mode = OSAB.RegI THEN
               lim := x.r - 1
            ELSE 
               OSAS.Mark("bad mode in Index")
            END ;
               (*Put0(CMP, 0, y.r, lim)*)
         END ;
         (*Put6(2, 1)*)   (*SWI*)
      END ;
*)
      IF (x.mode = OSAB.Par) THEN
          GetReg1(r, x.r);
          LoadParamAdr(EBP, x.a-4, r, x);
          r0 := x.r; x.r := r;
          ConvertRegNumToName (y.r, yname);
          IF s # 1 THEN
            IF s = 4 THEN
                ShiftL (2, y.r)
            ELSE
                MulConstWithReg (s, y.r)
            END;
          END;
          AddRegs (y.r, x.r);
          Release(y.r);
          (*IF s = 1 THEN
          MovAdrToReg(x.r, x.r, 1)
          ELSE
          MovAdrToReg(x.r, x.r, 4)
          END;*) (* eto delat nujno v load *)
          x.mode := OSAB.RegI;
          x.r := r;
          x.a := x.b;
      ELSIF x.mode = OSAB.RegI THEN
          ConvertRegNumToName(y.r, yname);
          IF s # 1 THEN
             IF s = 4 THEN
	        ShiftL (2, y.r)
             ELSE
	        MulConstWithReg (s, y.r)
             END;
          END;
          AddRegs (y.r, x.r);
          Release(y.r);
          x.mode := OSAB.RegI;
          x.r := r;
          x.a := x.b;

      ELSIF x.mode IN {OSAB.Var, (*OSAB.RegI,*) RegX} THEN
          IF (x.r = EBP) & (x.a > 0) THEN (* local array *)
            GetReg(r);
            lea (x, r);
            (*LoadParamAdr(EBP, x.a, r, x);*)
            x.r := r;
            ConvertRegNumToName (y.r, yname);
            IF s # 1 THEN
                IF s = 4 THEN
	            ShiftL (2, y.r)
                ELSE
	            MulConstWithReg (s, y.r)
                END;
            END;
            AddRegs (y.r, x.r);
            Release(y.r);
            x.mode := OSAB.RegI;
            x.r := r;
            x.a := x.b;
            (*x.label := '';
            IntStr.IntToStr(-x.a(*+y.a*s*), offset);
            Strings.Append (offset, x.label);
            Strings.Append ("(%ebp,", x.label);
            ConvertRegNumToName (y.r, yname);
            Strings.Append (yname, x.label);
            Strings.Append (",", x.label);
            IntStr.IntToStr (s, offset);
            Strings.Append (offset, x.label);
            Strings.Append (")", x.label);
            x.mode := OSAB.Var;
            Release (y.r);*)
          ELSE
            loadAdr(x);
            ConvertRegNumToName (y.r, yname);
            IF s # 1 THEN
                IF s = 4 THEN 
	            ShiftL (2, y.r)
                ELSE
	            MulConstWithReg (s, y.r)
                END;
            END;
            AddRegs(y.r, x.r);
            Release(y.r); (*Release(x.r);*)
            (*IF s = 1 THEN
	    MovAdrToReg(x.r, x.r, 1)
            ELSE
	    MovAdrToReg(x.r, x.r, 4)
            END;*)   (* eto delat nujno v load *)
            (*x.r := r;*) (*nottested*)
            x.mode := OSAB.RegI
          END;
      END ;
   END;
END ;
x.type := x.type.base
END Index;

PROCEDURE DeRef*(VAR x: Item);
VAR k, r: LONGINT;
BEGIN
IF x.mode = OSAB.Var THEN 
x.mode := (*OSAB.Par;*) OSAB.Typ; 
x.type.form := OSAB.Pointer;
x.b := 0
ELSIF x.mode = OSAB.Par THEN
(*GetReg1(r, x.r); *)
x.type.form := OSAB.Pointer;
(*k := x.a - 4; (* mov 8(%ebp), %edx *)
LoadParamToReg(EBP, k, r, x);*)
(*Put2(LDR, r, x.r, x.a); 
Put2(LDR, r, r, x.b); *)
(*x.mode := OSAB.RegI; *)
x.mode := OSAB.Par;
(*x.r := r; *)
(*x.a := 0*)
ELSIF x.mode = OSAB.RegI THEN 
GetReg1(r, x.r); 
(*Put2(LDR, r, x.r, x.a);*)
x.r := r; 
x.a := 0
ELSIF x.mode = OSAB.Reg THEN 
x.mode := OSAB.RegI; 
x.a := 0
ELSIF x.mode = RegX THEN
GetReg2(r, x.r, x.a); 
(*Put3(LDR, r, x.r, x.a, x.b);*) 
x.mode := OSAB.RegI; 
x.r := r; 
x.a := 0
ELSE OSAS.Mark("bad mode in DeRef"); r := 0
END ;
x.type := x.type.base
END DeRef;

PROCEDURE TypeTest*(VAR x: Item; T: OSAB.Type; varpar, isguard: INTEGER);
VAR r0, r: LONGINT; tdes: Item; r0name, rname : OSAS.Ident;
BEGIN
(*debug(x); DebugObj(x.type.base.typobj); (* x type is x.type.typobj.dsc, x record type is x.type.base.typobj *)
Out.Ln; Out.Ln;
DebugObj(T.typobj);*) (* T type is T.typobj *)
(*IF varpar = 1 THEN *)
   (*GetReg(r0);*)
   (*Put2(LDR, r0, FP, x.a+4)*)
(*ELSE *)
   load(x);
      MovAdrToReg(x.r, x.r, 4);
   Write ("movl "); Writeint (T.typobj.type.nofpar * 4);
   ConvertRegNumToName (x.r, rname);
   Write('('); Write (rname); Write ('), '); Writeln (rname);
   GetReg(r0);
   Write ('leal ');
   IF T.typobj.lev = 0 THEN
      Write (modname); 
   ELSIF T.typobj.lev < 0 THEN
   (* find and write object module name *)
   END;
   Write ('_'); Write(T.typobj.name); Write('_'); Write('typedsc'); Write ('_');
   ConvertRegNumToName (r0, r0name);
   Write (', ');
   Writeln (r0name);
   (*Put2(LDR, r0, x.r, -4);*)
(*END ;*)

(*Put2(LDR, r0, r0, T.nofpar*4);*)  (*TD of x*)
tdes.mode := OSAB.Var;
tdes.a := T.typobj.val;
tdes.b := T.typobj.lev;
MakeItem(tdes, T.typobj);  (*TD of y*) 
tdes.mode := OSAB.Var;
IF isguard = 0 THEN
   GetReg(r);
   Movl (x.r, r);
   CmpInRegs(r0, r);
   sete(x.r); Release(x.r); x.r := r;
   (*Movl (r, x.r);*)
   Release(r);
ELSIF isguard = 1 THEN
   CmpInRegs(r0, x.r);

   jne ("OBERON_ERRORCODE_");
   x.mode := OSAB.Var; Release (x.r);
END;
(*loadAdr(tdes);
Put0(CMP, 0, r0, tdes.r);
Release(tdes.r);*)
Release(r0); (*Release(x.r);*)
IF isguard # 1 THEN Release(x.r) END ;
IF isguard = 0 THEN SetCC(x, 0) (*ELSE Put6(1, 2)*) END
END TypeTest;

(* Code generation for Boolean operators *)


PROCEDURE Not*(VAR x: Item);   (* x := ~x *)
VAR rname : OSAS.Ident;
t,r : LONGINT;
BEGIN Out.String ("Not"); Out.Ln;
Out.String ("x.a="); Out.Int(x.a, 0); Out.Ln;
Out.String ("x.b="); Out.Int (x.b, 0); Out.Ln;
Out.String ("x.mode="); Out.Int(x.mode, 0); Out.Ln;
IF x.mode # CC THEN loadCC(x) END ;
x.not := TRUE; (*x.r := negated(x.r);*) (*x.op := Inv(x.op);*)
Write ('cmpb $0, '); GetReg(r); Convert8RegNumToName(r, rname); Release(r); Writeln (rname);
Write ('sete '); Writeln (rname);
Out.String ("x.mode="); Out.Int(x.mode, 0); Out.Ln;
t := x.a; x.a := x.b; x.b := t;
END Not;

PROCEDURE And1*(VAR x: Item);   (* x := x & *)
VAR lab, num, xname : OSAS.Ident;
reg : LONGINT;
BEGIN
   IF x.mode # CC THEN
      loadCC(x); 
      CmpConstWithReg (0, x.r);
      Out.String ("x.a="); Out.Int(x.a, 0); Out.Ln;
      Out.String ("x.b="); Out.Int(x.b, 0); Out.Ln;
      Out.String ("x.not="); IF x.not THEN Out.String("TRUE") ELSE Out.String ("FALSE") END;

      INC (landnext);
      COPY ('_', lab);
      Strings.Append (modname, lab);
      Strings.Append ('_', lab);
      Strings.Append (labelandnext, lab);
      IntStr.IntToStr(landnext, num);
      Strings.Append (num, lab);
      jnz (lab);
      COPY ('_', lab);
      Strings.Append (modname, lab);
      Strings.Append ('_', lab);
      Strings.Append (lableandfalse, lab);
      IntStr.IntToStr(landfalse, num);
      Strings.Append(num, lab);
      jmp (lab);
   END ;
x.and := TRUE;
END And1;

PROCEDURE And2*(VAR x, y: Item);
VAR lab, num : OSAS.Ident;
BEGIN 
   Write ('_'); Write (modname); Write ('_'); Write (labelandnext); Writeint(landnext); Writeln(':'); INC(landnext);
   IF y.mode # CC THEN
      loadCC(y);
      
      CmpConstWithReg (0, y.r);
      COPY ('_', lab);
      Strings.Append (modname, lab);
      Strings.Append ('_', lab);
      Strings.Append (labelandnext, lab);
      (*INC (landnext);*)
      IntStr.IntToStr (landnext, num);
      Strings.Append (num, lab);
      jnz (lab);
      COPY ('_', lab);
      Strings.Append (modname, lab);
      Strings.Append ('_', lab);
      Strings.Append (lableandfalse, lab);
      IntStr.IntToStr (landfalse, num);
      Strings.Append (num, lab);
      jmp (lab);
   END;
END And2;

PROCEDURE And3*(VAR y : Item);
VAR lab, num : OSAS.Ident;
BEGIN
   Write ('_'); Write (modname); Write ('_'); Write (labelandnext); Writeint(landnext); Writeln(':'); INC(landnext);
   MovConstToReg (1, y.r); 
   lab := '_';
   Strings.Append (modname, lab);
   Strings.Append ('_', lab);
   Strings.Append (labelandend, lab);
   IntStr.IntToStr (landend, num);
   Strings.Append (num, lab);
   jmp (lab);
   Write ('_'); Write (modname); Write ('_'); Write (lableandfalse); Writeint(landfalse); INC(landfalse); Writeln (':');
   MovConstToReg (0, y.r);
   Write ('_'); Write (modname); Write ('_'); Write (labelandend); Writeint (landend); Writeln(':'); INC (landend);
END And3;


PROCEDURE Or1*(VAR x: Item);   (* x := x OR *)
VAR num, lab : OSAS.Ident;
reg : LONGINT;
BEGIN
IF x.mode # CC THEN
   loadCC(x);
   CmpConstWithReg (0, x.r);
   lab := '_';
   Strings.Append (modname, lab);
   Strings.Append ('_', lab);
   Strings.Append (labelortrue, lab);
   IntStr.IntToStr (lortrue, num);
   Strings.Append (num, lab);
   jnz (lab);
   INC (lornext);
   lab := '_';
   Strings.Append (modname, lab);
   Strings.Append ('_', lab);
   Strings.Append (labelornext, lab);
   IntStr.IntToStr (lornext, num);
   Strings.Append (num, lab);
   jmp (lab);
END ;
(*Put5(x.r, x.b); x.b := pc-1; FixLink(x.a);*) x.a := 0
END Or1;

PROCEDURE Or2*(VAR x, y: Item);
VAR num, lab : OSAS.Ident;
BEGIN
Write ('_'); Write (modname); Write ('_'); Write (labelornext); Writeint(lornext); Writeln(':');
   IF y.mode # CC THEN
      loadCC(y);
      CmpConstWithReg (0, y.r);
      lab := '_';
      Strings.Append (modname, lab);
      Strings.Append ('_', lab);
      Strings.Append (labelortrue, lab);
      IntStr.IntToStr (lortrue, num);
      Strings.Append (num, lab);
      jnz (lab);
      INC (lornext);
      lab := '_';
      Strings.Append (modname, lab);
      Strings.Append ('_', lab);
      Strings.Append (labelornext, lab);
      IntStr.IntToStr (lornext, num);
      Strings.Append (num, lab);
      jmp (lab);
   END ;
END Or2;

PROCEDURE Or3*(VAR y : Item);
VAR lab, num : OSAS.Ident;
BEGIN
   Write ('_'); Write (modname); Write ('_');  Write (labelornext); Writeint(lornext); Writeln(':');
   MovConstToReg (0, y.r);
   lab := '_';
   Strings.Append (modname, lab);
   Strings.Append ('_', lab);
   Strings.Append (labelorend, lab);
   IntStr.IntToStr (lorend, num);
   Strings.Append (num, lab);
   jmp (lab);
   Write ('_'); Write (modname); Write ('_'); Write (labelortrue); Writeint(lortrue); INC(lortrue); Writeln (':');
   MovConstToReg (1, y.r);
   Write ('_'); Write (modname); Write ('_'); Write (labelorend); Writeint (lorend); Writeln(':'); INC (lorend);
END Or3;

(* Code generation for arithmetic operators *)

PROCEDURE Neg*(VAR x: Item);   (* x := -x *)
VAR r: LONGINT;
BEGIN
   IF x.type.form = OSAB.Int THEN
      IF x.mode = OSAB.Const THEN x.a := -x.a
      ELSE load(x); GetReg1(r, x.r); 
      negreg(r);
      END
   ELSIF x.type.form = OSAB.Real THEN
      IF x.mode = OSAB.Const THEN
         IF x.a # 0 THEN x.a := -x.a (*+*) (*-2147483648*)  (*80000000H*) END (* ! *)
      ELSE load(x); (*Put1a(2, x.r, x.r, 2, 1)*)
      END
   ELSE (*form = Set*)
      IF x.mode = OSAB.Const THEN x.a := -x.a-1 
      ELSE load(x); GetReg1(r, x.r);
      negreg (r);
      (*Put0(MVN, r, 0, x.r);*) x.r := r
      END
   END
END Neg;

PROCEDURE AddOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
VAR r: LONGINT;
BEGIN
   IF op = OSAS.plus THEN
      IF y.mode = OSAB.Const THEN
          IF x.mode = OSAB.Const THEN x.a := y.a + x.a
          ELSE load(x);
             IF y.a >= 0 THEN 
		AddConstToReg (y.a, x.r);     
	     ELSE
	        SubRegWithConst (y.a, x.r);
	     END
          END
      ELSIF x.mode = OSAB.Const THEN 
         load(y);
         IF x.a >= 0 THEN 
	    AddConstToReg (x.a, y.r);
         ELSE
	    SubRegWithConst (x.a, y.r);
         END ;
      x := y
      ELSIF (x.mode = OSAB.Reg) & (y.mode=OSAB.Reg) THEN
      AddRegs(y.r, x.r); Release (y.r);
      ELSIF (x.mode = OSAB.RegI) & (y.mode = OSAB.RegI) THEN
        IF x.type.size = 1 THEN
           MovAdrToReg(x.r, x.r, 1);
        ELSIF x.type.size >= 4 THEN
           MovAdrToReg(x.r, x.r, 4);
        END;
        IF y.type.size = 1 THEN
          MovAdrToReg(y.r, y.r, 1);
        ELSIF y.type.size >=4 THEN
           MovAdrToReg(y.r, y.r, 4);
        END;
        AddRegs(y.r, x.r); Release(y.r); x.mode := OSAB.Reg;
      ELSIF x.mode = OSAB.RegI THEN
        IF x.type.size = 1 THEN
           MovAdrToReg(x.r, x.r, 1);
        ELSIF x.type.size >= 4 THEN
           MovAdrToReg(x.r, x.r, 4);
        END;
	x.mode := OSAB.Reg;
        IF y.mode = OSAB.Const THEN
           AddConstToReg(y.a, x.r)
	ELSIF y.mode = OSAB.Var THEN
           AddMemToReg(y, x.r);
	ELSIF y.mode = OSAB.Reg THEN
	   AddRegs(y.r, x.r)
	END;
      ELSIF y.mode = OSAB.RegI THEN
        IF y.type.size = 1 THEN
           MovAdrToReg(y.r, y.r, 1);
        ELSIF y.type.size >= 4 THEN
           MovAdrToReg(y.r, y.r, 4);
        END;
	y.mode := OSAB.Reg;
	GetReg(r);
	LoadParamToReg(EBP, x.a, r, x);
        x.r := r;
	x.mode := OSAB.Reg;
	AddRegs(y.r, x.r);
	Release(y.r);
      ELSIF (y.mode = OSAB.Reg) & (x.mode = OSAB.Var) THEN
      (*GetReg1 (r, y.r);*) (* just commented to test procedures *)
      AddMemToReg (x, y.r);
      x.r := y.r; x.mode := OSAB.Reg;
      ELSE
      (* for risc like 
      load(x); load(y);
      GetReg2(r, x.r, y.r);
      AddRegs(r, x.r, y.r);
      x.r := r;
      -----------------*)
      (* for cisc like *)
      load (x); 
      GetReg1(r, x.r); 
      AddMemToReg (y, x.r);
      x.r := r; x.mode := OSAB.Reg;
      END
   ELSE (*op = minus*)
      IF y.mode = OSAB.Const THEN
         IF x.mode = OSAB.Const THEN x.a := x.a - y.a
         ELSE load(x);
            IF y.a >= 0 THEN
	       SubRegWithConst (y.a, x.r);
	    ELSE
	       AddConstToReg(-y.a, x.r)
	    END;
         END
      ELSIF x.mode = OSAB.Const THEN
         load(y); 
         IF x.a >= 0 THEN
	    load(x);
	    Release (x.r); (*Release (y.r);*) (* pomenyal komment mestami *)
	    SubRegs(y.r, x.r);
	    Movl (x.r, y.r);
	    x.r := y.r
         ELSE
	    load (x);
	    Release (x.r); (*Release (y.r);*) (* pomenyal komment mestami*)
	    SubRegs ( y.r, x.r);
	    Movl (x.r, y.r);
	    x.r := y.r;
         END ;
         x := y
      ELSIF (y.mode = OSAB.Reg) THEN 
         load(x); (*Release (x.r);*) Release (y.r);
         SubRegs (y.r, x.r); 
      ELSIF (x.mode = OSAB.RegI) & (y.mode = OSAB.RegI) THEN
        IF x.type.size = 1 THEN
           MovAdrToReg(x.r, x.r, 1);
        ELSIF x.type.size >= 4 THEN
           MovAdrToReg(x.r, x.r, 4);
        END;
        IF y.type.size = 1 THEN
          MovAdrToReg(y.r, y.r, 1);
        ELSIF y.type.size >=4 THEN
           MovAdrToReg(y.r, y.r, 4);
        END;
        SubRegs(y.r, x.r); Release(y.r); x.mode := OSAB.Reg;
      ELSIF x.mode = OSAB.RegI THEN
        IF x.type.size = 1 THEN
           MovAdrToReg(x.r, x.r, 1);
        ELSIF x.type.size >= 4 THEN
           MovAdrToReg(x.r, x.r, 4);
        END;
	x.mode := OSAB.Reg;
        IF y.mode = OSAB.Const THEN
           SubRegWithConst(y.a, x.r)
	ELSIF y.mode = OSAB.Var THEN
           SubRegWithMem(x.r, y);
	ELSIF y.mode = OSAB.Reg THEN
	   AddRegs(y.r, x.r)
	END;
       ELSIF y.mode = OSAB.RegI THEN
        IF y.type.size = 1 THEN
           MovAdrToReg(y.r, y.r, 1);
        ELSIF y.type.size >= 4 THEN
           MovAdrToReg(y.r, y.r, 4);
        END;
	y.mode := OSAB.Reg;
	GetReg(r);
	LoadParamToReg(EBP, x.a, r, x);
        x.r := r;
	x.mode := OSAB.Reg;
	SubRegs(y.r, x.r);
	Release(y.r);
    
      ELSE
      load (x);
      GetReg1 (r, x.r);
      SubRegWithMem (x.r, y);
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
   a := FALSE;
   IF EAX IN regs THEN
      a := TRUE;
      Push (EAX);
   END;
   regs := regs + {EAX};
   IF x.mode = OSAB.Var THEN
                   (* loading dividend to eax *)
      MovVarToReg (x, R0); 	   
   END;
   IF x.mode = OSAB.Const THEN
      MovConstToReg (x.a, R0);
   END;
   IF x.mode = OSAB.Reg THEN
      Movl (x.r, R0);
      Release (x.r);
   END;

   d := FALSE;
   IF EDX IN regs THEN
      d := TRUE;
      Push (EDX);
   END;
   regs := regs + {EDX};
   Writeln ("cltd");
   (* loading zero to edx as it must be 0 during div *)
   (*MovConstToReg (0, EDX);*)
   IF y.mode = OSAB.Var THEN
      Write ("idivl "); (* gas syntax *)
      (*Write ("idiv (");*) (* hla syntax *)
      Writeln (y.label); 
      (*Write (');');*)     (* hla syntax *)
   END;
   IF y.mode = OSAB.Const THEN
      (*Write ("$"); IntStr.IntToStr(y.a, constname2); Write(constname2)*)
      load(y);
      Write ("idivl "); ConvertRegNumToName (y.r, yname); Writeln (yname);    (* gas syntax *) 
      (*Write ("idiv ("); ConvertRegNumToName (y.r, yname); Write (yname); Writeln (');');*) (* hla syntax *)
      Release (y.r); y.mode := OSAB.Const;
   END;
   IF y.mode = OSAB.Reg THEN
      Write ("idivl "); (* gas syntax *)
      (*Write ("idiv (");*) (* hla syntax *)
      ConvertRegNumToName(y.r, yname);
      Write (yname); 
      (*Writeln (');');*) (* hla syntax *)
      Writeln (';') (* gas syntax *)
   END;
   x.r := EAX; x.mode := 8;
   (*GetReg1 (r, x.r);*)
   GetReg (r);
   Movl (R0, r);
   x.r := r; x.mode := 8; (* register *)
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
xname, yname, rname, constname, constname2, lab, num : OSAS.Ident;
BEGIN 
   a := FALSE;
   IF EAX IN regs THEN
      a := TRUE;
      Push(EAX);
   END;
   regs := regs + {EAX}; (* was it free, if not save it *)
   IF x.mode = OSAB.Var THEN
      MovVarToReg (x, R0);
   END;
   IF x.mode = OSAB.Const THEN 
      MovConstToReg (x.a, R0);
   END;
   IF x.mode = OSAB.Reg THEN 
      Movl (x.r, R0);
      Release (x.r);
   END;
   d := FALSE;
   IF EDX IN regs THEN
      Push(EDX);
      d := TRUE;
   END;
   regs := regs + {EDX}; (* was it free, if not save it *)
   (* loading zero to edx as it must be 0 during div *)
   Writeln ('cltd');
   (*MovConstToReg (0, EDX);*)
   IF y.mode = OSAB.Var THEN
      Write ("idivl ");       (* gas syntax *)
     (*Write ("idiv (");*) (* hla syntax *)
      Writeln (y.label); (*Writeln (');');*)
   END;
      IF y.mode = OSAB.Const  THEN
	 load(y); 
	 (*Write ("idiv (");*) (* hla syntax *)
	 Write ("idivl ");     (* gas syntax *)
	 ConvertRegNumToName (y.r, yname); Writeln (yname);
	 (*Writeln (');');*) (* hla syntax *)
	 (*Writeln (';'); *)
	 Release (y.r);
     ELSIF y.mode = OSAB.Reg THEN
         (*Write ("idiv (");*) (* hla syntax *)
         Write ("idivl ");                       (* gas syntax *)
      ConvertRegNumToName(y.r, yname);
      Write (yname); 
      (*Writeln (');');*) (* hla syntax *)
      Writeln (';') (* gas syntax *)
   END;
   x.r := EDX; x.mode := 8;

   (* mathematically correct modulus *)
   CmpConstWithReg(0, EDX);
   lab := '_';
   Strings.Append(modname, lab);
   Strings.Append('_', lab);
   Strings.Append(labelmod, lab);
   Strings.Append('_', lab);
   INC(labmodnum);
   IntStr.IntToStr(labmodnum, num);
   Strings.Append(num, lab);
   jge(lab);
   IF y.mode = OSAB.Var THEN MovVarToReg(y, EAX)
   ELSIF y.mode = OSAB.Const THEN MovConstToReg(y.a, EAX)
   ELSIF y.mode = OSAB.Reg THEN Movl (y.r, EAX)
   END;
   AddRegs(EAX, EDX);
   PutLabel(lab);
   


   GetReg (r);
   Movl (EDX, r);

   x.r := r; x.mode := 8; (* register *)

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
BEGIN 
   GetReg1(r, x.r);
   MulConstWithReg (c.a, x.r);
   x.r := r;
END multiply;

PROCEDURE MulOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
VAR n, r: LONGINT;
BEGIN
   IF (x.mode = OSAB.Const) & (y.mode = OSAB.Const) THEN
      IF op = OSAS.times THEN x.a := x.a * y.a;
      ELSIF y.a = 0 THEN OSAS.Mark("division by 0")
      ELSIF op = OSAS.div THEN x.a := x.a DIV y.a
      ELSE x.a := x.a MOD y.a
      END
   ELSE
     IF op = OSAS.times THEN
        IF (x.mode = OSAB.Const) (*& (x.a >= 2)*) THEN
           load(y); multiply(y, x); x.mode := OSAB.Reg; x.r := y.r;
        ELSIF (y.mode = OSAB.Const) (*& (y.a >= 2)*) THEN load(x); multiply(x, y);
	ELSIF (x.mode = OSAB.Reg) & (y.mode = OSAB.Reg) THEN
           MulRegs (y.r, x.r); Release (y.r);
	ELSIF (x.mode = OSAB.RegI) & (y.mode = OSAB.RegI) THEN
	   IF x.type.size = 1 THEN
	      MovAdrToReg(x.r, x.r, 1);
	   ELSIF x.type.size >= 4 THEN
	      MovAdrToReg(x.r, x.r, 4);
	   END;
	   IF y.type.size = 1 THEN
	     MovAdrToReg(y.r, y.r, 1);
	   ELSIF y.type.size >=4 THEN
	      MovAdrToReg(y.r, y.r, 4);
	   END;
	   MulRegs(y.r, x.r); Release(y.r);
	   x.mode := OSAB.Reg; 
        ELSIF x.mode = OSAB.RegI THEN
           IF x.type.size = 1 THEN
              MovAdrToReg(x.r, x.r, 1);
           ELSIF x.type.size >= 4 THEN
              MovAdrToReg(x.r, x.r, 4);
           END;
	   x.mode := OSAB.Reg;
           IF y.mode = OSAB.Const THEN
              MulConstWithReg(y.a, x.r)
	   ELSIF y.mode = OSAB.Var THEN
              MulMemToReg(y, x.r);
	   ELSIF y.mode = OSAB.Reg THEN
	      MulRegs(y.r, x.r)
	   END;
        ELSIF y.mode = OSAB.RegI THEN
           IF y.type.size = 1 THEN
              MovAdrToReg(y.r, y.r, 1);
           ELSIF y.type.size >= 4 THEN
              MovAdrToReg(y.r, y.r, 4);
           END;
           y.mode := OSAB.Reg;
           GetReg(r);
           LoadParamToReg(EBP, x.a, r, x);
           x.r := r;
	   x.mode := OSAB.Reg;
	   MulRegs(y.r, x.r);
	   Release(y.r);
	ELSIF (y.mode = OSAB.Reg) & (x.mode = OSAB.Var) THEN
	   load(x); GetReg1(r, x.r);
	   MulRegs (y.r, x.r); Release (y.r);
        ELSE load(x);
	   GetReg1(r, x.r);
           MulMemToReg (y, x.r);
	   x.mode := OSAB.Reg;
	   x.r := r;
        END
   ELSIF op = OSAS.div THEN
      IF x.mode = OSAB.RegI THEN
         IF x.type.size = 1 THEN
            MovAdrToReg(x.r, x.r, 1);
         ELSIF x.type.size >= 4 THEN
	    MovAdrToReg(x.r, x.r, 4);
         END;
	 x.mode := OSAB.Reg; Release(x.r);
      END;
      IF y.mode = OSAB.RegI THEN
	 IF y.type.size = 1 THEN
	    MovAdrToReg(y.r, y.r, 1);
	 ELSIF y.type.size >=4 THEN
	    MovAdrToReg(y.r, y.r, 4);
	 END;
         y.mode := OSAB.Reg; Release(y.r);
      END;
      divide (x, y);
   ELSIF op = OSAS.mod THEN (*load(x);*)
      IF x.mode = OSAB.RegI THEN
         IF x.type.size = 1 THEN
            MovAdrToReg(x.r, x.r, 1);
         ELSIF x.type.size >= 4 THEN
	    MovAdrToReg(x.r, x.r, 4);
         END;
	 x.mode := OSAB.Reg; Release(x.r);
      END;
      IF y.mode = OSAB.RegI THEN
	 IF y.type.size = 1 THEN
	    MovAdrToReg(y.r, y.r, 1);
	 ELSIF y.type.size >=4 THEN
	    MovAdrToReg(y.r, y.r, 4);
	 END;
         y.mode := OSAB.Reg; Release(y.r);
      END;

   modulus (x, y);
   END
END
END MulOp;

(* Code generation for REAL operators *)

PROCEDURE PrepOpd*(VAR x: Item);
BEGIN (*load(x)*)  (*for real division only*)
END PrepOpd;

PROCEDURE loadFP(VAR x : Item);
VAR k, r : LONGINT; rname, num : OSAS.Ident;
BEGIN Out.String ("PROCEDURE loadFP"); Out.Ln;
Out.String ("x.rdo="); IF x.rdo THEN Out.String ("TRUE") ELSE Out.String("FALSE"); END; Out.Ln;
debug(x); Out.String ("x.type.form="); Out.Int(x.type.form,0);
Out.Ln;
IF x.mode # OSAB.Reg THEN Out.String ("x.mode # OSAB.Reg"); Out.Ln;
   IF x.mode = OSAB.Const THEN

      Writeln ('.section .rodata');
      Writeln ('.globl');
      COPY (modname, x.label);
      Strings.Append ('_', x.label);
      INC(labelfloatinx);
      IntStr.IntToStr(labelfloatinx, num);
      Strings.Append (labelfloat, x.label);
      Strings.Append (num, x.label);
      Write (x.label); Write (': .long $'); Writeint(x.a); Writeln ('');
      Writeln ('.section .text');

      (*Write ('movl  $'); Writeint(x.a); Write (', '); GetReg(r); ConvertRegNumToName (r, rname); Writeln (rname);*)
      Write ('flds '); Writeln (x.label);(*Writeln(rname); Release(r);*)
      x.mode := OSAB.Reg;
   ELSIF x.mode = OSAB. Var THEN Out.String ("x.mode = OSAB.Var"); Out.Ln;
      IF x.r # EBP THEN Out.String ("x.r # EBP"); Out.Ln;
         Write ('flds '); Writeln(x.label);
         x.mode := OSAB.Reg;
      ELSE (* local var or par*) Out.String ("x.r = EBP"); Out.Ln;
         IF x.a > 0 (* local variable *) THEN Out.String ("x.a > 0"); Out.Ln;
	    Write ('flds '); Writeln (x.label);
	 ELSE (* parameter*) Out.String ("x.a <= 0"); Out.Ln;
            Write ('flds '); Writeln (x.label); 
	 END;
         x.mode := OSAB.Reg;
      END
   ELSIF x.mode = OSAB.Par THEN Out.String ("x.mode = OSAB.Par"); Out.Ln;
      k := x.a - 4;
      IF x.type.form # OSAB.Pointer THEN Out.String ("x.type.form # OSAB.Pointer"); Out.Ln;
         (* it seems this it the place to load var parameter*)
	 Write ('movl '); Writeint (-k); Write("(%ebp), "); GetReg(r); ConvertRegNumToName(r, rname); Writeln(rname);
	 Write ('flds '); Write ("("); Write(rname); Writeln (")"); Release(r);
      ELSE Out.String ("x.type.form = OSAB.Pointer"); Out.Ln;
         Write ('flds '); Writeln (x.label);
      END;
      x.mode := OSAB.Reg
   END
END;
END loadFP;


PROCEDURE RealOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
VAR r: LONGINT;  (*x.type.form = Real*)
BEGIN 
debug(x); debug(y);
(*load(x); load(y);*)
loadFP(y); loadFP(x);
   (*IF regs = {FP-2, FP-1} THEN*)
      IF op = OSAS.plus THEN 
         (*op := 1*)
	 Writeln ('faddp %st,%st(1)');
      ELSIF op = OSAS.minus THEN 
         (*Put1a(2, y.r, y.r, 2, 1); 
	 op := 1*)
	 Writeln ('fsubp %st,%st(1)');
      ELSIF op = OSAS.times THEN 
         (*op := 3*)
	 Writeln ('fmulp %st,%st(1)');
      ELSIF op = OSAS.rdiv THEN 
         (*op := 5*)
	 Writeln ('fdivp %st,%st(1)');
      END;
  (* ELSIF regs = {FP-3 .. FP-1} THEN
      IF op = OSAS.plus THEN 
         op := 2
      ELSIF op = OSAS.minus THEN 
         Put1a(2, y.r, y.r, 2, 1); 
	 op := 2
      ELSIF op = OSAS.times THEN 
         op := 4
      ELSIF op = OSAS.rdiv THEN 
         op := 6
      END
   ELSE 
      OSAS.Mark("simplify expression!")
   END ;*)
   (*GetReg2(x.r, x.r, y.r);*)
   (*Put5a(op*10000H + fixlistFP); 
   fixlistFP := pc-1*)
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
VAR r0: LONGINT; lab, num : OSAS.Ident;
BEGIN
IF y.mode = OSAB.Const THEN 
   x.mode := OSAB.Const; x.a := ASH(1, y.a)
ELSE 
   load(y); 
   (*GetReg1(x.r, y.r); *)
   (*GetReg(r); *)
   (*Put1(MOV, r, 0, 1); Put0b(MOV+1, x.r, 0, r, 0, y.r);*)

         GetReg (r0);
         MovConstToReg (1, r0);
         INC(labelnum);
         Write ('_'); Write (modname); Write ('_');
         Write (labelnumber); Writeint (labelnum); Writeln (":");
	 DecReg (y.r);
	 CmpConstWithReg (0, y.r);
	 lab := '_';
	 Strings.Append (modname, lab);
	 Strings.Append ('_', lab);
	 Strings.Append (labelnumber, lab);
	 IntStr.IntToStr (labelnum+1, num);
	 Strings.Append (num, lab);
         jl (lab);
         ShiftL (1, r0);
         lab := '_';
	 Strings.Append (modname, lab);
	 Strings.Append ('_', lab);
	 Strings.Append (labelnumber, lab);
	 IntStr.IntToStr (labelnum, num);
	 Strings.Append (num, lab);
	 jnz (lab);
	 INC(labelnum);
         Write ('_'); Write (modname); Write ('_');
         Write (labelnumber); Writeint (labelnum); Writeln (":");
   (*Release(r0);*)x.r := r0; x.mode := OSAB.Reg; Release (y.r);
END
END Singleton;

PROCEDURE Set*(VAR x, y, z: Item);   (* x := {y .. z} *)
VAR r0, r1: LONGINT; rname, rrname, lab, num : OSAS.Ident;
BEGIN
   IF (y.mode = OSAB.Const) & (z.mode = OSAB.Const) THEN
      x.mode := OSAB.Const;
      IF y.a <= z.a THEN x.a := ASH(2, z.a) - ASH(1, y.a) ELSE x.a := 0 END
   ELSE x.mode := OSAB.Reg; load(z); 
      IF (y.mode = OSAB.Const) & (y.a = 0) THEN
         GetReg(r0);
	 
	 MovConstToReg (2, r0);
        INC(labelnum);
         Write ('_'); Write (modname); Write ('_');
         Write (labelnumber); Writeint (labelnum); Writeln (":");
         ShiftL (1, r0);
         DecReg (z.r);

         lab := '_';
	 Strings.Append (modname, lab);
	 Strings.Append ('_', lab);
	 Strings.Append (labelnumber, lab);
	 IntStr.IntToStr (labelnum, num);
	 Strings.Append (num, lab);
	 jnz (lab);
	 DecReg (r0);
	 Release(r0); 
	 Release(x.r);
	 x.r := r0
      ELSE
         GetReg (r0);
         MovConstToReg (2, r0);
         INC(labelnum);
         Write ('_'); Write (modname); Write ('_');
         Write (labelnumber); Writeint (labelnum); Writeln (":");
         ShiftL (1, r0);
	 DecReg (z.r);
         lab := '_';
	 Strings.Append (modname, lab);
	 Strings.Append ('_', lab);
	 Strings.Append (labelnumber, lab);
	 IntStr.IntToStr (labelnum, num);
	 Strings.Append (num, lab);
	 jnz (lab);
         load(y);
         Release (z.r);
	 GetReg(r1);
	 MovConstToReg(1, r1);
         INC(labelnum);
         Write ('_'); Write (modname); Write ('_');
         Write (labelnumber); Writeint (labelnum); Writeln (":");
         ShiftL (1, r1);
         DecReg (y.r);
         lab := '_';
	 Strings.Append (modname, lab);
	 Strings.Append ('_', lab);
	 Strings.Append (labelnumber, lab);
	 IntStr.IntToStr (labelnum, num);
	 Strings.Append (num, lab);
	 jnz (lab);
         SubRegs (r1, r0);
         Release (r0); Release (r1);
         Release (y.r); Release (x.r); x.r := r0;
      END
   END
END Set;

PROCEDURE In*(VAR x, y: Item);  (* x := x IN y *)
VAR r: LONGINT;
xname, yname : OSAS.Ident;
BEGIN load(y);
   ConvertRegNumToName (y.r, yname); 
   IF x.mode = OSAB.Const THEN
       BitTest (x.a, y.r);
   ELSE 
      load(x); GetReg(r);
      BitTestRegs (x.r, y.r);
      Release(r); Release(x.r)
   END ;
   IF ~x.not THEN
      setc (y.r);
   ELSE
      setnc(y.r);
   END;
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
	 x.r := r; (*nottested*)
      END
   ELSIF x.mode = OSAB.Const THEN
      load(y); load(x);
      GetReg2 (r, x.r, y.r);
      SetOpInRegs(op, x, y);
      x.r := r; (*nottested*)
   ELSIF (y.mode = OSAB.Reg) & (x.mode = OSAB.Var) THEN
      GetReg1 (r, y.r); SetOpInRegAndMem (op, y, x);
      x.r := r; x.mode := OSAB.Reg;
   ELSE 
      load(x);
      GetReg1 (r, x.r);
      SetOpInRegAndMem (op, x, y); x.mode := OSAB.Reg;
      x.r := r; (*nottested*)
   END ;
(*x.r := r*) (*nottested*)
ELSE (* op0 = 28 *)
   GetReg2 (r, x.r, y.r );
   SetMinus (x, y);
   x.r := r;
END;
END SetOp;

(* Code generation for relations *)

PROCEDURE IntRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
VAR r: LONGINT;
BEGIN
   IF (x.mode = OSAB.Const) & (x.type.form # OSAB.Proc) THEN (*Out.String ("x.mode=OSAB.Const"); Out.Ln;*)
      load(y);  (*CMPI*)
      (*IF x.a >= 0 THEN 
      AddConst(CMP, y, x.a) ELSE AddConst(CMP+2, y, -x.a) END ;*)
      CompareInRegAndConst (op, y, x);
      Release(y.r); SetCC (x, op);
   ELSE
   load(x); load(y); CompareInRegs (op, y, x);
      Release(y.r); Release(x.r); debug(x);
      SetCC (x, op);
   END
END IntRelation;

PROCEDURE SetRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
VAR k: LONGINT;
BEGIN load(x); load(y);
IF op = OSAS.eql THEN (*Put0(19, 0, x.r, y.r);*) k := 0  (*TEQ*)
ELSIF op = OSAS.neq THEN (*Put0(19, 0, x.r, y.r);*) k := 1  (*TEQ*)
ELSIF (op = OSAS.lss) OR (op = OSAS.leq) THEN (*Put0(29, LNK, x.r, y.r);*) k := 0
ELSE (*gtr OR geq*) (*Put0(29, LNK, y.r, x.r);*) k := 0  (*BIC; LNK used as dummy*)
END ;
Release(y.r); Release(x.r); SetCC(x, k);
END SetRelation;

PROCEDURE RealRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
VAR r : LONGINT; rname : OSAS.Ident;
BEGIN 
loadFP(x); 
   IF (y.mode = OSAB.Const) & (y.a = 0) THEN 
      (*Put1(CMP, 0, x.r, 0)*) Out.String (y.label); Out.Ln;
      Writeln ('ftst'); Writeint(y.a); Writeln ('');
   ELSE 
      loadFP(y);
      (*Writeln ('fstsw');*)
      Writeln ('fxch %st(1)');
      Writeln ('fucomip %st(1), %st');
      Writeln ('fstp %st(0)');
      GetReg(r); Convert8RegNumToName(r, rname); Release (r);
      IF op = 13 (* > *) THEN
         Write ('seta '); Writeln (rname);
      ELSIF op = 11 (* < *) THEN
         Write ('setb '); Writeln (rname);
      ELSIF op = 14 (* >= *) THEN
         Write ('setae '); Writeln (rname);
      ELSIF op = 12 (* <= *) THEN
         Write ('setbe '); Writeln (rname);
      END;
      (*Writeln ('sahf');*)
      (*Put0(17, 0, x.r, y.r);  (*TST*)
      Put0(61EH, x.r, 0, x.r); Put0(61EH, y.r, 0, y.r); Put0(CMP, 0, x.r, y.r);  (*MVN, CMP*)*)
      (*Release(y.r)*)
END ;
(*Release(x.r); *)
(*SetCC(x, cond[op - OSAS.eql])*)
SetCC(x, op);
END RealRelation;

PROCEDURE StringRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
VAR r0, r1: LONGINT;  (*x, y are char arrays or strings*)
BEGIN
IF x.type.form = OSAB.String THEN loadString(x) ELSE loadAdr(x) END ;
IF y.type.form = OSAB.String THEN loadString(y) ELSE loadAdr(y) END ;
(*GetReg(r0); Put2(7, r0, x.r, 1);
GetReg(r1); Put2(7, r1, y.r, 1); 
Put0(CMP, 0, r0, r1);
Put5(1, 1);   (* BNE +3 *)
Put1(CMP, 0, r0, 0);
Put5(1, -7);   (* BNE -5 *)
Release(x.r); Release(y.r); Release(r0); Release(r1);*)
(*SetCC(x, cond[op - OSAS.eql])*)
SetCC(x, op);
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
r, rt, k : LONGINT; rname : OSAS.Ident;
BEGIN 
(* intel optimization *)
IF (y.type.form = OSAB.Real) & (y.mode # OSAB.Const) THEN
   Out.String ("PROCEDURE Store"); Out.Ln;
   Out.String ("x.mode = "); Out.Int (x.mode, 0); Out.Ln;
   IF x.mode = OSAB.Par THEN
      GetReg(r); k := x.a - 4; ConvertRegNumToName (r, rname);
      Write ('movl '); Writeint (-k); Write ("(%ebp), "); Writeln (rname);
      Write ('fsts ('); Write(rname); Writeln (')');
      Release(r);
   ELSE
      Write ('fsts '); Writeln (x.label);
   END;
ELSIF y.type.form = OSAB.Proc THEN
   GetReg(r);
   lea(y, r);
   MovRegToVar(r, x);
   Release(r);
ELSIF (y.mode = OSAB.Const) & (x.mode = OSAB.Var) THEN
       MovConstToVar (y.a, x);
ELSE

   IF  (x.type.size = 1) &  (y.mode = OSAB.Const) THEN
          GetReg(r);
         IF x.type.size = 1 THEN 
            Mov8ConstToReg(y.a, r)
         ELSE
            LoadConstToReg(y.a,r)
         END;
         y.mode := OSAB.Reg; y.r := r;
   ELSE
      load(y)
   END;
      IF x.mode = OSAB.Reg THEN (*Out.String ("x.mode = 8"); Out.Ln;*)
         (*cd := code[pc-1];
         IF (y.r < RH) & (y.r >= RL) & (cd DIV 4000000H = -8) & (cd DIV 10H MOD 10H # 9) 
         OR (cd DIV 4000000H = -7) & ODD(cd DIV 100000H) THEN (*fixup*)
            code[pc-1] := cd DIV 10000H * 10000H + x.r * 1000H + cd MOD 1000H
         ELSE Put0(MOV, x.r, 0, y.r)
         END*)
      ELSE
         (*IF x.type.size = 1 THEN cd := STR+4 ELSE cd := STR END ;*)
         IF x.mode = OSAB.Var THEN
	    IF y.mode = OSAB.RegI THEN
	       GetReg(r);
	       MovAdrToReg (y.r, r, x.type.size);
               MovRegToVar (r, x);
	       Release (r)
	    ELSE
               MovRegToVar (y.r, x)
	    END;
         ELSIF x.mode = OSAB.Par THEN
            GetReg(r);
	    LoadParamAdr (EBP, x.a-4, r, x);
	    rt := x.r; x.r := r; Release(r);
            MovRegToAdr (y.r, x);
	    x.r := rt;
         ELSIF x.mode = RegX THEN (*Put3(cd+8, y.r, x.r, x.a, x.b);*) Release(x.a)
         ELSIF x.mode = OSAB.RegI THEN (*array*)
	    IF y.mode = OSAB.RegI THEN
               GetReg(r);
	       IF x.type.size = 1 THEN
	          MovAdrToReg(y.r, r, 1);
		  MovRegToAdr2(r, x.r, 1);
	       ELSE
	          MovAdrToReg(y.r, r, 4);
		  MovRegToAdr2(r, x.r, 4);
	       END;
	       Release(r);
	    ELSE
	    MovRegToAdr(y.r, x);
	    END;
         ELSE OSAS.Mark("bad mode in Store");
         (*Out.String("Store"); Out.Int(x.mode, 4); Out.Int(y.mode, 4);
         Out.Ln*)
         END ;
      (*Release(x.r)*)
      END ;
      Release(y.r); Release (x.r)
END (* for intel *)
END Store;

PROCEDURE CopyString*(VAR x, y: Item);  (* x := y *)
VAR r, i, s, k : LONGINT;
BEGIN 
(*       Out.String ('x.type.len '); Out.Int(x.type.len, 0); Out.Ln;
       Out.String ('x.type.form '); Out.Int(x.type.form, 0); Out.Ln;
       Out.String ('y.type.len '); Out.Int(y.type.len, 0); Out.Ln;
       Out.String ('y.type.form '); Out.Int(y.type.form, 0); Out.Ln;
       Out.String ('y.b '); Out.Int (y.b, 0); Out.Ln;*)
      loadAdr(x);
   loadString(y);
   
   IF y.type.form = OSAB.String THEN
      k := y.type.len
   ELSE 
      k := y.b
   END;
   IF x.type.len = -1 THEN (*open dest array*)
      (*Put2(LDR, r, x.r, x.a+4); Put1(CMP, 0, r, y.b); Put6(11, 6)*)
   ELSIF x.type.len < k THEN 
      OSAS.Mark("string too long")
   END ;
   
   s := k DIV 4;
   GetReg(r);
   FOR i := 1 TO s DO
      MovAdrToReg(y.r, r, 4);
      MovRegToAdr2(r, x.r, 4);
      AddConstToReg(4, x.r);
      AddConstToReg(4, y.r)
   END;
   s := k MOD 4;
   IF s = 0 THEN
      (*IncReg(x.r);*)
      MovConstToAdr(0, x.r, 1)
   ELSE
      FOR i := 1 TO s DO
         MovAdrToReg(y.r, r, 1);
         MovRegToAdr2(r, x.r, 1);
         IF i < s THEN
            IncReg(y.r);
	    IncReg(x.r);
         END
      END;
      s := 4 - s;
      FOR i := 1 TO s DO
         IncReg(x.r);
         MovConstToAdr(0, x.r, 1);
      END;
   END;
(*pc0 := pc; Put2(3, r, y.r, 4); Put2(2, r, x.r, 4); Put1a(1, r, r, 0FFH, 4); Put5(1, pc0 - pc -2);*);

Release(r); Release(y.r); Release(x.r)
END CopyString;

PROCEDURE CopyRecord*(VAR x, y: Item);  (* x := y *)
VAR cnt, data, s, i, r: LONGINT; pc0: INTEGER; xL, yL: Item;
BEGIN
   s := x.type.size DIV 4;
   IF s > 0 THEN
      IF x.mode = OSAB.Par THEN
         GetReg(r);
	 MovVarToReg(x, r);
	 x.r := r;
      ELSIF x.mode = OSAB.Var THEN
         IF x.r = EBP THEN
	    loadAdr(x);
	 ELSE
	    GetReg(r); (*nottested*)
	    MovVarToReg(x, r);
	    x.r := r;
	    x.mode := OSAB.Reg; 
	 END
      END;
      IF y.mode = OSAB.Par THEN
         GetReg(r);
	 MovVarToReg(y, r);
	 y.r := r;
      ELSIF y.mode = OSAB.Var THEN
         IF y.r = EBP THEN
	    loadAdr(y);
	 ELSE
	    GetReg(r); (* nottested*)
	    MovVarToReg(y, r);
	    x.r := r;       (*nottested*)
	    x.mode := OSAB.Reg; (*nottested *)
	 END
      END;
(*
      loadAdr(x);
      loadAdr(y);*)
      (*GetReg(cnt);*)
      GetReg(data);
(*IF s < C8 THEN Put1(MOV, cnt, 0, s) ELSE enterIC(s); Put2(LDR, cnt, PC, 0) END ;
pc0 := pc; Put2(3, data, y.r, 4); Put2(2, data, x.r, 4); Put1(SUB+1, cnt, cnt, 1); Put5(1, pc0 - pc -2);*)
      (*MovConvstToReg(s, cnt);*)
      FOR i := 1 TO s DO
        MovAdrToReg(y.r, data, 4);
	MovRegToAdr2(data, x.r, 4);
	AddConstToReg (4, y.r);
	AddConstToReg (4, x.r);
      END;
      s := x.type.size MOD 4;
      FOR i := 1 TO s DO
         MovAdrToReg(y.r, data, 1);
	 MovRegToAdr2(data, x.r, 1);
	 IF i < s THEN
	    IncReg(y.r);
	    IncReg(x.r);
	 END
      END;
      (*
      MovConstToReg(s, cnt);
      
      SubRegWithConst (1, cnt);
      *)
      (*Release(cnt);*)
      Release(data);
      Release(x.r);
      Release(y.r)
   END
END CopyRecord;

PROCEDURE CopyArray*(VAR x, y: Item);  (* x := y *)
VAR cnt, data, s, i, r : LONGINT; pc0: INTEGER; xL, yL: Item;
BEGIN
(*CopyRecord(x, y);*)
   s := x.type.size DIV 4;
   IF s > 0 THEN
      IF x.mode = OSAB.Par THEN
         GetReg(r);
	 MovVarToReg(x, r);
	 x.r := r;
      ELSIF x.mode = OSAB.Var THEN
         IF x.r = EBP THEN
	    loadAdr(x);
	 ELSE
	    GetReg(r);   (*nottested*)
	    MovVarToReg(x, r);
	    x.r := r; x.mode := OSAB.Reg;  (* nottested*)
	 END
      END;
      IF y.mode = OSAB.Par THEN
         GetReg(r);
	 MovVarToReg(y, r);
	 y.r := r;
      ELSIF y.mode = OSAB.Var THEN
         IF y.r = EBP THEN
	    loadAdr(y);
	 ELSE
	    GetReg(r); (*nottested*)
	    MovVarToReg(y, r);
	    x.r := r;  x.mode := OSAB.Reg; (*nottested*)
	 END
      END;
      (*GetReg(cnt);*)
      GetReg(data);
(*IF s < C8 THEN Put1(MOV, cnt, 0, s) ELSE enterIC(s); Put2(LDR, cnt, PC, 0) END ;
pc0 := pc; Put2(3, data, y.r, 4); Put2(2, data, x.r, 4); Put1(SUB+1, cnt, cnt, 1); Put5(1, pc0 - pc -2);*)
      (*MovConvstToReg(s, cnt);*)
      FOR i := 1 TO s DO
        MovAdrToReg(y.r, data, 4);
	MovRegToAdr2(data, x.r, 4);
	AddConstToReg (4, y.r);
	AddConstToReg (4, x.r);
      END;
      s := x.type.size MOD 4;
      FOR i := 1 TO s DO
         MovAdrToReg(y.r, data, 1);
	 MovRegToAdr2(data, x.r, 1);
	 IF i < s THEN
	    IncReg(y.r);
	    IncReg(x.r);
	 END
      END;
      (*
      MovConstToReg(s, cnt);
      
      SubRegWithConst (1, cnt);
      *)
      (*Release(cnt);*)
      Release(data);
      Release(x.r);
      Release(y.r)
   END

(*   IF (x.type.form = OSAB.Array) & (x.type.len >= 0) THEN 
      MakeConstItem(xL, OSAB.intType, x.type.len)
   ELSE
      xL.mode := OSAB.Var;
      xL.type := OSAB.intType;
      xL.r := EBP;
      xL.a := x.a - 4
   END ;
   loadAdr(x);
   load(xL);
   IF (y.type.form = OSAB.Array) & (y.type.len >= 0) THEN
      MakeConstItem(yL, OSAB.intType, y.type.len)
   ELSE 
      yL.mode := OSAB.Var;
      yL.type := OSAB.intType;
      yL.r := EBP;
      yL.a := y.a - 4
   END ;
   loadAdr(y);
   load(yL);
   (*Put0(CMP, 0, xL.r, yL.r); Put6(11, 3);*)  (*length check*)
   (*determine word count*)
   cnt := yL.r;
   data := xL.r;  (*convert length of y to nofwords, size is in byte, len always a multiple of 4*)
   IF y.type.base.size < 4 THEN
      (*Put0a(MOV, cnt, 0, cnt, 1, 2)*)  (* /4, byte array*)
   ELSIF y.type.base.size > 4 THEN (*wordcnt = len * elementsize/4*)
      (*Put1a(MOV, xL.r, 0, y.type.base.size, 1); Put0c(0, cnt, cnt, xL.r, 0)*)
   END ;
   pc0 := pc;
   (*Put2(3, data, y.r, 4); Put2(2, data, x.r, 4); Put1(SUB+1, cnt, cnt, 1); Put5(1, pc0 - pc -2);*)
   Release(xL.r); Release(yL.r); Release(x.r); Release(y.r)
   *)
END CopyArray;

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
   para[paraindex].rdo := TRUE; (* marking x.r with 6 to recognize during call to push address, not value *)
END VarParam;

PROCEDURE ValueParam*(VAR x: Item);
VAR r: LONGINT;
BEGIN
   IF (x.mode = OSAB.Reg) & ((x.r < RL-1) OR (x.r >= RH)) THEN
      (*GetReg(r); *)
      (*Put0(MOV, r, 0, x.r)*)
      (*register variable*)
      INC(paraindex);
      para[paraindex] := x;
   ELSE 
   INC(paraindex);
   para[paraindex] := x;
   (*load(x)*)
   END ;
END ValueParam;

PROCEDURE StringParam*(VAR x: Item);
VAR r: LONGINT;
BEGIN (*string constant?*)
loadString(x);
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
(*IF code[at] = 0 THEN code[at] := -369098752 (*0EA000000H*) + pc - at -2  (*BR*)
ELSE OSAS.Mark("multiple def of label")
END*)
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
   IF x.mode = OSAB.Reg THEN 
      Store(x, y); (* not tested *)
      y.r := x.r 
   ELSE 
      load(y);
      MovRegToVar (y.r, x);
   END
END For0;

PROCEDURE For1*(VAR x, y, z, w: Item; VAR L: LONGINT);
VAR r: LONGINT;
    lab, num : OSAS.Ident;
BEGIN 
   r := y.r; 
   load (z);
   CmpInRegs (z.r, y.r);
   lab := '_';
   Strings.Append (modname, lab);
   L := labelnum;
   INC (labelnum);
   Strings.Append ('_', lab);
   Strings.Append (labelnumber, lab);
   IntStr.IntToStr(labelnum, num);
   Strings.Append (num, lab);
   jg (lab);
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
   Release(x.r);
IF (n >= 0) & (n < 100H) THEN (*Put1(ADD, x.r, x.r, n)*) 
   AddConstToReg (n, x.r);
ELSIF (n < 0) & (n >= -1000H) THEN (* to be changed to max longint *)
	SubRegWithConst (-n, x.r);
ELSE OSAS.Mark("step too large")
END;
MovRegToVar (x.r, x);
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
(*L := pc-1*)
(*Put5( negated(x.r), x.a); FixLink(x.b);*) x.a := pc-1;
lab := '_';
   Strings.Append (modname, lab);
   Strings.Append ('_', lab);
   Strings.Append (labelmark, lab);
   IntStr.IntToStr (x.jnum, num);
   Strings.Append (num, lab);
   COPY (lab, x.jlab);
CASE x.r OF
    0: (* = *) je(lab);
 |  1: (* # *) jne(lab);
 |  2: (* < *) jl(lab);
 |  3: (* <= *) jle(lab);
 |  4: (* > *) jg(lab);
 |  5: (* >= *) jge(lab)
 (* or *)
 |  9: (* =*) je(lab);
 | 10: (* #*) jne(lab);
 | 11: (* <*) jl(lab);
 | 12: (* <= *) jle(lab);
 | 13: (* > *) jg(lab);
 | 14: (* >=*) jge(lab);
END;
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

PROCEDURE SetAndLab(VAR y : Item);
VAR 
   num, lab : OSAS.Ident;
   BEGIN
      IntStr.IntToStr (y.jnum, num);
      COPY (labelmark, lab);
      Strings.Append (num, lab);
      Write ('_'); Write (modname); Write ('_');   Write (lab); Writeln (":");
END SetAndLab;


PROCEDURE CFJump*(VAR x: Item);
VAR lab, num : OSAS.Ident;
reg : LONGINT;
regname : OSAS.Ident;
BEGIN Out.String ("CFJump"); Out.Ln; debug(x); Out.String ('x.type.form '); Out.Int (x.type.form, 0); Out.Ln;
IF x.mode # CC THEN loadCC(x) END ;
(*Put5( negated(x.r), x.a); FixLink(x.b);*) x.a := pc-1;
(*
	CASE x.r OF 
    0: (* = *) Write ("jne ");
 |  1: (* # *) Write ("je ");
 |  2: (* < *) Write ("jge ");
 |  3: (* <= *) Write ("jg ");
 |  4: (* > *) Write ("jle ");
 |  5: (* >= *)Write ("jl ");
 (* or *) (* stegh kartses takiny petqa pakel *)
 |  9: (* =*) Write ("jne ");
 | 10: (* #*) Write ("je ");
 | 11: (* <*) Write ("jge ");
 | 12: (* <= *) Write ("jg ");
 | 13: (* > *) Write ("jle ");
 | 14: (* >=*) Write ("jl ");
 | 15: (* in *) IF x.not = FALSE THEN Write ("jnc ") ELSE Write ("jc ") END;
END;*)
(*Write ('jf (');*)
   GetReg(reg);
   Test (reg, reg);
   Release(reg);
   lab := '_';
   Strings.Append (modname, lab);
   Strings.Append ('_', lab);
   Strings.Append (labelmark, lab);
   IntStr.IntToStr(x.jnum, num);
   Strings.Append (num, lab);
   je(lab);
   COPY (lab, x.jlab);
END CFJump;

PROCEDURE wCFJump*(VAR x: Item);
VAR lab, num : OSAS.Ident;
reg : LONGINT;
regname : OSAS.Ident;
BEGIN Out.String ("wCFJump"); Out.Ln;
   IF x.mode # CC THEN loadCC(x) END ;
      (*GetReg(reg);
      Test(reg, reg);
      Release (reg);*)
      Test(x.r, x.r);
      Release(x.r);
      lab := '_';
      Strings.Append (modname, lab);
      Strings.Append ('_', lab);
      Strings.Append (labelnumber, lab);
      INC(labelnum);
      IntStr.IntToStr(labelnum, num);
      Strings.Append (num, lab);
      je(lab);
END wCFJump;

PROCEDURE Jump* (j : LONGINT);
VAR num, lab : OSAS.Ident;
BEGIN
   lab := '_';
   Strings.Append (modname, lab);
   Strings.Append ('_', lab);
   Strings.Append (labelendmark, lab);
   IntStr.IntToStr(j, num);
   Strings.Append (num, lab);
   jmp (lab);
END Jump;

PROCEDURE BJump*(L: LONGINT);
VAR lab, num : OSAS.Ident;
BEGIN
   lab := '_';
   Strings.Append (modname, lab);
   Strings.Append ('_', lab);
   Strings.Append (labelnumber, lab);
   IntStr.IntToStr (L+1, num);
   Strings.Append (num, lab);
   jmp (lab);
END BJump;

PROCEDURE CBJump*(VAR x: Item; L: LONGINT);
VAR reg : LONGINT; num, lab : OSAS.Ident;
BEGIN
      IF x.mode # CC THEN loadCC(x) END ;
      GetReg(reg);
      Test(reg, reg);
      Release (reg);
      
      lab := '_';
      Strings.Append (modname, lab);
      Strings.Append ('_', lab);
      Strings.Append (labelnumber, lab);
      IntStr.IntToStr(L+1, num);
      Strings.Append (num, lab);
      je(lab);
END CBJump;

PROCEDURE Fixup*(VAR x: Item);
BEGIN
   Write ('_'); Write (modname); Write ('_'); Write (labelnumber); Writeint (x.a); Writeln (':');
END Fixup;


PROCEDURE Call*(VAR x: Item; rs: SET);
VAR pc0, r, i: LONGINT;
k,j : INTEGER;
s, rname : OSAS.Ident; o : OSAB.Object; oindex : INTEGER;
oarr : ARRAY 30 OF OSAB.Object;
BEGIN
RL := 0;
   IF x.type.form = OSAB.Proc THEN
      IF x.mode IN {OSAB.Const, OSAB.Var} THEN
         (*IF x.b >= 0 THEN*)
	    IF paraindex > -1(*x.type.nofpar # 0*) THEN
	       k :=paraindex;
               REPEAT
		  (*Out.String ('PARAMETER'); Out.Ln;
                  debug(para[k]);
		      Out.String ('para.type.form '); Out.Int ( para[k].type.form, 0); Out.Ln;*)
		  IF para[k].mode = OSAB.Const THEN
        	     IF (para[k].type.form = OSAB.Array) THEN
                           GetReg(r);
			   lea (para[k], r);
			   PushConst(para[k].type.len); (* array length *)
			   Push(r);
			   Release (r);
	             ELSIF para[k].type.form = OSAB.String THEN
                        GetReg(r);
		        lea(para[k], r);
		        Push(para[k].type.len); (* array length *)
			Push(para[k].r);
		        Release(para[k].r);
		     ELSIF para[k].type.form = OSAB.Char THEN
			PushConst(*8*)(para[k].a)
			(*s[0] := CHR(para[k].a); s[1] := 0X;*)
		     ELSIF para[k].type.form = OSAB.Int THEN
		        PushConst(para[k].a)
	             ELSIF para[k].type.form = OSAB.Real THEN
		        PushConst(para[k].a)
		     END;
		  ELSIF para[k].mode = OSAB.Var THEN              Out.String ("para[k].mode=OSAB.Var"); Out.Ln;
		                                                  Out.String ("para[k].r="); Out.Int(para[k].r,0); Out.Ln;
								  Out.String ("para[k].a="); Out.Int(para[k].a,0); Out.Ln;
								  Out.String ("para[k].b="); Out.Int(para[k].b,0); Out.Ln;
								  Out.String("para[k].type.form=");Out.Int(para[k].type.form,0);Out.Ln;
								  Out.String ("para[k].label=");Out.String(para[k].label);Out.Ln;
								  Out.String ("para[k].ilabel=");Out.String(para[k].ilabel);Out.Ln;
		     IF (para[k].r = EBP) THEN
		        GetReg(r);  (* stegh *)
			IF para[k].type.form IN {OSAB.Array, OSAB.String} THEN
			   Write ("pushl "); Write ("4+"); Writeln (para[k].label); (* array length *)
			   (*PushConst(para[k].type.len);*)
			   lea(para[k], r); (* TOIMPROVE it is not necessary to load parameter to the register to be able to push it, it could be done from memory *)
			ELSIF para[k].type.form IN {OSAB.Record} THEN
			   lea(para[k], r);
			   Push(r); (* TOIMPROVE it should be removed after, because it is not necessary to pass record type descriptor *)
			ELSIF para[k].rdo THEN
			   lea(para[k], r)
		        ELSE
			   MovVarToReg(para[k], r);
			END;
		        Push(r);
			Release(r);
	             ELSIF para[k].type.form IN {OSAB.Char, OSAB.Int, OSAB.Bool} THEN
		        IF para[k].rdo THEN
			   GetReg(r);
			   lea(para[k], r);
			   Push(r);
			   Release(r);
			ELSE
			   PushMem(para[k])
			END;
		     ELSIF para[k].type.form IN {OSAB.String, OSAB.Array} THEN
		        GetReg(r);
			PushConst(para[k].type.len); (* array length *)
			lea(para[k],r);
			(*Push(r);*)
                        Push(r);
                        Release(r);
	    	     ELSIF para[k].type.form IN {OSAB.Record} THEN
		        lea(para[k],r);
			Push(r);
			Push(r);
			Release(r);
	             ELSIF para[k].type.form = OSAB.Pointer THEN
		        PushMem(para[k]);
		     ELSIF para[k].type.form = OSAB.Real THEN
		        IF para[k].rdo = TRUE THEN 
			   Out.String ("para[k].rdo = TRUE");
			   GetReg(r);
			   lea(para[k], r);
			   Push(r);
			   Release(r);
			ELSIF para[k].r = PC (*15 memory *) THEN 
			   Out.String ("para[k].rdo = FALSE");
			   PushMem(para[k]);
			   (*MovVarToReg(para[k], r); *)
			ELSE Out.String ("chnakhatsnvats real call"); Out.Ln
			END; 
			   Out.Ln;
			   (*Push(r);*)
			   (*Release(r);*)
		     END
		  ELSIF para[k].mode = OSAB.RegI THEN
		     IF para[k].type.size = 1 THEN
		     MovAdrToReg(para[k].r, para[k].r, 1);
		     Push8(para[k].r)
		     ELSE
		     MovAdrToReg(para[k].r, para[k].r, 4);
		     Push(para[k].r)
		     END;
		   ELSIF para[k].mode = OSAB.Reg THEN
                     IF para[k].type.form = OSAB.String THEN
			(*lea(para[k], para[k].r);*)
		        Push(para[k].r);
			Push(para[k].r); (* instead of type descriptor *)
		        Release(para[k].r);
		     ELSIF para[k].type.form = OSAB.Real THEN
		        IF para[k].rdo THEN
                          Out.String ("temporary variable cannot be passed to the procedure by reference"); Out.Ln; HALT(0);
			ELSE
			  (*GetReg(r); ConvertRegNumToName (r, rname); Release(r);*)
                          Writeln ("subl $4, %esp");
			  (*Writeln ('fsts (%esp)');*) (* instruction below will cleanup fp stack *)
			  Writeln ('fstpl (%esp)');

			  (* Writeln (rname);*)
                          (*Writeln ("subl $4, %esp");*)
			  (*Push(r);*)
			END
		     ELSE
		        IF para[k].rdo THEN
			   GetReg(r);
			   lea (para[k], r);
                           Push(para[k].r);
			   Release(r)
			ELSE
			   Push(para[k].r)
			END
		     END;
		  ELSIF para[k].mode = OSAB.Par THEN
		     IF para[k].type.form IN {OSAB.Array, OSAB.String, OSAB.Record} THEN
                        GetReg(r);
			IF para[k].a < 0 THEN
			   i := para[k].a - 4
			ELSIF para[k].a > 0 THEN
			   i := para[k].a
			ELSE Out.String('para[k] is 0'); Out.Ln; HALT(0);
			END;
			(*lea (para[k], r);*)
			IF para[k].type.form IN {OSAB.Array, OSAB.String} THEN
			   Write ("pushl "); Write("4+"); Writeln(para[k].label);
			   LoadParamToReg (EBP, i, r, para[k]);
                           Push(r);
			ELSIF para[k].type.form IN {OSAB.Record} THEN
			   LoadParamToReg (EBP, i, r, para[k]);
			   (*MovVarToReg(para[k], r);*)
                           Push(r); Push(r);
			END;
			Release(r);
		     ELSIF para[k].type.form IN {OSAB.Int, OSAB.Char, OSAB.Bool} THEN
                        GetReg(r);
			MovVarToReg(para[k], r);
			Push(r);
			Release(r);
		     ELSIF para[k].type.form IN {OSAB.Real} THEN
                        GetReg(r);
			MovVarToReg(para[k], r);
			Push(r);
			Release(r)
		     END;
		  END;
		  DEC(k)
	       UNTIL k < 0;
	    END;
	 (*Put5a(x.a-pc-2)*)
         (*ELSE*)
	 (*imported*) (*Put5a(x.a*10000H + fixlist[-x.b]); fixlist[-x.b] := pc-1*)
            (*Out.String ('imported function'); Out.Ln;
	    debug(x); 
	    Out.String ('x.type.dsc'); Out.Ln;
	    DebugObj(x.type.dsc);
	    Out.String ('x.type.dsc.next'); Out.Ln;
	    DebugObj(x.type.dsc.next); Out.Ln;
	    DebugObj(x.type.dsc.next.next); Out.Ln; Out.Ln; Out.Ln;
	    DebugObj(x.type.typobj); Out.Ln;
	    DebugObj(x.type.typobj.next); Out.Ln;
	    HALT(0);*)
	    (*
	    oindex := -1;
	  IF x.type.dsc # NIL THEN
	    o := x.type.dsc;
	    REPEAT
            IF (o # NIL) & (o.name # x.name) THEN
	    INC(oindex);
	       oarr[oindex] := o;
	       IF o.next # NIL THEN
	          o := o.next
	       END
	    END;
	    UNTIL (o.next = NIL) OR (o.name = x.name);
	  END;
	  IF oindex > -1 THEN
	   k := oindex; 
	   REPEAT
	    o := oarr[k];
	    IF o.class IN {OSAB.Par, OSAB.RegI} THEN
               IF o.type.form IN {OSAB.Char, OSAB.Var} THEN
               
	       END;
	    ELSE

	    END;
	   UNTIL k < 0;
	  END;
	 END*)
         (*   IF paraindex > -1(*x.type.nofpar # 0*) THEN
	       k :=paraindex;
               REPEAT
		  Out.String ('PARAMETER'); Out.Ln;
                  debug(para[k]);
		      Out.String ('para.type.form '); Out.Int ( para[k].type.form, 0); Out.Ln;
		  IF para[k].mode = OSAB.Const THEN
        	     IF (para[k].type.form = OSAB.Array) THEN
                           GetReg(r);
			   lea (para[k], r);
			   Push(r);
			   Push(r); (* instead of type descriptor *)
			   Release (r);
	             ELSIF para[k].type.form = OSAB.String THEN
                        GetReg(r);
		        lea(para[k], r);
		        Push(para[k].r);
			Push(para[k].r); (* instead of type descriptor *)
		        Release(para[k].r);
	             ELSIF para[k].type.form = OSAB.Char THEN
			PushConst(*8*)(para[k].a)
			(*s[0] := CHR(para[k].a); s[1] := 0X;*)
		     ELSIF para[k].type.form = OSAB.Int THEN
		        PushConst(para[k].a)
		     END;
		  ELSIF para[k].mode = OSAB.Var THEN
		     IF (para[k].r = EBP) THEN
		        GetReg(r);  (* stegh *)
			IF para[k].type.form IN {OSAB.Array, OSAB.String} THEN
			   lea(para[k], r);
		           (*MovVarToReg(para[k], r);*)
		           Push(r);
			   Push(r);
			ELSIF para[k].type.form IN {OSAB.Char, OSAB.Int, OSAB.Bool} THEN
			   MovVarToReg(para[k], r);
			   Push(r)
			END;
			Release(r);
	             ELSIF para[k].type.form IN {OSAB.Char, OSAB.Int, OSAB.Bool} THEN
		        PushMem(para[k])
		     END
		  ELSIF para[k].mode = OSAB.Reg THEN
                     IF para[k].type.form = OSAB.String THEN
			(*lea(para[k], para[k].r);*)
		        Push(para[k].r);
			Push(para[k].r); (* instead of type descriptor *)
		        Release(para[k].r);
		     END;
		  ELSIF para[k].mode = OSAB.Par THEN
		     IF para[k].type.form IN {OSAB.Array, OSAB.String} THEN
                        GetReg(r);
			IF para[k].a < 0 THEN
			   i := para[k].a - 4
			ELSIF para[k].a > 0 THEN
			   i := para[k].a
			END;
			LoadParamToReg (EBP, i, r, para[k]);
			(*lea(para[k], r);*)
			(*MovVarToReg(para[k], r);*)
                        Push(r); Push(r);
			Release(r);
		     ELSIF para[k].type.form IN {OSAB.Int, OSAB.Char, OSAB.Bool} THEN
                        GetReg(r);
			MovVarToReg(para[k], r);
			Push(r);
			Release(r);
		     ELSIF para[k].type.form IN {OSAB.Record} THEN

		     END;
		  END;
		  DEC(k)
	       UNTIL k < 0;
	    END;
          END;*)
      ELSE 
        (*Put0(MOV, LNK, 0, PC); Put0(MOV, PC, 0, x.r);*)
	 (*Release(x.r)*)
      END ;
         Write ('call '); 
	 IF x.mode = OSAB.Const THEN
	    Writeln (x.label);
	 ELSIF x.mode = OSAB.Var THEN
            Write ('*'); Writeln (x.label);
	 END;
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
   END;
   paraindex := -1;
END Call;

PROCEDURE Header*;
VAR i: INTEGER;
BEGIN (*entry := pc; Put4(18, SP, {FP, LNK}); (*STM*)
Put0(MOV, FP, 0, SP)*)  (*FP := SP*)
END Header;

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
(*Out.String ("obj"); Out.Ln; Out.Ln;
DebugObj(obj); Out.Ln;
Out.String ("obj.type.dsc"); Out.Ln; Out.Ln;
DebugObj(obj.type.dsc); Out.Ln;
(*Out.String ("obj.type.typobj"); Out.Ln; Out.Ln;
DebugObj(obj.type.typobj); Out.Ln;*)
Out.String ("obj.type.dsc.next"); Out.Ln; Out.Ln;
DebugObj(obj.type.dsc.next); Out.Ln;*)
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


PROCEDURE Procedure* (VAR proc : OSAB.Object);
VAR t : OSAB.Object;
k : LONGINT; ch : CHAR;
BEGIN

Writeln (".section .text");
proc.label := modname;
Strings.Append ("_", proc.label);
Strings.Append (proc.name, proc.label);
IF proc.expo THEN
   Write (".globl "); Writeln(proc.label)
END;
Write (".type ");
Write (proc.label); Write (', '); Writeln ('@function');
Write (proc.label); Writeln (":");
	 Push(EBP);
	 Movl (ESP, EBP);
t := proc.type.dsc;
k := 0;      (* counting variables size *) (* soschital ne to, soschital parametry, nujno soschitat variables *)
   REPEAT 
      k := k + t.type.size;
      t := t.next;
   UNTIL t.val >= 0;
       SubRegWithConst (k, ESP);

(*DebugObj(proc); Out.String ('a seychas'); 
DebugObj(proc.next);
DebugObj(proc.next.next);
DebugObj(proc.next.next.next);*)
(*Out.Ln; DebugObj(proc.type.dsc); Out.String('aa'); Out.Ln; *)
t := proc.type.dsc;
(*DebugObj(t.next);*)t := t.next;
(*Out.Ln; DebugObj(t.next);*) t := t.next;
(*Out.Ln; DebugObj(t.next);*)
END Procedure;

(*
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
*)
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

PROCEDURE Enter*(VAR proc : OSAB.Object; leaf, int: BOOLEAN; level, regvarno: INTEGER; parsize, varsize: LONGINT);
VAR n, nofregs: LONGINT;
BEGIN
   curlev := level; RL := regvarno; RH := EDX(*FP*); nofregs := parsize DIV 4;
   IF ~int THEN (*procedure prolog*)
      IF ~leaf THEN
	 Writeln (".section .text");
         IF proc.expo THEN
	    Write (".globl "); Writeln (proc.label);
	 END;
         Write (".type ");
         (*proc.label := modname;
         Strings.Append ("_", proc.label);
         IF level > 1 THEN
	    IF proc.type.dsc # NIL THEN
               Strings.Append (proc.type.dsc.name, proc.label);
	       Strings.Append ("_", proc.label);
	    END
	 END;
	 Strings.Append (proc.name, proc.label);*)
         Write (proc.label); Write (', '); Writeln ('@function');
         Write (proc.label); Writeln (":");
	 Push(EBP);
	 Movl (ESP, EBP);
         (*Put4(18, SP, {FP-nofregs .. FP, LNK});*)  (*save parameters, FP, LNK*)
         (*Put1(ADD, FP, SP, nofregs*4)*)    (*FP := SP + parsize*)
      ELSE
         (*Put4(18, SP, {FP, LNK}); Put0(MOV, FP, 0, SP);*) RH := EDX - nofregs (*FP - nofregs*)
      END
   ELSE (*interrupt procedure*)
      IF ~leaf THEN (*normal interrupt handler prolog*)
         (*Put4(18, SP, {0 .. FP, LNK});*)  (*save R0 - R11, FP, LNK*)
         (*Put0(MOV, FP, 0, SP); RL := 0*)   (*FP := SP; no code for fast interrupt*)
      END
   END ;

   IF varsize > 0 THEN  
       SubRegWithConst (varsize, ESP);

      (*SP := SP - varsize*)
      (*Put1(SUB, SP, SP, varsize MOD C8); varsize := varsize DIV C8;*)
      IF varsize > 0 THEN
         (*t1a(SUB, SP, SP, varsize, 12) *)
      END
   END;
   (*DebugObj(proc); Out.Ln; Out.Ln;
   DebugObj(proc.type.dsc); Out.Ln;
   DebugObj(proc.type.dsc.next); Out.Ln;
   DebugObj(proc.type.dsc.next.next); Out.Ln;
   DebugObj(proc.type.dsc.next.next.next); Out.Ln;
   DebugObj(proc.type.dsc.next.next.next.next); Out.Ln*)
END Enter;

PROCEDURE Return*(leaf, int: BOOLEAN; offset, resreg, form: INTEGER; VAR x: Item);
VAR res: Item; xname : OSAS.Ident;
BEGIN
resreg :=EAX;(* FP -resreg - 1;*)
IF x.mode = OSAB.Reg THEN
Movl(x.r, resreg); Release(x.r);
ELSIF (form # OSAB.NoTyp) & ~((x.mode = OSAB.Reg) & (x.r = resreg)) THEN
   res.mode := OSAB.Reg; res.r := resreg; res.type := x.type;
   MovVarToReg (x, R0);
END ;

IF ~int THEN (*procedure epilog*)
(*Put0(MOV, SP, 0, FP);   (*SP := FP*)
Put4(11, SP, {FP, PC})*)    (*restore FP, PC*)
Movl (EBP, ESP);
Pop (EBP);
Ret;
END;(*
ELSE
IF ~leaf THEN (*normal interupt handler epilog*)
Put0(MOV, SP, 0, FP); Put4(11, SP, {0 .. FP, LNK});  (*restore R0 - R11, FP, LNK*)
END ;
Put1(SUB+1, PC, LNK, offset)    (*PC := LNK - offset*)
END ;*)
RL := 0;  RH := EDX(*FP*); regs := {}; (*FixupConstants*)
END Return;

PROCEDURE Import*(module : OSAS.Ident);
BEGIN
INC(impind);
imports[impind] := module;
END Import;

(* In-line code procedures*)

PROCEDURE Increment*(upordown: LONGINT; VAR x, y: Item);
VAR op, inc, r, r1, l: LONGINT; rname : OSAS.Ident;
BEGIN
      debug(x); debug(y);
      IF upordown = 0 THEN op := ADD ELSE op := SUB END ;
      IF y.type.form = OSAB.NoTyp THEN inc := 1
      ELSE (*integer constant*) inc := y.a;
         IF (inc < 0) OR (inc >= (*100*)10000H) THEN OSAS.Mark("bad increment") END
      END ;
      IF x.mode = OSAB.Reg THEN (*Put1(op, x.r, x.r, inc)*)
         IF upordown # 0 THEN
	    FOR l := 1 TO inc DO
	     IncReg(x.r)
	    END
	 ELSE
	    FOR l := 1 TO inc DO
	    DecReg(x.r)
	    END
	 END;
      ELSE 
         GetReg(r); 
	 ConvertRegNumToName (r, rname);
         IF (x.mode = OSAB.Var) OR (x.mode = OSAB.RegI) THEN
            (*Put2(LDR, r, x.r, x.a); Put1(op, r, r, inc); Put2(STR, r, x.r, x.a)*)
	    IF upordown = 0 THEN
	       FOR l := 1 TO inc DO
	       IncVar(x);
	       END
	    ELSE
	       FOR l := 1 TO inc DO
	       DecVar(x)
	       END
	    END;
         ELSIF x.mode = OSAB.Par THEN
            (*Put2(LDR, r, x.r, x.a); GetReg(r1); Put2(LDR, r1, r, x.b);
            Put1(op, r1, r1, inc); Put2(STR, r1, r , x.b); Release(r1)*)
	     IF upordown = 0 THEN 
	        FOR l := 1 TO inc DO
		IncVar(x) 
		END
             ELSE
	        FOR l := 1 TO inc DO
		DecVar(x) 
		END
	     END;
         ELSIF x.mode = RegX THEN
           (*Put3(25, r, x.r, x.a, x.b); Put1(op, r, r, inc); Put3(24, r, x.r, x.a, x.b);*)
           IF upordown = 0 THEN 
	      FOR l := 1 TO inc DO
	      IncVar(x) 
	      END
	   ELSE 
	      FOR l := 1 TO inc DO
	      DecVar(x) 
	      END
	   END;
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
VAR T, z: Item; r, base, s: LONGINT; str, rname : OSAS.Ident;
BEGIN (*Out.Ln; Out.Ln; debug(x); Out.Ln; debug(y); Out.Ln; 
      DebugObj(x.type.base.typobj); Out.Ln;*)
      str := x.type.base.typobj.name;
   (*loadAdr(x); *)
      IF x.type.base = NIL THEN 
      OSAS.Mark("no pointer base")
   ELSE 
      MakeItem(T, x.type.base.typobj); 
      T.mode := OSAB.Var; 
      s := x.type.base.typobj.type.size;
      Push(0);
      Push(1); regs := regs + {0, 1};
      (* zdes nujno alloc v heap *)
      PutMemallo(s+4);
       (* a teper prisvoit pointer x-u *)
       MovRegToVar(0, x);
       (* teper sdelat chtoby on ukazyval na type descriptor *)
       GetReg(r);
       Write ('leal ');
       (* if record from this module *)
       Write (modname); Write ('_'); Write(str); Write('_'); Write('typedsc'); Write ('_');
       ConvertRegNumToName (r, rname);
       Write (', '); Writeln (rname);
       Write ('movl '); Write (rname);
       Release(r);
       Writeln (', (%eax)');
       x.mode := OSAB.Typ;
       x.type.form := OSAB.Pointer;
       Pop(1);
       Pop(0);
       regs := regs - {0, 1}
      (*loadAdr(T); 
      debug(T); HALT(0);*)
      (*Release(T.r)*)
   END ;
(*Put5a(fixlistMU + 20000H); fixlistMU := pc - 1;*)
   Release(x.r)
END New;

PROCEDURE Dispose* (VAR x : Item);
VAR s : LONGINT;
BEGIN
debug(x); DebugObj(x.type.base.typobj);
s := x.type.base.typobj.type.size;
Push(0); Push(1); Push(2);
MovConstToReg(91, 0);
MovVarToReg(x, 1);
(*MovConstToReg(s+4, 2);*)
MovAdrToReg(1, 2, 4);
MovAdrToReg(2, 2, 4);
Writeln ('int $0x80');
Pop(2); Pop(1); Pop(0)
END Dispose;

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
VAR r, cnt: LONGINT; a, d : BOOLEAN;
BEGIN
IF x.type.form = OSAB.Int THEN
      a := FALSE;
   IF EAX IN regs THEN
      a := TRUE;
      Push(EAX);
   END;
   regs := regs + {EAX}; (* was it free, if not save it *)
   IF x.mode = OSAB.Var THEN
      MovVarToReg (x, R0);
   END;
   IF x.mode = OSAB.Const THEN
      MovConstToReg(x.a, R0);
   END;
   IF x.mode = OSAB.Reg THEN 
      Movl (x.r, R0);
      Release (x.r);
   END;
   d := FALSE;
   IF EDX IN regs THEN
      Push(EDX);
      d := TRUE;
   END;
   regs := regs + {EDX};
   
   Writeln ("cdq");
   Writeln ("xor %edx, %eax");
   Writeln ("sub %edx, %eax");
   
   IF x.mode = OSAB.Reg THEN Movl(EAX, x.r) END;
   regs := regs - {EAX} + {x.r};
   regs := regs - {EDX};
   
   x.r := EAX;
   x.mode := OSAB.Reg;

   IF d THEN
      Pop(EDX);
      regs := regs + {EDX}
   END;
   IF a THEN
      Pop(EAX);
      regs := regs + {EAX}
   END;

   

      (*IF (x.mode = OSAB.Reg) & ((x.r < RL) OR (x.r >= RH)) THEN (*reg var*)
         GetReg(r); 
         Put0(MOV+1, r, 0, x.r)
      ELSE 
         load(x); 
         r := x.r; 
         Put1(CMP, 0, r, 0)
      END ;
      Put1(RSB+0D00H, r, r, 0);
      x.r := r*)
ELSIF x.type.form = OSAB.Real THEN 
     (* load(x);
      r := x.r;
      Put1a(28, r, x.r, 2, 1);
      x.r := r *) (*BIC sign*)
ELSE (*Set*)
     (*IF (x.mode = OSAB.Reg) & ((x.r < RL) OR (x.r >= RH)) THEN (*reg var*)
         GetReg(r); Put0(MOV, r, 0, x.r)
      ELSE 
         load(x); r := x.r
      END ;
   GetReg(cnt); 
   Put1(MOV, cnt, 0, 0);
   Put0a(MOV+1, r, 0, r, 1, 1); Put1(ADD+2, cnt, cnt, 0);  (*LSR x,1; ADDC*)
   Put5(1, -4); Put0(MOV, r, 0, cnt); Release(cnt); x.r := r; x.type := OSAB.intType*)
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
BEGIN curlev := 0; pc := 0; icx := 0; scx := 0; xrefx := 0; RL := 0; RH := EDX(*FP*); regs := {};
firstfixloc := 0; fixlistFP := 0; fixlistMU := 0;
(* inserted by me *)
regs := {};
COPY (modid, modname);
OSAB.MakeFileName(modid, name, ".s"); (*write code file*)
(*OSAB.MakeFileName(modid, hname, '.hhf');*)
(*F := Files.New(name); Files.Set(R, F, 0);*)
CreateFile(name);
(*CreateFileh(hname);*)
 (*Out.String ("file created"); Out.Ln;*)
(*IF main THEN
Write ('program '); Write (modname); Write ("_ "); Writeln (';');
ELSE
Write ('unit '); Write (modname); Write ("_ "); Writeln (';');
END;*)
COPY (name, asmname);
(*Write ('#include ("'); Write (modname); Write ('.hhf'); Writeln ('")')*)
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
(*Write ('begin '); Write (modname); Write ('_ '); Writeln (';');*)
(*IF check THEN PutArrayCheck END;*)
IF main THEN PutStart;
PutMemMan;
ELSE
PutInit;
END;
FOR i := 0 TO impind DO
   Write ('call '); Write (imports[i]); Writeln ('_init_ ;'); 
END;
wasbegin := TRUE;
END Begin;

PROCEDURE Init*;
VAR i : INTEGER;
BEGIN
END Init;

PROCEDURE Close*(VAR modid: OSAS.Ident; key, datasize: LONGINT);
VAR obj: OSAB.Object; i, nofentries: INTEGER;
name: OSAS.Ident;
(*F: Files.File; R: Files.Rider;*)
BEGIN (*Put0(MOV, SP, 0, FP); Put4(11, SP, {FP, PC}); (*MOV, LDM*)
FixupConstants;*)
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
(*IF ~ main THEN Write ('end '); Write (modname); Write ('_'); Writeln ('Init_;') ;END;
Write ('end '); Write (modid); Write ("_ "); Writeln (";");
CloseFile;
CloseFileh;*) (* hla *)
IF main THEN 
PutExit(0);
IF check THEN PutArrayCheck END;
ELSE 
   IF ~wasbegin THEN
      PutInit;  
   END;
   Ret
END;
CloseFile;

END Close;

BEGIN
cond[0] := 0; cond[1] := 1; cond[2] := 11; cond[3] := 13; cond[4] := 12; cond[5] := 10;
revcond[0] := 0; revcond[1] := 1; revcond[2] := 12; revcond[3] := 10; revcond[4] := 11; revcond[5] := 13;
landnext :=0; landend := 0; landfalse := 0;
lornext :=0; lorend := 0; lortrue := 0;
labelnum := 0; labelstr := 0; labmodnum := 0; impind := -1;
labelfloatinx := 0;
vvv := FALSE;
wasbegin := FALSE; paraindex := -1;
END OSAG.
