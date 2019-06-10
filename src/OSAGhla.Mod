MODULE OSAG; (* NW 1.7.97 / 2.5.99 / 24.11.99 / 5.10.2007*)
IMPORT SYSTEM, Files, TextRider, Out := OakOut, OSAS, OSAB, Strings := OakStrings;
(*Code generator for Oberon compiler for Strong-ARM processor.
   Procedural interface to Parser OSAP; result in array "code".
   Procedure Close writes code-files*)

CONST WordSize* = 4;
FP* = 12; SP = 13; LNK = 14; PC = 15;   (*dedicated registers*)
WRegLim* = FP-2;  (*minimum of 2 work registers in leaf procedures*)
maxCode = 10000; maxImp = 12; ITlen = 64; STlen = 128;
C8 = 100H; C12 = 1000H; C16 = 10000H; C24 = 1000000H;
RegX = 10; CC = 11; (*internal item modes*)
MAUkey =-853891096 (*0CD1AA7E8H*); FPUkey = -1949024149 (*8BD4406BH*);

(*frequently used opcodes*)
SUB = 4; RSB = 7; ADD = 8; CMP = 21; MOV = 26; MVN = 30; STR = 16; LDR = 17;

TYPE Item* = RECORD
mode*: INTEGER;
type*: OSAB.Type;
a, b, r: LONGINT;
rdo*: BOOLEAN  (*read only*)
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

VAR pc, curlev: INTEGER;   (*program counter; current level*)
entry, firstfixloc: INTEGER;   (*main entry point; first ref to const to be fixed up*)
RL: LONGINT;  (*registers variables in registers R[0] ... R[RL-1]*)
RH: LONGINT;  (*parameters in R[FP-1] ... R[H], in descending order*)
regs: SET;   (*used registers in range R[L] ... R[H-1]*)
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
code: ARRAY maxCode OF LONGINT;

main* : BOOLEAN;
modname* : OSAS.Ident;
asmname* : OSAS.Ident;
(* ------------------------------------ non portable between compilers file routins ------------------------------ *)

f: Files.File; r: TextRider.Writer;


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
(* ----------------------------------------------------- ------------------------------------------------------------------*)
(*register management*)

PROCEDURE GetReg(VAR r: LONGINT);
VAR u: LONGINT;
BEGIN u := RH-1;
WHILE u IN regs DO DEC(u) END ;
IF u >= RL THEN INCL(regs, u) ELSE OSAS.Mark("too few registers") END ;
r := u
END GetReg;

PROCEDURE Release(r: LONGINT);
BEGIN
IF (r >= RL) & (r < RH) THEN EXCL(regs, r) END
END Release;

PROCEDURE GetReg1(VAR r: LONGINT; u: LONGINT);
BEGIN Release(u); GetReg(r)
END GetReg1;

PROCEDURE GetReg2(VAR r: LONGINT; u, v: LONGINT);
BEGIN Release(v); Release(u); GetReg(r)
END GetReg2;

(*instruction assemblers according to formats*)

PROCEDURE Put0(op, dst, src0, src1: LONGINT);   (*register operation*)
BEGIN
code[pc] := (((0E00H + op)*10H + src0)*10H + dst)*1000H + src1; INC(pc)
END Put0;

PROCEDURE Put0a(op, dst, src0, src1, shmd, shcnt: LONGINT);   (*register operation with shift count*)
BEGIN
code[pc] := (((((0E00H + op)*10H + src0)*10H + dst)*20H + shcnt)*8 + shmd*2)*10H + src1; INC(pc)
END Put0a;

PROCEDURE Put0b(op, dst, src0, src1, shmd, shreg: LONGINT);   (*register operation with shift reg*)
BEGIN
code[pc] := (((((0E00H + op)*10H + src0)*10H + dst)*10H + shreg)*10H + shmd*2)*10H + src1 + 10H; INC(pc)
END Put0b;

PROCEDURE Put0c(op, dst, src0, src1, src2: LONGINT);  (*multiply*)
BEGIN
code[pc] := ((((0E00H + op)*10H + dst)*10H + src2)*10H + src0)*100H + src1 + 90H; INC(pc)
END Put0c;

PROCEDURE Put1(op, dst, src, imm: LONGINT);  (*register operation with immediate, no shift*)
BEGIN
code[pc] := (((0E20H + op)*10H + src)*10H + dst)*1000H + imm; INC(pc)
END Put1;

PROCEDURE Put1a(op, dst, src, imm, rot: LONGINT);  (*register operation with immediate*)
BEGIN
code[pc] := ((((0E20H + op)*10H + src)*10H + dst)*10H + rot)*100H + imm; INC(pc)
END Put1a;

PROCEDURE Put2(op, reg, base, offset: LONGINT);  (*Load/Store with offset literal*)
VAR temp, op1: LONGINT;
BEGIN temp := base;
IF (base = PC) & (offset # 0) THEN offset := offset - pc*4 - 8 END ;
IF offset < 0 THEN offset := -offset; op1 := 4 ELSE INC(op, 8); op1 := 8 END ;
IF offset >= 100000H THEN OSAS.Mark("offset too large")
ELSIF offset >= C12 THEN GetReg(temp); Put1a(op1, temp, base, offset DIV 1000H, 10); Release(temp)
END ;
code[pc] := (((0E40H + op)*10H + temp)*10H + reg)*C12 + offset MOD C12; INC(pc)
END Put2;

PROCEDURE Put3(op, reg, base, offreg, shift: LONGINT);  (*Load/Store with offset from register*)
BEGIN
code[pc] := ((((0E60H + op)*10H + base)*10H + reg)*20H + shift)*80H + offreg; INC(pc)
END Put3;

PROCEDURE Put4(op, base: LONGINT; regs: SET);  (*Load/Store multiple*)
BEGIN
code[pc] := ((0E80H + op)*10H + base)*10000H + SYSTEM.VAL(LONGINT, regs); INC(pc)
END Put4;

PROCEDURE Put5(cond, offset: LONGINT);  (*Branch conditional*)
BEGIN
code[pc] := (cond*10H + 10)*1000000H + offset MOD 1000000H; INC(pc)
END Put5;

PROCEDURE Put5a(offset: LONGINT);  (*Branch and Link*)
BEGIN
code[pc] := -352321536 (*0EB000000H*) + offset MOD 1000000H; INC(pc)
END Put5a;

PROCEDURE Put6(cond, num: LONGINT);  (*SWI*)
BEGIN
code[pc] := (cond*10H + 15) * 1000000H + num; INC(pc)
END Put6;

PROCEDURE PutC(u: LONGINT);
BEGIN code[pc] := u; INC(pc)
END PutC;

PROCEDURE SetCC(VAR x: Item; n: LONGINT);
BEGIN x.mode := CC; x.a := 0; x.b := 0; x.r := n
END SetCC;

PROCEDURE negated(cond: LONGINT): LONGINT;
VAR c: LONGINT;
BEGIN
IF ODD(cond) THEN c := cond-1 ELSE c := cond+1 END ;
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
BEGIN code[at] := code[at] DIV C24 * C24 + ((pc - at - 2) MOD C24)
END fix24;

PROCEDURE fix12(at: LONGINT);
VAR d: LONGINT;
BEGIN d := (pc - at - 2)*4; (*d >= 0, fixup LDR*)
IF d < C12 THEN code[at] := code[at] DIV C12 * C12 + d
ELSE OSAS.Mark("const not allocatable")
END
END fix12;

PROCEDURE FixLink*(L0: LONGINT);
VAR L1: LONGINT;
BEGIN
WHILE L0 # 0 DO L1 := code[L0] MOD C24; fix24(L0); L0 := L1 END
END FixLink;

PROCEDURE FixLinkWith(L0, dst: LONGINT);
VAR L1: LONGINT;
BEGIN
WHILE L0 # 0 DO
L1 := code[L0] MOD C24;
code[L0] := code[L0] DIV C24 * C24 + ((dst - L0 - 2) MOD C24); L0 := L1
END
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
IF scx < ITlen THEN
TSC[scx].index := k; TSC[scx].adr := pc; INC(scx);
IF firstfixloc = 0 THEN firstfixloc := pc END
ELSE OSAS.Mark("too many strings"); scx := 0
END
END enterStr;

PROCEDURE ScaleConst(VAR n, s: LONGINT);
VAR m: INTEGER;
BEGIN m := 16;
IF n # 0 THEN
WHILE n MOD 4 = 0 DO n := SYSTEM.LSH(n, -2); DEC(m) END
END ;
s := m MOD 16
END ScaleConst ;

PROCEDURE FixupConstants;
VAR i, j, k, disp, s, pc0: LONGINT;
BEGIN i := 0;
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
END FixupConstants;

(* loading of operands and addresses into registers *)

PROCEDURE ExtRef(VAR x: Item; r, d: LONGINT);
BEGIN
IF xrefx < ITlen THEN
TXR[xrefx].ref := - x.b * 10000H + x.a + d; TXR[xrefx].adr := pc; INC(xrefx);
IF firstfixloc = 0 THEN firstfixloc := pc END
ELSE OSAS.Mark("too many external refs"); xrefx := 0
END ;
Put2(LDR, r, PC, 0)
END ExtRef;

PROCEDURE load(VAR x: Item);
VAR r, cd, n, s: LONGINT;
BEGIN
IF x.mode # OSAB.Reg THEN
IF x.type.size = 1 THEN cd := LDR+4 ELSE cd := LDR END ;
IF x.mode = OSAB.Var THEN
IF x.b >= 0 THEN GetReg1(r, x.r); Put2(cd, r, x.r, x.a)
ELSE GetReg(r); ExtRef(x, r, 0); Put2(LDR, r, r, 0)
END
ELSIF x.mode = OSAB.Par THEN GetReg1(r, x.r); Put2(LDR, r, x.r, x.a); Put2(cd, r, r, x.b)
ELSIF x.mode = OSAB.RegI THEN GetReg1(r, x.r); Put2(cd, r, x.r, x.a)
ELSIF x.mode = RegX THEN GetReg2(r, x.r, x.a); Put3(cd+8, r, x.r, x.a, x.b)
ELSIF x.mode = OSAB.Const THEN
IF x.type.form <= OSAB.Set THEN
GetReg(r); n := x.a;
IF n = 0 THEN Put1(MOV, r, 0, 0)
ELSIF (n > 0) OR (n <= -C8) THEN
ScaleConst(n, s);
IF n DIV C8 = 0 THEN Put1a(MOV, r, 0, n, s)
ELSE enterIC(x.a); Put2(LDR, r, PC, 0)
END
ELSE Put1(MVN, r, 0, -n-1)
END
ELSIF x.type.form = OSAB.Proc THEN
GetReg(r);
IF x.b >= 0 THEN
n := pc + 2 - x.a; Put1a(SUB, r, PC, n MOD C8, 15);
IF n >= C8 THEN Put1a(SUB, r, r, n DIV C8, 11) END
ELSE ExtRef(x, r, 100H);
END ;
ELSIF x.type.form = OSAB.NilTyp THEN GetReg(r); Put1(MOV, r, 0, 0)
END
ELSIF x.mode = CC THEN GetReg(r);
IF (x.a = 0) & (x.b = 0) THEN
Put1((x.r+2)*100H + MOV, r, 0, 1); Put1((negated(x.r)+2)*100H + MOV, r, 0, 0)
ELSE Put5(x.r, 1); FixLink(x.a); Put1(MOV, r, 0, 0); Put5(14, 0); FixLink(x.b); Put1(MOV, r, 0, 1)
END
ELSE OSAS.Mark("illegal object (type?)");
Out.String("load"); Out.Int(x.mode, 4); Out.Int(x.a, 5); Out.Int(x.b, 5);
Out.Ln
END ;
x.mode := OSAB.Reg; x.r := r
END
END load;

PROCEDURE loadAdr(VAR x: Item);
VAR n, r, s, temp: LONGINT;
BEGIN
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
END loadAdr;

PROCEDURE loadCC(VAR x: Item);
BEGIN
IF x.type.form = OSAB.Bool THEN
IF x.mode = OSAB.Const THEN Put0(CMP, 0, 0, 0); SetCC(x, 1-x.a)
ELSE load(x); Put1(17, 0, x.r, 1); Release(x.r); SetCC(x, 1)
END
ELSE OSAS.Mark("must be Boolean"); x.a := 0; x.b := 0
END
END loadCC;

PROCEDURE loadArrayLen(VAR x: Item);
VAR z: Item;
BEGIN z.type := OSAB.intType;
IF x.type.len >= 0 THEN z.mode := OSAB.Const; z.a := x.type.len
ELSE (*open array*)
IF x.mode = OSAB.Par THEN z.mode := OSAB.Var; z.r := x.r; z.a := x.a - 4
ELSE OSAS.Mark("bad array parameter")
END
END ;
load(z)
END loadArrayLen;

PROCEDURE loadString(VAR x: Item);
VAR r, xL: LONGINT;
BEGIN xL := x.a DIV 10000H; x.a := x.a MOD 10000H;
IF x.mode = OSAB.Const THEN
GetReg(r); x.mode := OSAB.Reg; x.r := r; enterStr(x.a); Put1(ADD, r, PC, 0)
ELSE loadAdr(x); x.b := xL
END
END loadString;

(*
PROCEDURE AllocString*(VAR adr: LONGINT);
(*String Constant declaration; moves string into code array*)
BEGIN adr := OSAS.slen*10000H + pc*4; OSAS.MoveStrings(code, pc, maxCode)
END AllocString;
*)


PROCEDURE AllocString*(VAR o : OSAB.Object);
(*String Constant declaration; moves string into code array*)
VAR tmpstr : OSAS.Ident;
label : OSAS.Ident;
BEGIN 
Writeln ('.section .data');
COPY('', label);
Strings.Append ('.', label);
Strings.Append (modname, label);
Strings.Append ('_', label);
Strings.Append (o.name, label);
Write (label); COPY(label, o.label);
(*Write ('.'); Write (modname); Write ('_'); Write (o.name);*)
Writeln (':');
Write ('       .ascii '); Write ('"'); 
Write (OSAS.name);
(*
OSAS.CopyId(tmpstr);
Write (tmpstr);
*)
Writeln ('\0"');
(*adr := OSAS.slen*10000H + pc*4; OSAS.MoveStrings(code, pc, maxCode)*)
END AllocString;

PROCEDURE AllocVariablesInit*;
BEGIN
    Writeln (".section .bss");
END AllocVariablesInit;    

PROCEDURE AllocVariable*(VAR o : OSAB.Object);
VAR label: OSAS.Ident;
BEGIN
    COPY ('', label);  
       IF o.expo THEN
          Strings.Append (modname, label);
          Write ("       .comm   ");
       ELSE
          Write ("       .lcomm   ");
       END;
       Out.String ("generating variable for "); Out.String (o.name); Out.Ln;
    Strings.Append ("_", label);
    Strings.Append (o.name, label);
    Write (label);
    Write (',');
    Writeint (o.type.size);
    Writeln ('');    
END AllocVariable;

PROCEDURE Q(T: OSAB.Type);
VAR obj: OSAB.Object;
BEGIN
IF T.base # NIL THEN
Q(T.base); obj := T.typobj;
PutC(obj.val * 10000H + fixlist[-obj.lev]); fixlist[-obj.lev] := pc-1
END
END Q;

PROCEDURE AllocTD*(obj: OSAB.Object);
VAR n: INTEGER; tp: OSAB.Type;
BEGIN obj.lev := 0; obj.val := pc*4; tp := obj.type;
IF tp.form = OSAB.Pointer THEN tp := tp.base END ;
PutC(tp.size); (*does not include tag*)
Q(tp); n := tp.nofpar; 
WHILE n < 2 DO PutC(0); INC(n) END ;
END AllocTD;

(* Items: Conversion from constants or from Objects on the Heap to Items on the Stack*)

PROCEDURE Val*(VAR x: Item): LONGINT;
BEGIN 
Out.String ("x.a="); Out.Int(x.a, 0); Out.Ln;
RETURN x.a
END Val;

PROCEDURE Lev*(VAR x: Item): LONGINT;
BEGIN RETURN x.b
END Lev;

PROCEDURE MakeConstItem*(VAR x: Item; typ: OSAB.Type; val: LONGINT);
BEGIN x.mode := OSAB.Const; x.type := typ; x.a := val
END MakeConstItem;

PROCEDURE MakeRealItem*(VAR x: Item; val: REAL);
BEGIN x.mode := OSAB.Const; x.type := OSAB.realType; x.a := SYSTEM.VAL(LONGINT, val)
END MakeRealItem;

PROCEDURE MakeStringItem*(VAR x: Item; inx, len: LONGINT);
BEGIN x.mode := OSAB.Const; x.type := OSAB.strType; x.a := inx; x.b:= len
END MakeStringItem;

PROCEDURE MakeItem*(VAR x: Item; y: OSAB.Object);
VAR cl: INTEGER; r: LONGINT;
BEGIN cl := y.class; x.mode := cl; x.type := y.type; x.a := y.val; x.b := y.lev; x.rdo := y.rdo;
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
END
END MakeItem;

(* Code generation for Selectors, Variables, Constants *)

PROCEDURE AddConst(op: LONGINT; VAR x: Item; n: LONGINT);
VAR r0, r1, n0, s: LONGINT;  (*n >= 0*)
BEGIN n0 := n; ScaleConst(n0, s); GetReg1(r0, x.r);
IF n0 DIV C8 = 0 THEN Put1a(op, r0, x.r, n0, s)
ELSE enterIC(n); GetReg(r1); Put2(LDR, r1, PC, 0); Put0(op, r0, x.r, r1); Release(r1)
END ;
x.r := r0
END AddConst;

PROCEDURE Field*(VAR x: Item; y: OSAB.Object);   (* x := x.y *)
VAR r: LONGINT;
BEGIN x.type := y.type;
IF (x.mode = OSAB.Var) OR (x.mode = OSAB.RegI) THEN x.a := x.a + y.val
ELSIF x.mode = OSAB.Par THEN x.b := x.b +  y.val
ELSIF x.mode = RegX THEN
GetReg2(r, x.r, x.a); Put0a(ADD, r, x.r, x.a, 0, x.b); x.mode := OSAB.RegI; x.r := r; x.a := y.val
ELSE OSAS.Mark("bad mode in Field")
END ;
END Field;

PROCEDURE Index*(VAR x, y: Item; check: BOOLEAN);   (* x := x[y] *)
VAR r, r0, s, s0, n, lim: LONGINT;
BEGIN s := x.type.base.size; lim := x.type.len;
IF (y.mode = OSAB.Const) & (lim >= 0) THEN
IF (y.a >= 0) & (y.a < lim) THEN
IF (x.mode = OSAB.Var) OR (x.mode = OSAB.RegI) THEN x.a := y.a * s + x.a
ELSIF x.mode = OSAB.Par THEN x.b := y.a * s + x.b
ELSIF x.mode = RegX THEN
Put0a(ADD, x.a, x.r, x.a, 0, x.b); Release(x.b); x.mode := OSAB.RegI; x.a := y.a * s
ELSE OSAS.Mark("bad mode in Index")
END
ELSE OSAS.Mark("index out of range")
END ;
ELSE load(y);
IF check THEN (*check index bounds*)
IF lim >= 0 THEN AddConst(CMP, y, lim)
ELSE (*open array*)
IF x.mode <= OSAB.Par THEN GetReg(lim); Put2(LDR, lim, x.r, x.a - 4); Release(lim)
ELSIF x.mode = OSAB.RegI THEN lim := x.r - 1
ELSE OSAS.Mark("bad mode in Index")
END ;
Put0(CMP, 0, y.r, lim)
END ;
Put6(2, 1)   (*SWI*)
END ;
IF x.mode = OSAB.Par THEN
GetReg1(r, x.r); Put2(LDR, r, x.r, x.a); x.mode := OSAB.RegI; x.r := r; x.a := x.b
END ;
s0 := s; n := 0;
WHILE ~ODD(s0) DO s0 := s0 DIV 2; INC(n) END ;
IF (s0 = 1) & (x.mode = OSAB.RegI) & (x.a = 0) THEN x.mode := RegX; x.a := y.r; x.b := n
ELSIF x.mode IN {OSAB.Var, OSAB.RegI, RegX} THEN
IF s0 = 1 THEN GetReg2(r, x.r, y.r); Put0a(ADD, r, x.r, y.r, 0, n)
ELSE GetReg(r0); s0 := s; ScaleConst(s0, n);
IF s0 < C8 THEN Put1a(MOV, r0, 0, s0, n)
ELSE enterIC(s); Put2(LDR, r0, PC, 0)
END ;
Release(r0); GetReg2(r, x.r, y.r); Put0c(2, r, y.r, r0, x.r)  (*MUL*)
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
x.r := negated(x.r); t := x.a; x.a := x.b; x.b := t
END Not;

PROCEDURE And1*(VAR x: Item);   (* x := x & *)
BEGIN
IF x.mode # CC THEN loadCC(x) END ;
Put5(negated(x.r), x.a); x.a := pc-1; FixLink(x.b); x.b := 0
END And1;

PROCEDURE And2*(VAR x, y: Item);
BEGIN
IF y.mode # CC THEN loadCC(y) END ;
x.a := merged(y.a, x.a); x.b := y.b; x.r := y.r
END And2;

PROCEDURE Or1*(VAR x: Item);   (* x := x OR *)
BEGIN
IF x.mode # CC THEN loadCC(x) END ;
Put5(x.r, x.b); x.b := pc-1; FixLink(x.a); x.a := 0
END Or1;

PROCEDURE Or2*(VAR x, y: Item);
BEGIN
IF y.mode # CC THEN loadCC(y) END ;
x.a := y.a; x.b := merged(y.b, x.b); x.r := y.r
END Or2;

(* Code generation for arithmetic operators *)

PROCEDURE Neg*(VAR x: Item);   (* x := -x *)
VAR r: LONGINT;
BEGIN
IF x.type.form = OSAB.Int THEN
IF x.mode = OSAB.Const THEN x.a := -x.a
ELSE load(x); GetReg1(r, x.r); Put1(RSB, r, x.r, 0); x.r := r
END
ELSIF x.type.form = OSAB.Real THEN
IF x.mode = OSAB.Const THEN
IF x.a # 0 THEN x.a := -x.a (*+*) (*-2147483648*)  (*80000000H*) END (* ! *)
ELSE load(x); Put1a(2, x.r, x.r, 2, 1)
END
ELSE (*form = Set*)
IF x.mode = OSAB.Const THEN x.a := -x.a-1 
ELSE load(x); GetReg1(r, x.r); Put0(MVN, r, 0, x.r); x.r := r
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
IF y.a >= 0 THEN AddConst(ADD+1, x, y.a) ELSE AddConst(SUB+1, x, -y.a) END
END
ELSIF x.mode = OSAB.Const THEN
load(y);
IF x.a >= 0 THEN AddConst(ADD+1, y, x.a)
ELSE AddConst(RSB, y, -x.a); Put1(RSB, y.r, y.r, 0)
END ;
x := y
ELSE load(x); load(y); GetReg2(r, x.r, y.r); Put0(ADD+1, r, x.r, y.r); x.r := r
END
ELSE (*op = minus*)
IF y.mode = OSAB.Const THEN
IF x.mode = OSAB.Const THEN x.a := x.a - y.a
ELSE load(x);
IF y.a >= 0 THEN AddConst(SUB+1, x, y.a) ELSE AddConst(ADD+1, x, -y.a) END
END
ELSIF x.mode = OSAB.Const THEN
load(y);
IF x.a >= 0 THEN AddConst(SUB+3, y, x.a)
ELSE AddConst(ADD+1, y, -x.a); Put1(RSB, y.r, y.r, 0)
END ;
x := y
ELSE load(x); load(y); GetReg2(r, x.r, y.r); Put0(SUB+1, r, x.r, y.r); x.r := r
END
END
END AddOp;

PROCEDURE log(m: LONGINT; VAR n: LONGINT): LONGINT;
BEGIN n := 0;
WHILE ~ODD(m) DO m := m DIV 2; INC(n) END ;
RETURN m
END log;

PROCEDURE multiply(VAR x, c: Item);  (*c.a >= 2*)
VAR i, j, m, r: LONGINT;
BEGIN GetReg1(r, x.r); m := c.a; i := 0;
REPEAT m := m DIV 2; INC(i) UNTIL ODD(m);
IF m = 1 THEN
IF ODD(c.a) THEN Put0a(ADD+1, r, x.r, x.r, 0, i) ELSE Put0a(MOV+1, r, 0, x.r, 0, i) END
ELSE load(c); Put0c(1, r, x.r, c.r, 0); Release(c.r)
END ;
x.r := r
END multiply;

PROCEDURE MulOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
VAR n, r: LONGINT;
BEGIN
IF (x.mode = OSAB.Const) & (y.mode = OSAB.Const) THEN
IF op = OSAS.times THEN x.a := x.a * y.a
ELSIF y.a = 0 THEN OSAS.Mark("division by 0")
ELSIF op = OSAS.div THEN x.a := x.a DIV y.a
ELSE x.a := x.a MOD y.a
END
ELSE
IF op = OSAS.times THEN
IF (x.mode = OSAB.Const) & (x.a >= 2) THEN
load(y); multiply(y, x); x.mode := OSAB.Reg; x.r := y.r
ELSIF (y.mode = OSAB.Const) & (y.a >= 2) THEN load(x); multiply(x, y)
ELSE load(x); load(y); GetReg2(r, x.r, y.r); Put0c(1, r, x.r, y.r, 0); x.r := r
END
ELSIF op = OSAS.div THEN load(x);
IF (y.mode = OSAB.Const) & (y.a > 0) & (log(y.a, n) = 1) THEN
GetReg1(r, x.r); Put0a(MOV+1, r, 0, x.r, 2, n); x.r := r
ELSE load(y);
IF x.r = FP-1 THEN op := 9
ELSIF x.r = FP-2 THEN op := 10
ELSE OSAS.Mark("expression too complex")
END ;
Release(y.r); Put5a(op * 10000H + fixlistFP); fixlistFP := pc-1
END
ELSIF op = OSAS.mod THEN load(x);
IF (y.mode = OSAB.Const) & (y.a > 0) & (log(y.a, n) = 1) THEN
GetReg1(r, x.r);
IF n <= 8 THEN Put1(0, r, x.r, y.a-1)  (*AND*)
ELSIF n >= 24 THEN Put1a(29, r, x.r, (-y.a) DIV 1000000H MOD 100H, 4) (*BIC*)
ELSE Put0a(MOV, r, 0, x.r, 0, (32-n) MOD 20H); Put0a(MOV+1, r, 0, r, 1, (32-n) MOD 20H)
END ;
x.r := r
ELSE load(y);
IF x.r = FP-1 THEN op := 9
ELSIF x.r = FP-2 THEN op := 10
ELSE OSAS.Mark("expression too complex")
END ;
Release(y.r); Put5a(op * 10000H + fixlistFP); fixlistFP := pc-1;
IF x.r IN regs THEN Put0(MOV, x.r, 0, y.r) ELSE Put0(MOV, y.r, 0, x.r); x.r := y.r END
END
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
VAR r0, r1: LONGINT;
BEGIN
IF (y.mode = OSAB.Const) & (z.mode = OSAB.Const) THEN
x.mode := OSAB.Const;
IF y.a <= z.a THEN x.a := ASH(2, z.a) - ASH(1, y.a) ELSE x.a := 0 END
ELSE x.mode := OSAB.Reg; load(z); 
IF (y.mode = OSAB.Const) & (y.a = 0) THEN
GetReg(r0); Put1(MVN, r0, 0, 1); Release(r0); GetReg1(x.r, z.r); Put0b(MVN+1, x.r, 0, r0, 0, z.r)
ELSE load(y); GetReg(r0); Put1(MVN, r0, 0, 1); Put0b(MVN, r0, 0, r0, 0, z.r);
GetReg(r1); Put1(MVN, r1, 0, 0); Release(r1); Release(r0);
GetReg2(x.r, y.r, z.r); Put0b(1, x.r, r0, r1, 0, y.r)
END
END
END Set;

PROCEDURE In*(VAR x, y: Item);  (* x := x IN y *)
VAR r: LONGINT;
BEGIN load(y);
IF x.mode = OSAB.Const THEN
Put1a(17, 0, y.r, (x.a MOD 2) + 1, (16 - (x.a DIV 2)) MOD 10H)  (*TST*)
ELSE load(x); GetReg(r); Put1(MOV, r, 0, 1);
Put0b(17, 0, y.r, r, 0, x.r); Release(r); Release(x.r)
END ;
Release(y.r); SetCC(x, 1)
END In;

PROCEDURE SetOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
VAR n, r, r0, s, op0: LONGINT;  (*x.type.form = Set*)
BEGIN
IF op = OSAS.plus THEN op0 := 24 (*or*) ELSIF op = OSAS.minus THEN op0 := 28 (*bic*)
ELSIF op = OSAS.times THEN op0 := 0 (*and*) ELSIF op = OSAS.rdiv THEN op0 := 2 (*xor*)
ELSE OSAS.Mark("not a set operator"); op0 := 0
END ;
IF y.mode = OSAB.Const THEN
IF (x.mode = OSAB.Const) & (op = OSAS.plus) THEN
x.a := SYSTEM.VAL(LONGINT, SYSTEM.VAL(SET, x.a) + SYSTEM.VAL(SET, y.a))
ELSE load(x); n := y.a; ScaleConst(n, s); GetReg1(r, x.r); 
IF n DIV C8 = 0 THEN Put1a(op0, r, x.r, n, s MOD 10H)
ELSE  enterIC(y.a); GetReg(r0); Put2(LDR, r0, PC, 0); Put0(op0, r, x.r, r0); Release(r0)
END
END
ELSE load(x); load(y); GetReg2(r, x.r, y.r); Put0(op0, r, x.r, y.r)
END ;
x.r := r
END SetOp;

(* Code generation for relations *)

PROCEDURE IntRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
VAR r: LONGINT;
BEGIN
IF (y.mode = OSAB.Const) & (y.type.form # OSAB.Proc) THEN
load(x);
IF y.a >= 0 THEN AddConst(CMP, x, y.a) ELSE AddConst(CMP+2, x, -y.a) END ;
Release(x.r); SetCC(x, cond[op - OSAS.eql])
ELSIF (x.mode = OSAB.Const) & (x.type.form # OSAB.Proc) THEN
load(y);  (*CMPI*)
IF x.a >= 0 THEN AddConst(CMP, y, x.a) ELSE AddConst(CMP+2, y, -x.a) END ;
Release(y.r); SetCC(x, revcond[op - OSAS.eql])
ELSE load(x); load(y); Put0(CMP, r, x.r, y.r);
Release(y.r); Release(x.r); SetCC(x, cond[op - OSAS.eql])
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
Release(y.r); Release(x.r); SetCC(x, k)
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
Put0a(ADD, x.r, x.r, x.a, 0, x.b); Release(x.a); x.a := 0; x.mode := OSAB.RegI
END
END PrepStore;

PROCEDURE Store*(VAR x, y: Item); (* x := y *)
VAR cd, ra: LONGINT;
BEGIN load(y);
IF x.mode = OSAB.Reg THEN
cd := code[pc-1];
IF (y.r < RH) & (y.r >= RL) & (cd DIV 4000000H = -8) & (cd DIV 10H MOD 10H # 9)
OR (cd DIV 4000000H = -7) & ODD(cd DIV 100000H) THEN (*fixup*)
code[pc-1] := cd DIV 10000H * 10000H + x.r * 1000H + cd MOD 1000H
ELSE Put0(MOV, x.r, 0, y.r)
END
ELSE
IF x.type.size = 1 THEN cd := STR+4 ELSE cd := STR END ;
IF x.mode = OSAB.Var THEN Put2(cd, y.r, x.r, x.a)
ELSIF x.mode = OSAB.Par THEN
GetReg(ra); Put2(LDR, ra, x.r, x.a); Put2(cd, y.r, ra, x.b); Release(ra)
ELSIF x.mode = RegX THEN Put3(cd+8, y.r, x.r, x.a, x.b); Release(x.a)
ELSIF x.mode = OSAB.RegI THEN Put2(cd, y.r, x.r, x.a); Release(x.a)
ELSE OSAS.Mark("bad mode in Store");
Out.String("Store"); Out.Int(x.mode, 4); Out.Int(y.mode, 4);
Out.Ln
END ;
Release(x.r)
END ;
Release(y.r)
END Store;

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
BEGIN GetReg(r);
IF x.type.len = -1 THEN (*open dest array*)
Put2(LDR, r, x.r, x.a+4); Put1(CMP, 0, r, y.b); Put6(11, 6)
ELSIF x.type.len < y.b THEN OSAS.Mark("string too long")
END ;
loadAdr(x); loadString(y);
pc0 := pc; Put2(3, r, y.r, 4); Put2(2, r, x.r, 4); Put1a(1, r, r, 0FFH, 4); Put5(1, pc0 - pc -2);
Release(r); Release(y.r); Release(x.r)
END CopyString;

(* Code generation for parameters *)

PROCEDURE VarParam*(VAR x: Item; ftype: OSAB.Type);
VAR x0, tdes: Item;
BEGIN x0 := x; loadAdr(x);
IF (ftype.form = OSAB.Array) & (ftype.len < 0) THEN loadArrayLen(x0)
ELSIF ftype.form = OSAB.Record THEN
IF x.type.typobj = NIL THEN OSAS.Mark("anonymous type for VAR-Param") END ;
MakeItem(tdes, x.type.typobj); tdes.mode := OSAB.Var; loadAdr(tdes)
END
END VarParam;

PROCEDURE ValueParam*(VAR x: Item);
VAR r: LONGINT;
BEGIN
IF (x.mode = OSAB.Reg) & ((x.r < RL-1) OR (x.r >= RH)) THEN
GetReg(r); Put0(MOV, r, 0, x.r)  (*register variable*)
ELSE load(x)
END ;
END ValueParam;

PROCEDURE StringParam*(VAR x: Item);
VAR r: LONGINT;
BEGIN (*string constant?*)
loadString(x); GetReg(r); Put1(MOV, r, 0, x.b) (*len*)
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
VAR k: LONGINT;
BEGIN load(x); Put1(CMP, 0, x.r, n);
Put0a(ADD+500H, PC, PC, x.r, 0, 2); Release(x.r);
Put6(14, 4);   (*SWI; case out of range*)
L := pc; k := 0;  (*branch table*)
WHILE k < n DO PutC(0); INC(k) END
END Case;

PROCEDURE For0*(VAR x, y: Item);
BEGIN
IF x.mode = OSAB.Reg THEN Store(x, y); y.r := x.r ELSE load(y) END
END For0;

PROCEDURE For1*(VAR x, y, z, w: Item; VAR L: LONGINT);
VAR r: LONGINT;
BEGIN r := y.r; IntRelation(OSAS.leq, y, z); L := pc;
IF (r >= RL) & (r < RH) THEN INCL(regs, r) END ;
IF w.a > 0 THEN Put5(12, 0)
ELSIF w.a < 0 THEN Put5(11, 0)
ELSE OSAS.Mark("zero increment"); Put5(0, 0)
END ;
IF x.mode # OSAB.Reg THEN Put2(STR, r, x.r, x.a); Release(r) END
END For1;

PROCEDURE For2*(VAR x, y, w: Item);
VAR n: LONGINT;
BEGIN n := w.a; load(x); Release(x.r);
IF (n >= 0) & (n < 100H) THEN Put1(ADD, x.r, x.r, n)
ELSIF (n < 0) & (n >= -100H) THEN Put1(SUB, x.r, x.r, -n)
ELSE OSAS.Mark("step too large")
END
END For2;

(* Branches, procedure calls, procedure prolog and epilog *)

PROCEDURE Here*(VAR L: LONGINT);
BEGIN L := pc
END Here;

PROCEDURE FJump*(VAR L: LONGINT);
BEGIN Put5(14, L); L := pc-1
END FJump;

PROCEDURE CFJump*(VAR x: Item);
BEGIN
IF x.mode # CC THEN loadCC(x) END ;
Put5( negated(x.r), x.a); FixLink(x.b); x.a := pc-1
END CFJump;

PROCEDURE BJump*(L: LONGINT);
BEGIN Put5(14, L-pc-2)
END BJump;

PROCEDURE CBJump*(VAR x: Item; L: LONGINT);
BEGIN
IF x.mode # CC THEN loadCC(x) END ;
Put5(negated(x.r), L-pc-2); FixLink(x.b); FixLinkWith(x.a, L)
END CBJump;

PROCEDURE Fixup*(VAR x: Item);
BEGIN FixLink(x.a)
END Fixup;

PROCEDURE SaveRegs*(VAR rs: SET);
BEGIN rs := regs;
IF regs # {} THEN Put4(18, SP, regs); regs := {} END  (*STM*)
END SaveRegs;

PROCEDURE Call*(VAR x: Item; rs: SET);
VAR pc0, r: LONGINT;
BEGIN RL := 0;
IF x.type.form = OSAB.Proc THEN
IF x.mode = OSAB.Const THEN
IF x.b >= 0 THEN Put5a(x.a-pc-2)
ELSE (*imported*) Put5a(x.a*10000H + fixlist[-x.b]); fixlist[-x.b] := pc-1
END
ELSE load(x); Put0(MOV, LNK, 0, PC); Put0(MOV, PC, 0, x.r); Release(x.r)
END ;
IF x.type.base.form # OSAB.NoTyp THEN
IF rs # {} THEN
regs := rs; GetReg(r); x.mode := OSAB.Reg; x.r := r;
Put0(MOV, r, 0, FP-1); Put4(11, SP, rs)  (*restore registers*)
ELSE regs := {FP-1}; r := FP-1
END ;
x.mode := OSAB.Reg; x.r := r
ELSE regs := {}
END
ELSE OSAS.Mark("not a procedure"); regs := {}
END
END Call;

PROCEDURE Header*;
VAR i: INTEGER;
BEGIN entry := pc; Put4(18, SP, {FP, LNK});  (*STM*)
Put0(MOV, FP, 0, SP)  (*FP := SP*)
END Header;

PROCEDURE Enter*(leaf, int: BOOLEAN; level, regvarno: INTEGER; parsize, varsize: LONGINT);
VAR n, nofregs: LONGINT;
BEGIN curlev := level; RL := regvarno; RH := FP; nofregs := parsize DIV 4;
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
VAR res: Item;
BEGIN resreg := FP -resreg - 1;
IF (form # OSAB.NoTyp) & ~((x.mode = OSAB.Reg) & (x.r = resreg)) THEN
res.mode := OSAB.Reg; res.r := resreg; res.type := x.type; Store(res, x)
END ;
IF ~int THEN (*procedure epilog*)
Put0(MOV, SP, 0, FP);   (*SP := FP*)
Put4(11, SP, {FP, PC})    (*restore FP, PC*)
ELSE
IF ~leaf THEN (*normal interupt handler epilog*)
Put0(MOV, SP, 0, FP); Put4(11, SP, {0 .. FP, LNK});  (*restore R0 - R11, FP, LNK*)
END ;
Put1(SUB+1, PC, LNK, offset)    (*PC := LNK - offset*)
END ;
RL := 0;  RH := FP; regs := {}; FixupConstants
END Return;

(* In-line code procedures*)

PROCEDURE Increment*(upordown: LONGINT; VAR x, y: Item);
VAR op, inc, r, r1: LONGINT;
BEGIN
IF upordown = 0 THEN op := ADD ELSE op := SUB END ;
IF y.type.form = OSAB.NoTyp THEN inc := 1
ELSE (*integer constant*) inc := y.a;
IF (inc < 0) OR (inc >= 100H) THEN OSAS.Mark("bad increment") END
END ;
IF x.mode = OSAB.Reg THEN Put1(op, x.r, x.r, inc)
ELSE GetReg(r);
IF (x.mode = OSAB.Var) OR (x.mode = OSAB.RegI) THEN
Put2(LDR, r, x.r, x.a); Put1(op, r, r, inc); Put2(STR, r, x.r, x.a)
ELSIF x.mode = OSAB.Par THEN
Put2(LDR, r, x.r, x.a); GetReg(r1); Put2(LDR, r1, r, x.b);
Put1(op, r1, r1, inc); Put2(STR, r1, r , x.b); Release(r1)
ELSIF x.mode = RegX THEN
Put3(25, r, x.r, x.a, x.b); Put1(op, r, r, inc); Put3(24, r, x.r, x.a, x.b); Release(x.a)
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
IF pc >= maxCode-100H THEN OSAS.Mark("program too long"); pc := 0 END ;
IF (firstfixloc # 0) & (pc - firstfixloc >= 220) THEN
pc0 := pc; INC(pc); FixupConstants; code[pc0] := -369098752(*0EA000000H*) + pc - pc0 - 2
END ;
regs := {}
END CheckRegs;

PROCEDURE Open* (VAR flname : ARRAY OF CHAR; modid : OSAS.Ident);
VAR i: INTEGER;
name : OSAS.Ident;
BEGIN curlev := 0; pc := 0; icx := 0; scx := 0; xrefx := 0; RL := 0; RH := FP; regs := {};
firstfixloc := 0; fixlistFP := 0; fixlistMU := 0;
FOR i := 0 TO maxImp-1 DO fixlist[i] := 0 END ;
PutC(0);  (*avoid fixups at address 0!*)
(* inserted by me *)
COPY (modid, modname);
OSAB.MakeFileName(modid, name, ".s"); (*write code file*)
Out.String ("name is "); Out.String (name); Out.Ln;
(*F := Files.New(name); Files.Set(R, F, 0);*)
CreateFile(name);
 Out.String ("file created"); Out.Ln;
Write ('       .file "'); Write (flname);
Writeln ('"');
Out.String ("exiting open"); Out.Ln;
COPY (name, asmname);
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
BEGIN
Writeln (".section .text");
IF main THEN Writeln (".globl  _start") END;
Writeln ("_start:");
END Begin;

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
IF main THEN
Writeln ("       movl $1, %eax");
Writeln ("       movl $0, %ebx");
Writeln ("       int $0x80");
END;
CloseFile;
END Close;

BEGIN Out.Open;
cond[0] := 0; cond[1] := 1; cond[2] := 11; cond[3] := 13; cond[4] := 12; cond[5] := 10;
revcond[0] := 0; revcond[1] := 1; revcond[2] := 12; revcond[3] := 10; revcond[4] := 11; revcond[5] := 13;
END OSAG.
