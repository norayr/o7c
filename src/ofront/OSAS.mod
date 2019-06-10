MODULE OSAS; (* NW 19.9.93 / 16.9.98 / 8.8.2007*)
  IMPORT SYSTEM, Reals := CompatReals, Files, (*TextRider,*) Out := OakOut;
  
(* Oberon-SA Scanner does lexical analysis. Input is Oberon-Text, output is
  sequence of symbols, i.e identifiers, numbers, strings, and special symbols.
  Recognises all Oberon keywords and skips comments. The 36 keywords are
  recorded in a hash table. Strings are stored in the string table str.
  Get(sym) delivers next symbol from input text with Reader R.
  Mark(msg) records error and delivers error message with Writer W;
  CopyId and NotEql are to copy and compare the identifier read *)
  
  CONST IdLen* = 32; WS = 4; (*Word size*)
    KTsize = 59; maxExp = 38; stringTabSize = 1024;
    maxStrLen = 128;
    
    (*lexical symbols*)
    null = 0; times* = 1; rdiv* = 2; div* = 3; mod* = 4;
    and* = 5; plus* = 6; minus* = 7; or* = 8; eql* = 9;
    neq* = 10; lss* = 11; leq* = 12; gtr* = 13; geq* = 14;
    in* = 15; is* = 16; arrow* = 17; period* = 18;
    char* = 20; int* = 21; real* = 22; false* = 23; true* = 24;
    nil* = 25; string* = 26; not* = 27; lparen* = 28; lbrak* = 29;
    lbrace* = 30; ident* = 31; if* = 32; case* = 33; while* = 34;
    repeat* = 35; for* = 36;  with* = 37; assert* = 38;
    comma* = 40; colon* = 41; becomes* = 42; upto* = 43; rparen* = 44;
    rbrak* = 45; rbrace* = 46; then* = 47; of* = 48; do* = 49;
    to* = 50; by* = 51; semicolon* = 52; end* = 53; bar* = 54;
    else* = 55; elsif* = 56; until* = 57; return* = 58;
    array* = 60; record* = 61; pointer* = 62; const* = 63; type* = 64;
    var* = 65; procedure* = 66; begin* = 67; import* = 68; module* = 69;
    eof = 70; asm* = 77; dispose* = 99;

  TYPE Ident* = ARRAY IdLen OF CHAR;

  VAR ival*, slen*: LONGINT;  (*results of Get*)
    rval*: REAL;
    error*: BOOLEAN;

    ch: CHAR;  (*last character read*)
    id: Ident;  (*not exported*)
    strx: INTEGER;  (*index of string table*)
    errpos, errcnt: LONGINT;
    
    keyTab: ARRAY KTsize OF
        RECORD sym: INTEGER; id: ARRAY 14 OF CHAR END;
    str: ARRAY stringTabSize OF LONGINT;
    name* : ARRAY maxStrLen OF CHAR;

(* ------------------------------- non portable between compilers file routines ------------------------- *)
f : Files.File;
(*r: TextRider.Reader;*)
VAR r : Files.Rider;
PROCEDURE OpenFile (name : ARRAY OF CHAR; pos : LONGINT);
(*VAR res : Files.Result;*)
BEGIN
(*
f := Files.Old (name, {Files.read}, res);
IF (res # Files.done) THEN
    Out.String ("failed to open file "); Out.String (name); Out.Ln; HALT(0);
END;
*)
f := Files.Old (name);
(*
r := TextRider.ConnectReader (f);
IF r = NIL THEN
   Out.String ("failed to connect reader"); Out.Ln; HALT(0);
END;*)
IF f = NIL THEN
   Out.String ("failed to open file "); Out.String (name); Out.Ln; HALT(0);
END;
(*
r.SetPos(pos);
*)
Files.Set (r, f, pos);
END OpenFile;

PROCEDURE Pos() : LONGINT;
VAR p : LONGINT;
BEGIN
(*
RETURN r.Pos();
*)
p := Files.Pos(r); RETURN (p);
END Pos;

PROCEDURE Read (VAR ch : CHAR);
BEGIN
(*
r.ReadChar(ch)
*)
Files.Read (r, ch);
END Read;

PROCEDURE EOF() : BOOLEAN;
BEGIN
(*
IF r.res # Files.done THEN RETURN TRUE ELSE RETURN FALSE END
*)
RETURN r.eof;
END EOF;
(* --------------------------------------------------------------------------------------------------------- *)

  PROCEDURE length* (len : ARRAY OF CHAR) : LONGINT;
  VAR k : LONGINT;
  BEGIN
  k := 0;
  REPEAT
  INC(k);
  UNTIL len[k] = 0X;
  RETURN k
  END length;

  PROCEDURE CopyId*(VAR ident: Ident);
  BEGIN COPY(id, ident)
  END CopyId;
  
  PROCEDURE NotEql*(VAR ident: ARRAY OF CHAR): BOOLEAN;
  BEGIN RETURN ident # id
  END NotEql;

  PROCEDURE Mark*(msg: ARRAY OF CHAR);
    VAR p: LONGINT;
  BEGIN p := Pos() - 1;
    IF (p > errpos) & (errcnt < 25) THEN
      Out.String("  pos "); Out.Int(p, 1);
      Out.Char(" "); Out.String(msg);
      Out.Ln;
    END ;
    errpos := p + 3;
    IF msg[0] # "!" THEN error := TRUE; INC(errcnt) END
  END Mark;
  
  PROCEDURE hex(ch: CHAR): LONGINT;
    VAR n: LONGINT;
  BEGIN
    IF (ch >= "0") & (ch <= "9") THEN n := ORD(ch) - 30H
    ELSIF (ch >= "A") & (ch <= "F") THEN n := ORD(ch) - 37H
    ELSE n := 2
    END ;
    RETURN n
  END hex;

PROCEDURE String(VAR sym: INTEGER);
  VAR i: INTEGER;
BEGIN i := 0;
  LOOP Read(ch);
    IF ch = 22X THEN EXIT END ;
       IF ch < " " THEN Mark("Illegal symbol");
          Out.String ("ch="); Out.Char(ch);Out.Ln;
	  Out.String ("ORD(ch)="); Out.Int(ORD(ch),0); Out.Ln;
          EXIT 
       END ;
    IF i < maxStrLen-1 THEN name[i] := ch; INC(i) ELSE Mark("string too long"); i := 0 END
  END ;
  Read(ch);
  IF i = 1 THEN sym := char(*; numtyp := 1; intval := ORD(name[0])*)
  ELSE sym := string; name[i] := 0X
  END;
  slen := length(name)
END String;


(*
  PROCEDURE String(VAR sym: INTEGER);  (*stores string in word array*)
    VAR buf: LONGINT; i, k: INTEGER; eos: CHAR;
  BEGIN eos := ch; Read(ch); i := strx; ival := i; k := 0; buf := 0;
    WHILE ~EOF() & (ch # eos) DO
      IF ch >= " " THEN
        IF i >= stringTabSize-WS THEN Mark("string too long"); i := 0; k := 0 END ;
        buf := ASH(ORD(ch), k*8) + buf; INC(k);
        IF k = WS THEN str[i] := buf; INC(i); k := 0; buf := 0 END
      END ;
      Read(ch)
    END ;
    Read(ch);
    IF (i = ival) & (k = 1) THEN sym := char; ival := buf
    ELSE sym := string; str[i] := buf; slen := ((i - ival) + (k DIV WS) + 1) * WS; INC(i); strx := i
    END
  END String;
*)
  PROCEDURE Identifier(VAR sym: INTEGER);
    VAR i, k: INTEGER;
  BEGIN i := 0; k := 0;
    REPEAT
      IF i < IdLen-1 THEN id[i] := ch; INC(i); k := ORD(ch) + k END ;
      Read(ch)
    UNTIL (ch < "0") OR (ch > "9") & (ch < "A") OR (ch > "Z") & (ch < "a") OR (ch > "z");
    id[i] := 0X; k := k MOD KTsize;  (*hash function*)
    IF (keyTab[k].sym # 0) & (keyTab[k].id = id) THEN sym := keyTab[k].sym
    ELSE k := (k+3) MOD KTsize;
      IF (keyTab[k].sym # 0) & (keyTab[k].id = id) THEN sym := keyTab[k].sym
      ELSE sym := ident
      END
    END
  END Identifier;

  PROCEDURE Number(VAR sym: INTEGER);
    CONST max = 2147483647;
    VAR i, e, n, s: INTEGER; k, h: LONGINT; x: REAL;
      d: ARRAY 16 OF LONGINT;
      negE: BOOLEAN;
  BEGIN ival := 0; i := 0; n := 0; k := 0;
    REPEAT
      IF n < 16 THEN d[n] := ORD(ch)-30H; INC(n) ELSE Mark("too many digits"); n := 0 END ;
      Read(ch)
    UNTIL (ch < "0") OR (ch > "9") & (ch < "A") OR (ch > "F");
    IF (ch = "H") OR (ch = "R") OR (ch = "X") THEN  (*hex*)
      REPEAT h := d[i];
        IF h >= 10 THEN h := h-7 END ;
        k := k*10H + h; INC(i) (*no overflow check*)
      UNTIL i = n;
      IF ch = "X" THEN sym := char; name := "";
        IF k < 100H THEN ival := k ELSE Mark("illegal value"); ival := 0 END
      ELSIF ch = "R" THEN sym := real; rval := SYSTEM.VAL(REAL, k)
      ELSE sym := int; ival := k
      END ;
      Read(ch)
    ELSIF ch # "." THEN  (*decimal integer*)
      REPEAT
        IF d[i] < 10 THEN
          IF k <= (max-d[i]) DIV 10 THEN k := k*10 + d[i] ELSE Mark("too large"); k := 0 END
        ELSE Mark("bad integer")
        END ;
        INC(i)
      UNTIL i = n;
      sym := int; ival := k
    ELSE Read(ch); x := 0.0; e := 0;  (*flt.pt*)
      REPEAT x := x * 10.0 + d[i]; INC(i) UNTIL i = n;  (*integer part*)
      WHILE ("0" <= ch) & (ch <= "9") DO
        x := x * 10.0 + (ORD(ch) - 30H); DEC(e); Read(ch)  (*fraction*)
      END ;
      IF (ch = "E") OR (ch = "D") THEN  (*scale factor*)
        Read(ch); s := 0; 
        IF ch = "-" THEN negE := TRUE; Read(ch)
        ELSE negE := FALSE;
          IF ch = "+" THEN Read(ch) END
        END ;
        IF ("0" <= ch) & (ch <= "9") THEN
          REPEAT s := s*10 + ORD(ch)-30H; Read(ch)
          UNTIL (ch < "0") OR (ch >"9");
          IF negE THEN e := e-s ELSE e := e+s END
        ELSE Mark("digit?")
        END
      END ;
      IF e < 0 THEN
        IF e >= -maxExp THEN x := SHORT(x / Reals.Ten(-e)) ELSE x := 0.0 END
      ELSIF e > 0 THEN
        IF e <= maxExp THEN x := SHORT(Reals.Ten(e) * x) ELSE x := 0.0; Mark("too large") END
      END ;
      sym := real; rval := x
    END
  END Number;
    
  PROCEDURE comment(VAR sym: INTEGER);
  BEGIN
    REPEAT
      REPEAT Read(ch);
        IF ch = "(" THEN Read(ch);
          IF ch = "*" THEN comment(sym) END
        END
      UNTIL (ch = "*") OR EOF();
      REPEAT Read(ch) UNTIL (ch # "*") OR EOF()
    UNTIL (ch = ")") OR EOF();
    IF ~EOF() THEN Read(ch) ELSE Mark("unterminated comment"); sym := eof END
  END comment;
  
  PROCEDURE Asm*( VAR ch : CHAR);
  BEGIN
  Read(ch)
  END Asm;
  
  PROCEDURE Get*(VAR sym: INTEGER);
  BEGIN
    REPEAT
      WHILE ~EOF() & (ch <= " ") DO Read(ch) END;
      IF EOF() THEN sym := eof ELSE
        CASE ch OF
            "!": Read(ch); sym := assert
        |  '"': String(sym)
        |  "#": Read(ch); sym := neq
        |  "&": Read(ch); sym := and
        |  "'": String(sym)
        |  "(": Read(ch);
            IF ch = "*" THEN sym := null; comment(sym) ELSE sym := lparen END
        |  ")": Read(ch); sym := rparen
        |  "*": Read(ch); sym := times
        |  "+": Read(ch); sym := plus
        |  ",": Read(ch); sym := comma
        |  "-": Read(ch); sym := minus
        |  ".": Read(ch);
            IF ch = "." THEN Read(ch); sym := upto ELSE sym := period END
        |  "/": Read(ch); sym := rdiv
        |  "0" .."9": Number(sym);
        |  ":": Read(ch);
            IF ch = "=" THEN Read(ch); sym := becomes ELSE sym := colon END 
        |  ";": Read(ch); sym := semicolon
        |  "<": Read(ch);
            IF ch = "=" THEN Read(ch); sym := leq ELSE sym := lss END
        |  "=": Read(ch); sym := eql
        |  ">": Read(ch);
            IF ch = "=" THEN Read(ch); sym := geq ELSE sym := gtr END
        |  "A" .. "Z": Identifier(sym)
        |  "[": Read(ch); sym := lbrak
        |  "]": Read(ch); sym := rbrak
        |  "^": Read(ch); sym := arrow 
        |  "a".."z": Identifier(sym)
        |  "{": Read(ch); sym := lbrace
        |  "|": Read(ch); sym := bar
        |  "}": Read(ch); sym := rbrace
        |  "~": Read(ch); sym := not
        ELSE Read(ch); Mark("strange character"); sym := null
        END
      END
    UNTIL sym # null
  END Get;

  PROCEDURE MoveStrings*(VAR dst: ARRAY OF LONGINT; VAR dstx: INTEGER; dstlim: INTEGER);
  (*Move string from string buffer at position pos to dst*)
    VAR i, j: INTEGER;
  BEGIN i := dstx; j := 0;
    IF strx < dstlim - dstx THEN
      WHILE j < strx DO dst[i] := str[j]; INC(i); INC(j) END ;
    END ;
    dstx := i; strx := 0  (*reset string table*)
  END MoveStrings;
  
  PROCEDURE Init*(VAR s: ARRAY OF CHAR; pos: LONGINT);
  BEGIN error := FALSE; errpos := pos; errcnt := 0; strx := 0;
  OpenFile (s, pos);
  END Init;

  PROCEDURE EnterKW(sym: INTEGER; name: ARRAY OF CHAR);
    VAR j, k: INTEGER;
  BEGIN j := 0; k := 0;
    REPEAT INC(k, ORD(name[j])); INC(j)
    UNTIL name[j] = 0X;
    k := k MOD 59;  (*hash function*)
    WHILE keyTab[k].sym # 0 DO k := (k + 3) MOD KTsize END ;
    COPY(name, keyTab[k].id); keyTab[k].sym := sym
  END EnterKW;

BEGIN Out.Open; error := TRUE;
  EnterKW(array, "ARRAY");
  EnterKW(begin, "BEGIN");
  EnterKW(by, "BY");
  EnterKW(case, "CASE");
  EnterKW(const, "CONST");
  EnterKW(div, "DIV");
  EnterKW(do, "DO");
  EnterKW(else, "ELSE");
  EnterKW(elsif, "ELSIF");
  EnterKW(end, "END");
  EnterKW(null, "EXIT");
  EnterKW(false, "FALSE");
  EnterKW(for, "FOR");
  EnterKW(if, "IF");
  EnterKW(import, "IMPORT");
  EnterKW(in, "IN");
  EnterKW(is, "IS");
  EnterKW(null, "LOOP");
  EnterKW(mod, "MOD");
  EnterKW(module, "MODULE");
  EnterKW(nil, "NIL");
  EnterKW(of, "OF");
  EnterKW(or, "OR");
  EnterKW(pointer, "POINTER");
  EnterKW(procedure, "PROCEDURE");
  EnterKW(record, "RECORD");
  EnterKW(repeat, "REPEAT");
  EnterKW(return, "RETURN");
  EnterKW(then, "THEN");
  EnterKW(to, "TO");
  EnterKW(true, "TRUE");
  EnterKW(type, "TYPE");
  EnterKW(until, "UNTIL");
  EnterKW(var, "VAR");
  EnterKW(while, "WHILE");
  EnterKW(with, "WITH");
  EnterKW(asm, "ASM");
  EnterKW(dispose, "DISPOSE");
END OSAS.
