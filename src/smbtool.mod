MODULE smbtool;  (*NW 1.12.06 / 15.3.2007*)

IMPORT SYSTEM, args, Files, Texts:=CmdlnTexts,(* Oberon,*) OSAB, Out := OakOut;

VAR (*W: Texts.Writer;*)
   name1: ARRAY 68 OF CHAR;  (*mnemonics*)
   name2: ARRAY 30 OF CHAR;
   name3: ARRAY 64 OF CHAR;
   shmd:  ARRAY 16 OF CHAR;

PROCEDURE Showhelp;
BEGIN
Out.String ('symbol file browser '); Out.Ln;
END Showhelp;

PROCEDURE ReadType(VAR R: Files.Rider);
VAR key, len, ext, lev, size, off: LONGINT;
ref, class, form, readonly: SHORTINT;
name: ARRAY 32 OF CHAR;
BEGIN Files.Read(R, ref); (*Texts.Write(W, " "); Texts.Write(W, "[");*) Out.String (" "); Out.String ("[");
	IF ref < 0 THEN Out.String("^"); Out.Int( -ref, 1)
	ELSE Out.String("#"); Out.Int( ref, 1);
		Files.Read(R, form); Out.String("  form = "); Out.Int( form, 1);
		IF form = OSAB.Pointer THEN ReadType(R)
		ELSIF form = OSAB.Array THEN
			ReadType(R); Files.ReadNum(R, len); Files.ReadNum(R, size);
			Out.String("  len = "); Out.Int( len, 1);
			Out.String("  size = "); Out.Int( size, 1)
		ELSIF form = OSAB.Record THEN
			ReadType(R); Files.ReadNum(R, lev); Out.String("  lev = "); Out.Int( lev, 1);
			Files.ReadNum(R, size); Out.String("  size = "); Out.Int( size, 1);
			Out.String(" "); Out.String("{"); Files.Read(R, class);
			WHILE class # 0 DO
				Out.Ln; Out.String(" class = "); Out.Int( class, 1);
				ReadType(R); Files.ReadNum(R, off); Out.String("  off = "); Out.Int( off, 1); 
				Files.ReadString(R, name); Out.String(" "); Out.String(name);
				Files.Read(R, class)
			END ;
			Out.String("}")
		ELSIF form = OSAB.Proc THEN
			ReadType(R); Out.String("("); Files.Read(R, class);
			WHILE class # 0 DO
				Out.Ln; Out.String(" class = "); Out.Int( class, 1); Files.Read(R, readonly);
				IF readonly = 1 THEN Out.String("#") END ;
					ReadType(R); Files.Read(R, class)
			END ;
			Out.String(")")
		ELSIF form = OSAB.ImpTyp THEN (*reimported type*)
			Out.Ln; Files.ReadLInt(R, key); Files.ReadString(R, name);
			Out.String(name); Files.ReadString(R, name);
			Out.String("."); Out.String(name); (*Texts.WriteHex(W, key);*) Out.Int(key, 0); ReadType(R)
		END
	END ;
	Out.String("]")
END ReadType;

PROCEDURE DecSym*;  (*decde symbol file*)
VAR class: SHORTINT;
k: LONGINT;
name: ARRAY 32 OF CHAR;
F: Files.File; R: Files.Rider;
(*S: Texts.Scanner;*)
BEGIN (*Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S);
	IF S.class = Texts.Name THEN
		Out.String("OSA decode "); Out.String(S.s);*)
IF args.argscount() < 1 THEN Out.String ("Error processing: wrong number of arguments"); Out.Ln; Showhelp; HALT(0) END;
args.arg (1, name);
		F := Files.Old(name);
		IF F # NIL THEN
			Files.Set(R, F, 0); Files.ReadLInt(R, k); (*Texts.Write(W, 9X);*) Out.String (9X);
			Files.ReadString(R, name); (*Texts.WriteString(W, name); Texts.WriteHex(W, k);*)
			                           Out.String (name); Out.Int (k, 0);
			Files.Read(R, class);
			WHILE class # 0 DO
				(*Texts.WriteLn(W); Texts.WriteString(W, " class = "); Out.Int( class, 1);*)
				Out.Ln; Out.String ("class = "); Out.Int(class, 1);
				ReadType(R); Files.ReadNum(R, k); Files.ReadString(R, name);
				(*Texts.Write(W, " "); Texts.WriteString(W, name); Out.Int( k, 5);*)
				Out.String (" "); Out.Int (k, 5);
				Files.Read(R, class)
			END ;
		ELSE (*Texts.WriteString(W, " not found")*) Out.String (" not found");
		END ;
		(*Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf)*)
		Out.Ln;
END DecSym;

(* ---------------------------------------------------

PROCEDURE reg(r: LONGINT);
BEGIN
IF r < 12 THEN Texts.WriteString(W, " R"); Texts.WriteInt(W, r, 1)
ELSIF r = 12 THEN Texts.WriteString(W, " FP")
ELSIF r = 13 THEN Texts.WriteString(W, " SP")
ELSIF r = 14 THEN Texts.WriteString(W, " LNK")
ELSIF r = 15 THEN Texts.WriteString(W, " PC")
END
END reg;

PROCEDURE word(VAR a: ARRAY OF CHAR; k: LONGINT);
VAR i: LONGINT;
BEGIN k := k*4; i := 3;
REPEAT Texts.Write(W, a[k]); INC(k); DEC(i) UNTIL i = 0
END word;

PROCEDURE wordtab(VAR a: ARRAY OF CHAR; k: LONGINT);
VAR i: LONGINT;
BEGIN k := k*4; i := 3;
REPEAT Texts.Write(W, a[k]); INC(k); DEC(i) UNTIL i = 0;
Texts.Write(W, 9X)
END wordtab;

PROCEDURE num(n: LONGINT);
VAR m: LONGINT;
BEGIN m := n DIV 100H; n := SYSTEM.ROT(n MOD 100H, -m*2);
Texts.Write(W, " "); Texts.WriteInt(W, n, 1)
END num;

PROCEDURE opcode(instr: LONGINT);
VAR cond, op, r0, r1, r2, sh: LONGINT;
BEGIN op := instr DIV 100000H MOD 20H;
r0 := instr DIV 10000H MOD 10H;
r1 := instr DIV 1000H MOD 10H;
r2 := instr MOD 1000H;
CASE instr DIV 2000000H MOD 8 OF
	  0: (*register operation*)
		  IF r2 DIV 10H MOD 10H = 9 THEN (*multiply*)
			  wordtab(name2, 6); reg(r0); reg(r1); reg(r2 DIV 100H); reg(r2 MOD 10H)
		  ELSE wordtab(name1, op DIV 2); reg(r1); reg(r0); reg(r2 MOD 10H); sh := r2 DIV 10H;
			  IF ~ODD(sh) THEN
				  IF sh DIV 8 # 0 THEN
					  Texts.Write(W, " "); word(shmd, sh DIV 2 MOD 4); Texts.WriteInt(W, sh DIV 8, 3)
				  END
			  ELSE Texts.Write(W, " "); word(shmd, sh DIV 2 MOD 4); reg(sh DIV 10H)
			  END
		  END
	| 1: (*register operation with immediate operand*)
		wordtab(name1, op DIV 2); reg(r1); reg(r0); num(r2)
	| 2: (*load/store with immediate offset*)
		word(name2, op MOD 2);
		IF ODD(op DIV 4) THEN Texts.Write(W, "B") END ;
		IF ~ODD(op DIV 8) THEN r2 := -r2 END ;
		    Texts.Write(W, 9X); reg(r1); reg(r0); Texts.Write(W, " "); Texts.WriteInt(W, r2, 1)
	| 3: (*load/store with offset in register*)
		word(name2, op MOD 2);
		    IF ODD(op DIV 4) THEN Texts.Write(W, "B") END ;
		    Texts.Write(W, 9X); reg(r1); reg(r0); reg(r2 MOD 10H); sh := r2 DIV 10H;
		    IF sh # 0 THEN Texts.Write(W, " "); word(shmd, sh DIV 2 MOD 4); Texts.WriteInt(W, sh DIV 8, 3) END
	| 4: (*load/store multiple registers*)
		wordtab(name2, op MOD 2 + 2); reg(r0)
	| 5: cond := instr DIV 10000000H MOD 10H;
		IF cond = 14 THEN wordtab(name2, op DIV 10H + 4) ELSE wordtab(name3, cond) END ;
		Texts.WriteInt(W, instr * 100H DIV 100H, 8)
	| 6: Texts.WriteString(W, "UNDEF")
	| 7: Texts.WriteString(W, "SWI")
END
END opcode;
*)
(*
PROCEDURE DecObj*;   (*decode object file*)
VAR class: SHORTINT;
i, key, fix, adr, data, len: LONGINT;
name: ARRAY 32 OF CHAR;
F: Files.File; R: Files.Rider;
S: Texts.Scanner;
BEGIN Texts.OpenScanner(S, (*Oberon.Par.text*), Oberon.Par.pos); Texts.Scan(S);
	IF S.class = Texts.Name THEN
		Texts.WriteString(W, "OSA decode "); Texts.WriteString(W, S.s); F := Files.Old(S.s);
		IF F # NIL THEN
			Files.Set(R, F, 0); Files.ReadString(R, name); Texts.WriteLn(W); Texts.WriteString(W, name);
			Files.ReadLInt(R, key); Texts.WriteHex(W, key);
			Files.ReadLInt(R, fix); Texts.WriteInt(W, fix, 5); Texts.WriteLn(W);
			Texts.WriteString(W, "imports:"); Texts.WriteLn(W);
			Files.ReadString(R, name);
			WHILE name[0] # 0X DO
				Texts.Write(W, 9X); Texts.WriteString(W, name);
				Files.ReadLInt(R, key); Texts.WriteHex(W, key);
				Files.ReadLInt(R, fix); Texts.WriteInt(W, fix, 5); Texts.WriteLn(W);
				Files.ReadString(R, name)
			END ;
			Texts.WriteString(W, "commands:"); Texts.WriteLn(W);
			Files.ReadString(R, name);
			WHILE name[0] # 0X DO
				Texts.Write(W, 9X); Texts.WriteString(W, name);
				Files.ReadLInt(R, adr); Texts.WriteInt(W, adr, 5); Texts.WriteLn(W);
				Files.ReadString(R, name)
			END ;
			Files.ReadLInt(R, len); Texts.WriteString(W, "entries at ");
			WHILE len >= 0 DO
				DEC(len); Files.ReadLInt(R, adr); Texts.WriteInt(W, adr, 6)
			END ;
			Texts.WriteLn(W);
			Files.ReadLInt(R, data); Texts.WriteString(W, "datasize = "); Texts.WriteInt(W, data, 8); Texts.WriteLn(W);
			Files.ReadLInt(R, len); Texts.WriteString(W, "codesize = "); Texts.WriteInt(W, len, 8); Texts.WriteLn(W);
			Texts.Append(Oberon.Log, W.buf);
			i := 0;
			WHILE i < len DO
				Files.ReadLInt(R, data); Texts.WriteInt(W, i, 4); Texts.Write(W, 9X); Texts.WriteHex(W, data);
				Texts.Write(W, 9X); opcode(data); Texts.WriteLn(W); INC(i)
			END
		ELSE Texts.WriteString(W, " not found"); Texts.WriteLn(W)
		END ;
		Texts.Append(Oberon.Log, W.buf)
	END
END DecObj;
*)
BEGIN (*Texts.OpenWriter(W);*)
DecSym;
name1 := "AND XOR SUB RSB ADD ADC SBC RSC TST TEQ CMP CMN OR  MOV BIC MVN ";
name2 := "STR LDR STM LDM BR  BL  MUL ";
name3 := "BEQ BNE BCS BCC BMI BPL BVS BVC BMI BLS BGE BLT BGT BLE BAL BNV";
shmd := "LSL LSR ASR ROR"
END smbtool.
