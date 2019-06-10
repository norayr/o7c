MODULE smbtool;  (*NW 1.12.06 / 15.3.2007*)

IMPORT (*SYSTEM,*) args, Files, BinaryRider, TextRider, StdChannels, OSAB;

VAR W : TextRider.Writer;
   name1: ARRAY 68 OF CHAR;  (*mnemonics*)
   name2: ARRAY 30 OF CHAR;
   name3: ARRAY 64 OF CHAR;
   shmd:  ARRAY 16 OF CHAR;

PROCEDURE Showhelp;
BEGIN
W.WriteString ('symbol file browser '); W.WriteLn;
END Showhelp;

PROCEDURE ReadType(VAR R: BinaryRider.Reader);
VAR key, len,(* ext,*) lev, size, off: LONGINT;
ref, class, form, readonly: SHORTINT;
name: ARRAY 32 OF CHAR;
BEGIN 
R.ReadSInt(ref); 
	(*Texts.Write(W, " "); Texts.Write(W, "[");*)
	W.WriteChar(" "); W.WriteChar("[");
	IF ref < 0 THEN 
	W.WriteString("^"); W.WriteInt(-ref, 1);
	ELSE W.WriteString("#"); W.WriteInt(ref, 1);
		R.ReadSInt(form); W.WriteString("  form = "); W.WriteLInt(form, 1);
		IF form = OSAB.Pointer THEN ReadType(R)
		ELSIF form = OSAB.Array THEN
			ReadType(R); R.ReadNum(len); R.ReadNum(size);
			W.WriteString("  len = "); W.WriteLInt( len, 1);
			W.WriteString("  size = "); W.WriteLInt( size, 1)
		ELSIF form = OSAB.Record THEN
			ReadType(R); R.ReadNum(lev); W.WriteString("  lev = "); W.WriteLInt( lev, 1);
			R.ReadNum(size); W.WriteString("  size = "); W.WriteLInt(size, 1);
			W.WriteString(" "); W.WriteString("{"); R.ReadSInt(class);
			WHILE class # 0 DO
				W.WriteLn; W.WriteString(" class = "); W.WriteInt(class, 1);
				ReadType(R); R.ReadNum(off); W.WriteString("  off = "); W.WriteLInt( off, 1); 
				R.ReadString(name); W.WriteString(" "); W.WriteString(name);
				R.ReadSInt(class)
			END ;
			W.WriteString("}")
		ELSIF form = OSAB.Proc THEN
			ReadType(R); W.WriteString("("); R.ReadSInt(class);
			WHILE class # 0 DO
				W.WriteLn; W.WriteString(" class = "); W.WriteLInt( class, 1); R.ReadSInt(readonly);
				IF readonly = 1 THEN W.WriteString("#") END ;
					ReadType(R); R.ReadSInt(class)
			END ;
			W.WriteString(")")
		ELSIF form = OSAB.ImpTyp THEN (*reimported type*)
			W.WriteLn; R.ReadLInt(key); R.ReadString(name);
			W.WriteString(name); R.ReadString(name);
			W.WriteString("."); W.WriteString(name); W.WriteHex(key, 0); ReadType(R)
		END
	END ;
	W.WriteString("]")
END ReadType;

PROCEDURE DecSym*;  (*decde symbol file*)
VAR class: SHORTINT;
k: LONGINT;
name: ARRAY 32 OF CHAR;
F: Files.File; R: BinaryRider.Reader;
(*S: Texts.Scanner;*) res : Files.Result;
BEGIN (*Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S);
	IF S.class = Texts.Name THEN
		Out.String("OSA decode "); Out.String(S.s);*)
IF args.argscount() < 1 THEN W.WriteString ("Error processing: wrong number of arguments"); W.WriteLn; Showhelp; HALT(0) END;
args.arg (1, name);
		F := Files.Old(name, {Files.read}, res);
		(*IF F # NIL THEN*)
		IF res = Files.done THEN
			(*Files.Set(R, F, 0); Files.ReadLInt(R, k); *)
			R := BinaryRider.ConnectReader (F); R.SetPos(0); R.ReadLInt(k);
			(*Texts.Write(W, 9X);*) W.WriteChar(9X);
			(*Files.ReadString(R, name); *)
			R.ReadString (name);
			(*Texts.WriteString(W, name); Texts.WriteHex(W, k);*)
			W.WriteString (name); W.WriteHex(k, 0);
			(*Files.Read(R, class);*)
			R.ReadSInt (class);
			WHILE class # 0 DO
				(*Texts.WriteLn(W); Texts.WriteString(W, " class = "); Out.Int( class, 1);*)
				W.WriteLn; W.WriteString (" class = "); W.WriteInt (class, 1);
				ReadType(R); 
				(*Files.ReadNum(R, k); Files.ReadString(R, name);*)
				R.ReadNum(k); R.ReadString (name);
				(*Texts.Write(W, " "); Texts.WriteString(W, name); Out.Int( k, 5);*)
				W.WriteString(" "); W.WriteString(name); W.WriteLInt(k, 5);
				(*Files.Read(R, class)*)
				R.ReadSInt(class);
			END ;
		ELSE (*Texts.WriteString(W, " not found")*) W.WriteString (" not found");
		END ;
		(*Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf)*)
		W.WriteLn;
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
BEGIN W := TextRider.ConnectWriter(StdChannels.stdout);
DecSym;
name1 := "AND XOR SUB RSB ADD ADC SBC RSC TST TEQ CMP CMN OR  MOV BIC MVN ";
name2 := "STR LDR STM LDM BR  BL  MUL ";
name3 := "BEQ BNE BCS BCC BMI BPL BVS BVC BMI BLS BGE BLT BGT BLE BAL BNV";
shmd := "LSL LSR ASR ROR"
END smbtool.
