MODULE OSATool;  (*NW 1.12.06 / 12.2.2007*)
  IMPORT SYSTEM, Files, Texts, Oberon, OSAB;
  VAR W: Texts.Writer;
    name1: ARRAY 68 OF CHAR;  (*mnemonics*)
    name2: ARRAY 30 OF CHAR;
    name3: ARRAY 64 OF CHAR;
  
  PROCEDURE ReadType(VAR R: Files.Rider);
    VAR key, len, ext, size, off: LONGINT;
      ref, class, form, readonly: SHORTINT;
      name: ARRAY 32 OF CHAR;
  BEGIN Files.Read(R, ref); Texts.Write(W, " "); Texts.Write(W, "[");
    IF ref < 0 THEN Texts.Write(W, "^"); Texts.WriteInt(W, -ref, 1)
    ELSE Texts.WriteInt(W, ref, 1);
      Files.Read(R, form); Texts.WriteString(W, "  form = "); Texts.WriteInt(W, form, 1);
      IF form = OSAB.Pointer THEN ReadType(R)
      ELSIF form = OSAB.Array THEN
        ReadType(R); Files.ReadNum(R, len); Files.ReadNum(R, size);
        Texts.WriteString(W, "  len = "); Texts.WriteInt(W, len, 1);
        Texts.WriteString(W, "  size = "); Texts.WriteInt(W, size, 1)
      ELSIF form = OSAB.Record THEN
        Texts.WriteString(W, " Record "); ReadType(R); 
        Files.ReadNum(R, size); Texts.WriteString(W, "  size = "); Texts.WriteInt(W, size, 1);
        Texts.Write(W, " "); Texts.Write(W, "{"); Files.Read(R, class);
        WHILE class # 0 DO
          Texts.WriteLn(W); Texts.WriteString(W, " class = "); Texts.WriteInt(W, class, 1);
          ReadType(R); Files.ReadNum(R, off); Texts.WriteString(W, "  off = "); Texts.WriteInt(W, off, 1); 
          Files.ReadString(R, name); Texts.Write(W, " "); Texts.WriteString(W, name);
          Files.Read(R, class)
        END ;
        Texts.Write(W, "}")
      ELSIF form = OSAB.Proc THEN
        ReadType(R); Texts.Write(W, "("); Files.Read(R, class);
        WHILE class # 0 DO
          Texts.WriteLn(W); Texts.WriteString(W, " class = "); Texts.WriteInt(W, class, 1); Files.Read(R, readonly);
          IF readonly = 1 THEN Texts.Write(W, "#") END ;
          ReadType(R); Files.Read(R, class)
        END ;
        Texts.Write(W, ")")
      ELSIF form = OSAB.ImpTyp THEN (*reimported type*)
        Texts.WriteLn(W); Files.ReadLInt(R, key); Files.ReadString(R, name);
        Texts.WriteString(W, "reexported "); Texts.WriteString(W, name); Files.ReadString(R, name);
        Texts.WriteString(W, " as "); Texts.WriteString(W, name)
      END
    END ;
    Texts.Write(W, "]")
  END ReadType;

  PROCEDURE DecSym*;
    VAR class: SHORTINT;
      k: LONGINT;
      name: ARRAY 32 OF CHAR;
      F: Files.File; R: Files.Rider;
      S: Texts.Scanner;
  BEGIN Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S);
    IF S.class = Texts.Name THEN
      Texts.WriteString(W, "OSA decode "); Texts.WriteString(W, S.s); F := Files.Old(S.s);
      IF F # NIL THEN
        Files.Set(R, F, 0); Files.ReadLInt(R, k); Texts.Write(W, 9X);
        Files.ReadString(R, name); Texts.WriteString(W, name); Texts.WriteHex(W, k);
        Files.Read(R, class);
        WHILE class # 0 DO
          Texts.WriteLn(W); Texts.WriteString(W, " class = "); Texts.WriteInt(W, class, 1);
          ReadType(R); Files.ReadNum(R, k); Files.ReadString(R, name);
          Texts.Write(W, " "); Texts.WriteString(W, name); Texts.WriteInt(W, k, 5);
          Files.Read(R, class)
        END ;
      ELSE Texts.WriteString(W, " not found")
      END ;
      Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf)
    END
  END DecSym;
  
(* ---------------------------------------------------*)

  PROCEDURE hex(VAR z: LONGINT);
    VAR x: LONGINT;
  BEGIN z := SYSTEM.ROT(z, 4); x := z MOD 10H;
    IF x >= 10 THEN INC(x, 7) END ;
    Texts.Write(W, CHR(x+30H))
  END hex;

  PROCEDURE word(VAR a: ARRAY OF CHAR; k: LONGINT);
    VAR i: LONGINT;
  BEGIN k := k*4; i := 3;
    REPEAT Texts.Write(W, a[k]); INC(k); DEC(i) UNTIL i = 0
  END word;
  
  PROCEDURE opcode(instr: LONGINT);
    VAR op, cond: LONGINT;
  BEGIN op := instr DIV 100000H MOD 20H;
    CASE instr DIV 2000000H MOD 8 OF
      0:  IF instr DIV 16 MOD 16 = 9 THEN word(name2, 6)
        ELSE word(name1, op DIV 2); Texts.Write(W, "R")
        END
    | 1: word(name1, op DIV 2); Texts.Write(W, "I")
    | 2: word(name2, op MOD 2);
          IF ODD(op DIV 4) THEN Texts.Write(W, "B") END
    | 3: word(name2, op MOD 2); Texts.Write(W, "R")
    | 4: word(name2, op MOD 2 + 2)
    | 5: cond := instr DIV 10000000H MOD 10H;
      IF cond = 14 THEN word(name2, op DIV 16 + 4) ELSE word(name3, cond) END
    | 6: Texts.WriteString(W, "UNDEF")
    | 7: Texts.WriteString(W, "SWI")
    END
  END opcode;
  
  PROCEDURE DecObj*;
    VAR class: SHORTINT;
      i, key, fix, adr, data, len: LONGINT;
      name: ARRAY 32 OF CHAR;
      F: Files.File; R: Files.Rider;
      S: Texts.Scanner;
  BEGIN Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S);
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
          Files.ReadLInt(R, data); Texts.WriteInt(W, i, 3); Texts.Write(W, 9X);
          hex(data); hex(data); hex(data); Texts.Write(W, " ");
          hex(data); hex(data); hex(data); hex(data); hex(data);
          Texts.Write(W, 9X); opcode(data); Texts.WriteLn(W); INC(i)
        END
      ELSE Texts.WriteString(W, " not found"); Texts.WriteLn(W)
      END ;
      Texts.Append(Oberon.Log, W.buf)
    END
  END DecObj;

BEGIN Texts.OpenWriter(W);
  name1 := "AND XOR SUB RSB ADD ADC SBC RSC TST TEQ CMP CMN OR  MOV BIC MVN ";
  name2 := "STR LDR STM LDM BR  BSR MUL ";
  name3 := "BEQ BNE BCS BCC BMI BPL BVS BVC BMI BLS BGE BLT BGT BLE BAL BNV";
END OSATool.
