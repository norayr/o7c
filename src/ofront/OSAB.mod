MODULE OSAB;  (*NW 14.6.97 / 17.12.98 / 15.10.2007*)
  IMPORT Files := OakFiles, Out := OakOut, OSAS;
  (*Definition of data types Object and Type, which together form the data structure
    called "symbol table". Contins procedures for creation of Objects, and for search:
    NewObj, this, thisimport, thisfield (and OpenScope, CloseScope).
    Handling of import and export, i.e. reading and writing of "symbol files" is done by Import
    and Export. This module contains the list of standard identifiers, with which the symbol table
    (universe), and that of the pseudo-module SYSTEM are initialized. *)

  CONST 
    (* class values*) Head* = 0;
      Const* = 1; Var* = 2; Par* = 3; Fld* = 4; Typ* = 5;
      SProc* = 6; Mod* = 7; Reg* = 8; RegI* = 9;

    (* form values*)
      Byte* = 1; Bool* = 2; Char* = 3; Int* = 4; Real* = 5; Set* = 6;
      Pointer* = 7; NilTyp* = 8; NoTyp* = 9; Proc* = 10;
      String* = 11; Array* = 12; Record* = 13; ImpTyp* = 14;

  TYPE Object* = POINTER TO ObjDesc;
    Type* = POINTER TO TypeDesc;
    Module* = POINTER TO ModDesc;

    ObjDesc*= RECORD
      class*, lev*: INTEGER;
      rdo*, expo*: BOOLEAN;   (*read-only, exported*)
      next*, anc*: Object;
      type*: Type;
      name*: OSAS.Ident;
      val*: LONGINT;
      label*, ilabel*: OSAS.Ident;
    END ;

    TypeDesc* = RECORD
      form*, ref*: INTEGER;  (*ref is only used in "Import"*)
      nofpar*: INTEGER;  (*for procedures, extension level for records*)
      len*: LONGINT;  (*for arrays, len < 0 => open*)
      dsc*, typobj*: Object;
      base*: Type;  (*for arrays, records, pointers*)
      size*: LONGINT  (*in bytes; always multiple of 4, except for Bool and Char*)
    END ;
    
    ModDesc* = RECORD (ObjDesc) name1*: OSAS.Ident END ;

  (* Object classes and the meaning of "val":
    class    val
    ----------
    Var      address
    Par      address
    Const    value
    Fld      offset
    Typ      type descriptor (TD) address
    SProc    inline code number
    Mod      key
    Reg      register number
    RegI     register number

  Type forms and the meaning of "dsc" and "base":
    form     dsc      base
    ------------------------
    Pointer  -        type of dereferenced object
    Proc     params   result type
    Array    -        type of elements
    Record   fields   extension *)
  
  VAR topScope*, guard*, universe, system: Object;
    byteType*, boolType*, charType*: Type;
    intType*, realType*, setType*, nilType*, noType*, strType*: Type;
    nofmod, Ref: INTEGER;
    nameless: OSAS.Ident;

  PROCEDURE NewObj*(): Object; (*insert new Object with name OSAS.id*)
    VAR new, x: Object;
  BEGIN OSAS.CopyId(guard.name); x := topScope;
    WHILE x.next.name # guard.name DO x := x.next END ;
    IF x.next = guard THEN
      NEW(new); new.name := guard.name; new.next := guard; x.next := new 
    ELSE OSAS.Mark("mult def"); 
    END ;
    RETURN x.next 
  END NewObj;
PROCEDURE debugobj(VAR obj : Object);
BEGIN
   (*Out.String ("obj.name="); Out.String (obj.name); Out.Ln;
   (*Out.String ("obj.type.form="); Out.Int (obj.type.form,0); Out.Ln;*)
   Out.String ("obj.class="); Out.Int(obj.class, 0); Out.Ln;
   Out.String ("obj.lev="); Out.Int (obj.lev, 0); Out.Ln;
   Out.String ("obj.rdo="); IF obj.rdo THEN Out.String ("TRUE") ELSE Out.String ("FALSE") END; Out.Ln;
   Out.String ("obj.expo="); IF obj.expo THEN Out.String ("TRUE") ELSE Out.String ("FALSE") END; Out.Ln;
   Out.String ("obj.val="); Out.Int (obj.val, 0); Out.Ln;
   Out.String ("obj.label="); Out.String (obj.label); Out.Ln; Out.Ln;*)
END debugobj;

  PROCEDURE this*(): Object;  (*return the Object with name OSAS.id*)
    VAR s, x: Object;
  BEGIN s := topScope; OSAS.CopyId(guard.name);
     (*Out.String ('this '); Out.Ln; Out.String (guard.name); Out.Ln;
     debugobj(s);*)
    REPEAT x := s.next;
      WHILE x.name # guard.name DO (*debugobj(x);*) x := x.next END ;
      s := s.anc
    UNTIL (x # guard) OR (s = NIL);
    IF x = guard THEN OSAS.Mark("undef"); x.class := Var; x.type := intType; x.val := 0 END ;
    (*Out.String ('return'); debugobj(x);*)
    RETURN x
  END this;
  
  PROCEDURE thisimport*(mod: Object): Object;
    VAR obj: Object;
  BEGIN obj := mod.anc; OSAS.CopyId(guard.name);
  Out.String ('OSAS.CopyId (guard.name)'); Out.Ln; Out.String (guard.name); Out.Ln;
    WHILE obj.name # guard.name DO (*debugobj(obj);*) obj := obj.next END ;
    IF obj = guard THEN OSAS.Mark("undef") END ;
    Out.String ('Return'); (*debugobj(obj);*)
    RETURN obj
  END thisimport;

  PROCEDURE thisfield*(rec: Type): Object;
    VAR fld: Object; name: OSAS.Ident;
  BEGIN OSAS.CopyId(name); fld := rec.dsc;
    WHILE (fld # NIL) & (fld.name # name) DO fld := fld.next END ;
    RETURN fld
  END thisfield;

  PROCEDURE OpenScope*;
    VAR s: Object;
  BEGIN NEW(s); s.class := Head; s.anc := topScope; s.next := guard; topScope := s
  END OpenScope;

  PROCEDURE CloseScope*;
  BEGIN topScope := topScope.anc
  END CloseScope;

  (*------------------------------- Import ---------------------------------*)

  PROCEDURE MakeFileName*(VAR name, FName: OSAS.Ident; ext: ARRAY OF CHAR);
    VAR i, j: INTEGER;
  BEGIN i := 0; j := 0;  (*assume name suffix less than 4 characters*)
    WHILE (i < OSAS.IdLen-5) & (name[i] > 0X) DO FName[i] := name[i]; INC(i) END ;
    REPEAT FName[i]:= ext[j]; INC(i); INC(j) UNTIL ext[j] = 0X;
    FName[i] := 0X
  END MakeFileName;
  
  PROCEDURE ThisModule(VAR name, name1: OSAS.Ident; VAR key: LONGINT): Object;
    VAR obj: Object; mod: Module;
  BEGIN obj := topScope.next; (*search for module*)
    WHILE (obj # guard) & (obj(Module).name1 # name1) DO obj := obj.next END ;
    IF obj = guard THEN  (*insert new module*)
      NEW(mod); mod.class := Mod; mod.name := name; mod.name1 := name1;
      mod.val := key; mod.lev := nofmod; INC(nofmod); mod.type := noType; mod.anc := guard;
      mod.next := topScope.next; topScope.next := mod; obj := mod
    ELSE (*module already present*)
      IF obj.val # key THEN OSAS.Mark("invalid version"); Out.String(name) END ;
      IF name[0] > 0X THEN
        IF obj.name[0] = 0X THEN obj.name := name ELSE OSAS.Mark("multiple import") END
      END
    END ;
    RETURN obj
  END ThisModule;
  
  PROCEDURE Read(VAR R: Files.Rider; VAR x: INTEGER);
    VAR byte: CHAR;
  BEGIN Files.Read(R, byte); x := SHORT(ASH(ASH(LONG(ORD(byte)), 24), -24))  (* sign extension *)
  END Read;
  
  PROCEDURE InType(VAR R: Files.Rider; VAR type: Type; VAR typdesc: ARRAY OF Type);
    VAR key, tmp: LONGINT;
      ref, class, form, np, readonly: INTEGER;
      fld, par, obj, mod: Object;
      t: Type;
      typname, modname: OSAS.Ident;
  BEGIN Read(R, ref);
    IF ref < 0 THEN t := typdesc[-ref];
    ELSE Read(R, form);
      IF form = ImpTyp THEN (*re-imported type, module anchor*)
        Files.ReadLInt(R, key); Files.ReadString(R, modname); Files.ReadString(R, typname);
        InType(R, t, typdesc); mod := ThisModule(nameless, modname, key);
        obj := mod.anc; (*search for type*)
        WHILE (obj # guard) & (obj.name # typname) DO obj := obj.next END ;
        IF obj = guard THEN (*this type has not yet been imported*)
          NEW(obj); obj.name := typname; obj.class := Typ; obj.lev := -mod.lev;
          obj.anc := mod; obj.type := t; typdesc[ref] := t; t.typobj := obj; obj.next := mod.anc; mod.anc := obj
        ELSE (*module and type already present*) t := obj.type; typdesc[ref] := t
        END
      ELSE NEW(t); t.form := form;;
        IF ref > 0 THEN Out.String ('typdesc[ref] := t'); Out.Ln;
	   Out.String ('ref='); Out.Int(ref,0); Out.Ln;
	   Out.String ('len of typdesc is'); Out.Int(LEN(typdesc),0); Out.Ln;
	   typdesc[ref] := t ; Out.String ('here');
	END ;
        IF form = Pointer THEN InType(R, t.base, typdesc); t.size := 4
        ELSIF form = Array THEN InType(R, t.base, typdesc);
          Files.ReadNum(R, key); t.len := SHORT(key); Files.ReadNum(R, t.size); 
        ELSIF form = Record THEN
          InType(R, t.base, typdesc);
          IF t.base.form = NoTyp THEN t.base := NIL; obj := NIL ELSE obj := t.base.dsc END ;
          Files.ReadNum(R, tmp); t.nofpar := SHORT(tmp); Files.ReadNum(R, t.size);
          Read(R, class);
          WHILE class # 0 DO
            NEW(fld); fld.class := class; fld.expo := TRUE; InType(R, fld.type, typdesc); Files.ReadNum(R, fld.val);
            fld.next := obj; Files.ReadString(R, fld.name); obj := fld; Read(R, class)
          END ;
          t.dsc := obj
        ELSIF form = Proc THEN
          InType(R, t.base, typdesc); obj := guard; np := 0; Read(R, class);
          WHILE class # 0 DO
            NEW(par); par.class := class; Read(R, readonly);
            IF readonly = 1 THEN par.rdo := TRUE ELSE par.rdo := FALSE END ; 
            InType(R, par.type, typdesc); par.next := obj; obj := par; INC(np); Read(R, class)
          END ;
          t.dsc := obj; t.nofpar := np; t.size := 4
        END
      END
    END ;
    type := t
  END InType;
  
  PROCEDURE Import*(VAR modid, modid1: OSAS.Ident);
    CONST Ttablen = 200(*28*);
    VAR i, key:LONGINT; class: INTEGER;
      obj, mod, new: Object;
      modname, fname: OSAS.Ident;
      F: Files.File; R: Files.Rider;
      typdesc: ARRAY Ttablen OF Type; ch : CHAR;
  BEGIN key := 0;
    IF modid = "SYSTEM" THEN
      mod := ThisModule(modid, modid, key); mod.anc := system
    ELSE MakeFileName(modid1, fname, ".smb"); F := Files.Old(fname); Out.String ('filename is '); Out.String (fname); Out.Ln;
      IF F # NIL THEN
        Files.Set(R, F, 0); (*Files.ReadLInt(R, key)*) Files.Read (R, ch); (* supposed to be 0 temp *)
	Out.String ('opened smb file'); Out.Ln;
	Out.String ('key = '); Out.Int(key,0); Out.Ln;
	
	Files.ReadString(R, modname);
	
	Out.String ('modname is '); Out.String (modname); Out.Ln;

        typdesc[Byte] := byteType; typdesc[Bool] := boolType; typdesc[Char] := charType; typdesc[Int] := intType;
        typdesc[Real] := realType; typdesc[Set] := setType; typdesc[String] := strType; typdesc[NoTyp] := noType;
        mod := ThisModule(modid, modid1, key);
        Read(R, class);
        WHILE class # 0 DO
          NEW(new); new.class := class; InType(R, new.type, typdesc);
          IF class = Const THEN
            IF new.type.form = Real THEN Files.ReadLInt(R, new.val) ELSE Files.ReadNum(R, new.val) END
          ELSIF class = Typ THEN
            new.type.typobj := new; new.anc := mod; Files.ReadNum(R, new.val);
          ELSIF class = Var THEN Files.ReadNum(R, new.val); new.rdo := TRUE;
            IF new.type.form = String THEN Files.ReadNum(R, i); (*len*) new.val := i*10000H + new.val END
          END ;
          Files.ReadString(R, new.name); new.lev := -mod.lev;
          obj := mod.anc;
          WHILE (obj # guard) & (obj.name # new.name) DO obj := obj.next END ;
          IF obj = guard THEN  (*insert new object*) new.next := mod.anc; mod.anc := new
          ELSIF obj.class = Typ THEN i := ImpTyp;  (*this type is already in typdesc*)
            REPEAT INC(i) UNTIL (i = Ttablen) OR (typdesc[i] = new.type);
            IF i < Ttablen THEN typdesc[i] := obj.type END
          END ;
          Read(R, class)
        END
      ELSE OSAS.Mark("import not available")
      END
    END
  END Import;
  
  (*-------------------------------- Export ---------------------------------*)

  PROCEDURE Write(VAR R: Files.Rider; x: INTEGER);
  BEGIN Files.Write(R, CHR(x))  (* -128 <= x < 128 *)
  END Write;

  PROCEDURE OutType(VAR R: Files.Rider; t: Type);
    VAR obj, last: Object; class: INTEGER;
    
    PROCEDURE OutPar(VAR R: Files.Rider; par: Object; n: INTEGER);
      VAR cl: INTEGER;
    BEGIN
      IF n > 0 THEN
        OutPar(R, par.next, n-1); cl := par.class;
        IF cl = Reg THEN cl := Var ELSIF cl = RegI THEN cl := Par END ;
        Write(R, cl);
        IF par.rdo THEN Write(R, 1) ELSE Write(R, 0) END ;
        OutType(R, par.type)
      END
    END OutPar;

  BEGIN
    IF t.ref > 0 THEN Write(R, -t.ref)
    ELSE
      IF t.typobj # NIL THEN
        t.ref := Ref; Write(R, Ref); INC(Ref);
        IF t.typobj.lev < 0 THEN (*imported type*);
          Write(R, ImpTyp); Files.WriteLInt(R, t.typobj.anc.val); (*key*) 
          Files.WriteString(R, t.typobj.anc(Module).name1); Files.WriteString(R, t.typobj.name);
          Write(R, Ref-1)
        END
      ELSE Write(R, 0)
      END ;
      Files.Write(R, SHORT(t.form));
      IF t.form = Pointer THEN OutType(R, t.base)
      ELSIF t.form = Array THEN OutType(R, t.base); Files.WriteNum(R, t.len); Files.WriteNum(R, t.size)
      ELSIF t.form = Record THEN
        IF t.base # NIL THEN OutType(R, t.base); last := t.base.dsc; 
        ELSE OutType(R, noType); last := NIL
        END ;
        Files.WriteNum(R, t.nofpar); Files.WriteNum(R, t.size); obj := t.dsc;
        WHILE obj # last DO
          IF obj.expo THEN
            Files.Write(R, SHORT(obj.class)); OutType(R, obj.type);
            Files.WriteNum(R, obj.val); (*offset*) Files.WriteString(R, obj.name)
          END ;
          obj := obj.next
        END ;
        Write(R, 0)
      ELSIF t.form = Proc THEN
        OutType(R, t.base); OutPar(R, t.dsc, t.nofpar); Write(R, 0)
      END
    END
  END OutType;

  PROCEDURE Export*(VAR modid: OSAS.Ident;
      VAR newSF: BOOLEAN; VAR key: LONGINT);
      VAR  x, sum, oldkey, expno: LONGINT;
        obj: Object;
        filename: OSAS.Ident;
        F: Files.File; R: Files.Rider;
	syma : ARRAY 10000 OF CHAR; (* warning, to be changed*) ch : CHAR; ind, in : LONGINT;
  BEGIN Ref := ImpTyp + 1; expno := 1; MakeFileName(modid, filename, ".smb");
    F := Files.Old(filename); 
    IF F # NIL THEN Files.Set(R, F, 0); Files.ReadLInt(R, oldkey) END ;
    key := oldkey;
    F := Files.New(filename); Files.Set(R, F, 0);
    Files.WriteLInt(R, 0); Files.WriteString(R, modid);
    obj := topScope.next;
    WHILE obj # guard DO
      Write(R, obj.class); OutType(R, obj.type);
      IF obj.class = Const THEN
        IF obj.type.form = Proc THEN Files.WriteNum(R, expno); INC(expno)
        ELSIF obj.type.form = Real THEN Files.WriteLInt(R, obj.val)
        ELSE Files.WriteNum(R, obj.val)
        END;
      ELSIF (obj.class = Var) OR (obj.class = Typ) THEN
        Files.WriteNum(R, expno); INC(expno);
        IF obj.type.form = String THEN  (*clear length for OSAG.Close*)
          Files.WriteNum(R, obj.val DIV 10000H); obj.val := obj.val MOD 10000H
        END
      END ;
      Files.WriteString(R, obj.name); obj := obj.next
    END ;
    REPEAT Write(R, 0) UNTIL Files.Length(F) MOD 4 = 0;
    Files.Close(F);
    (*Files.Set(R, F, 0); sum := 0;
    WHILE ~R.eof DO Files.ReadLInt(R, x);Out.String ('sum '); Out.Int(sum,0); Out.Ln; sum := sum + x END ;
    IF newSF THEN
      IF sum # oldkey THEN 
        newSF := TRUE; key := sum; Files.Set(R, F, 0); Files.WriteLInt(R, sum);
        Files.Register(F)  (*insert checksum at the beginning*)
      ELSE newSF := FALSE
      END
    ELSIF sum # oldkey THEN OSAS.Mark("new symbol file inhibited")
    END*) Files.Register(F)
  END Export;

  PROCEDURE Init*;
  BEGIN topScope := universe; nofmod := 1
  END Init;
  
  PROCEDURE type(form: INTEGER; size: LONGINT): Type;
    VAR tp: Type;
  BEGIN NEW(tp); tp. form := form; tp.size := size; tp.ref := form; tp.base := NIL; RETURN tp
  END type;

  PROCEDURE enter(name: ARRAY OF CHAR; cl: INTEGER; type: Type; n: LONGINT);
    VAR obj: Object;
  BEGIN NEW(obj);
    COPY(name, obj.name); obj.class := cl; obj.type := type; obj.val := n; obj.anc := NIL;
    IF type # NIL THEN type.typobj := obj END ;
    obj.next := system; system := obj
  END enter;
  
BEGIN Out.Open; nameless[0] := 0X;
  byteType := type(Byte, 1);
  boolType := type(Bool, 1);
  charType := type(Char, 1);
  intType := type(Int, 4);
  realType := type(Real, 4);
  setType := type(Set, 4);
  nilType := type(NilTyp, 4);
  noType := type(NoTyp, 4);
  strType := type(String, 8);
  NEW(guard); guard.class := Var; guard.type := intType; guard.val := 0; guard.next := guard;
  
  (*initialize universe with data types and in-line procedures*)
  system := guard;  (*n = procno*4 + nofpar*)
  enter("ASR", SProc, NIL, 122);  (*functions*)
  enter("LSR", SProc, NIL, 118);
  enter("LSL", SProc, NIL, 114);
  enter("CHR", SProc, NIL, 105);
  enter("ORD", SProc, NIL, 101);
  enter("FLT", SProc, NIL, 97);
  enter("FLOOR", SProc, NIL, 93);
  enter("ODD", SProc, NIL, 89);
  enter("ABS", SProc, NIL, 85);
  enter("LEN", SProc, NIL, 81);
  enter("UNPK", SProc, NIL, 22);  (*procedures*)
  enter("PACK", SProc, NIL, 18);
  enter("NEW", SProc, NIL, 13);
  enter("ASSERT", SProc, NIL, 9);
  enter("DEC", SProc, NIL, 5);
  enter("INC", SProc, NIL, 1);
  enter("SET", Typ, setType, 0);   (*types*)
  enter("BOOLEAN", Typ, boolType, 0);
  enter("CHAR", Typ, charType, 0);
  enter("REAL", Typ, realType, 0);
  enter("LONGINT", Typ, intType, 0);
  enter("INTEGER", Typ, intType, 0);
  topScope := NIL; OpenScope; topScope.next := system; universe := topScope;
  
  system := guard;  (* initialize "unsafe" pseudo-module SYSTEM*)
  enter("FP", Reg, intType, 12);  (*registers *)
  enter("SP", Reg, intType, 13);
  enter("LNK", Reg, intType, 14);
  enter("PC", Reg, intType, 15);
  enter("BYTE", Typ, byteType, 0);
  enter("XOR", SProc, NIL, 154);  (*functions*)
  enter("OVFL", SProc, NIL, 149);
  enter("NULL", SProc, NIL, 145);
  enter("SIZE", SProc, NIL, 141);
  enter("ADR", SProc, NIL, 137);
  enter("VAL", SProc, NIL, 134);
  enter("BIT", SProc, NIL, 130);
  enter("ROR", SProc, NIL, 126);
  enter("MULD", SProc, NIL, 59);  (*procedures*)
  enter("ADDC", SProc, NIL, 55);
  enter("FLUSH", SProc, NIL, 49);
  enter("STCPR", SProc, NIL, 47);
  enter("LDCPR", SProc, NIL, 43);
  enter("STPSR", SProc, NIL, 38);
  enter("LDPSR", SProc, NIL, 34);
  enter("PUT", SProc, NIL, 30);
  enter("GET", SProc, NIL, 26);
END OSAB.
