MODULE OSACompiler;  

IMPORT Texts, OSAP, OSAS, BootSA, BootSAX, AosIO, Oberon, ErrorGadgets, Display;  

CONST 
  autoMark = TRUE;  
TYPE 
  
  OberonReader = OBJECT (AosIO.Reader);  
  VAR r: Texts.Reader;  
    
    PROCEDURE & Init( T: Texts.Text;  pos: LONGINT );  
    BEGIN 
      Texts.OpenReader( r, T, pos );  
    END Init;  

    PROCEDURE Char( VAR ch: CHAR );  
    BEGIN 
      IF r.eot THEN ch := 0X;  ELSE Texts.Read( r, ch );  END;  
    END Char;  

    PROCEDURE Available( ): LONGINT;  
    BEGIN 
      IF r.eot THEN RETURN 0 ELSE RETURN 1 END;  
    END Available;  

    PROCEDURE Pos( ): LONGINT;  
    BEGIN 
      RETURN Texts.Pos( r );  
    END Pos;  

  END OberonReader;  
  
  OberonWriter = OBJECT (AosIO.Writer)
  VAR W: Texts.Writer;  
    
    PROCEDURE & Init;  
    BEGIN 
      Texts.OpenWriter( W );  
    END Init;  

    PROCEDURE String( s: ARRAY OF CHAR );  
    BEGIN 
      Texts.WriteString( W, s );  
    END String;  

    PROCEDURE Int( n, m: LONGINT );  
    BEGIN 
      Texts.WriteInt( W, n, m );  
    END Int;  

    PROCEDURE Ln( );  
    BEGIN 
      Texts.WriteLn( W );  
    END Ln;  

    PROCEDURE Char( ch: CHAR );  
    BEGIN 
      Texts.Write( W, ch );  
    END Char;  

    PROCEDURE Hex( x, w: LONGINT );  
    BEGIN 
      Texts.WriteHex( W, x );  
    END Hex;  

    PROCEDURE Update( );  
    BEGIN 
      Texts.Append( Oberon.Log, W.buf );  
    END Update;  

  END OberonWriter;  

VAR 
  W: Texts.Writer;  

  PROCEDURE Options( VAR S: Texts.Scanner;  VAR newSF: BOOLEAN );  
  VAR i: INTEGER;  
    options: ARRAY 32 OF CHAR;  
  BEGIN 
    newSF := FALSE;  
    IF (S.class = Texts.Char) & (S.c = Oberon.OptionChar) THEN 
      Texts.Scan( S );  
      IF S.class = Texts.Name THEN 
        i := 0;  COPY( S.s, options );  
        WHILE options[i] # 0X DO 
          CASE options[i] OF 
          "s":       newSF := TRUE 
          ELSE Texts.WriteString( W, "Option not found" );  Texts.WriteLn( W );  
          END;  
          Texts.Append( Oberon.Log, W.buf );  INC( i )
        END;  
        Texts.Scan( S )
      END 
    END 
  END Options;  

  PROCEDURE Compile*;  
  VAR beg, end, time: LONGINT;  S, S1: Texts.Scanner;  T: Texts.Text;  R: OberonReader;  
    W: OberonWriter;  fbeg: LONGINT;  frame: Display.Frame;  newSF: BOOLEAN;  
  BEGIN 
    Texts.OpenScanner( S, Oberon.Par.text, Oberon.Par.pos );  Texts.Scan( S );  
    Options( S, newSF );  NEW( W );  
    IF (S.class = Texts.Char) THEN 
      IF S.c = "*" THEN 
        T := Oberon.MarkedText();  NEW( R, T, 0 );  
        IF T # NIL THEN 
          fbeg := Oberon.Log.len;  
          IF autoMark THEN 
            frame := Oberon.MarkedFrame();  ErrorGadgets.RemoveErrors( T );  
          END;  
          OSAP.Init( R, W, newSF );  OSAP.Module;  
          IF autoMark & OSAS.error THEN 
            Texts.OpenScanner( S1, Oberon.Log, fbeg );  Texts.Scan( S1 );  
            ErrorGadgets.markErrors( S1, T, frame );  
          END;  
        END 
      ELSIF S.c = "@" THEN 
        T := NIL;  time := -1;  Oberon.GetSelection( T, beg, end, time );  
        NEW( R, T, beg );  
        IF (T # NIL ) & (time # -1) THEN 
          OSAP.Init( R, W, newSF );  OSAP.Module
        END 
      END 
    ELSE 
      WHILE S.class = Texts.Name DO 
        NEW( T );  Texts.Open( T, S.s );  NEW( R, T, 0 );  OSAP.Init( R, W, newSF );  
        OSAP.Module;  Texts.Scan( S )
      END 
    END 
  END Compile;  

  PROCEDURE Link*;  
  VAR W: OberonWriter;  S: Texts.Scanner;  i: LONGINT;  
  BEGIN 
    NEW( W );  BootSA.Init( W );  
    Texts.OpenScanner( S, Oberon.Par.text, Oberon.Par.pos );  Texts.Scan( S );  
    IF S.class = Texts.Int THEN 
      i := S.i;  Texts.Scan( S );  
      IF S.class = Texts.Name THEN BootSA.Link( i, S.s ) END 
    END 
  END Link;  
  
  PROCEDURE LinkX (linker: BootSAX.Linker);
  VAR S: Texts.Scanner; fileOut: ARRAY 256 OF CHAR; base: LONGINT; 
  BEGIN
    Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S);
    IF S.class = Texts.Int THEN
      base := S.i; Texts.Scan(S);
      IF S.class = Texts.Name THEN
        COPY(S.s,fileOut); 
        linker.Begin (base, fileOut);
        Texts.Scan(S); 
        WHILE S.class= Texts.Name DO
          linker.Link(S.s); 
          Texts.Scan(S); 
        END;
        linker.End;        
      END; 
    END; 
  END LinkX;
  
  PROCEDURE LinkXFile*;
  VAR W: OberonWriter; linker: BootSAX.Linker;
  BEGIN NEW (W); NEW (linker, W, FALSE, TRUE);
    LinkX(linker);
  END LinkXFile;
  
  PROCEDURE LinkXLoader*;
  VAR W: OberonWriter; linker: BootSAX.Linker;
  BEGIN NEW (W); NEW (linker, W, TRUE, FALSE);
    LinkX(linker);
  END LinkXLoader;
  
  PROCEDURE LinkXImage*;
  VAR W: OberonWriter; linker: BootSAX.Linker;
  BEGIN NEW (W); NEW (linker, W, TRUE, TRUE);
    LinkX(linker);
  END LinkXImage;


BEGIN 
  Texts.OpenWriter( W );  
