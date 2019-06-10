MODULE IntStr;
VAR string : ARRAY 16 OF CHAR;
charrr : CHAR;
rrr : INTEGER;
PROCEDURE Reverse (VAR str : ARRAY OF CHAR; start, end : INTEGER);
(* Reverses order of characters in the interval [start..end]. *)
VAR
  h : CHAR;
BEGIN
   WHILE start < end DO
    h := str[start]; str[start] := str[end]; str[end] := h;
    INC(start); DEC(end)
  END
END Reverse;

PROCEDURE IntToStr*(int: LONGINT; VAR str: ARRAY OF CHAR);
CONST
  maxLength = 11; (* maximum number of digits representing a LONGINT value *)
VAR
  b : ARRAY maxLength+1 OF CHAR;
  s, e, t: INTEGER;
BEGIN
  (* build representation in string 'b' *)
    IF int < 0 THEN               (* negative sign *)
      b[0] := "-"; int := -int; s := 1;
      rrr := int;
    ELSE  (* no sign *)
      s := 0
    END;
    e := s;                       (* 's' holds starting position of string *)
    REPEAT
      t := int MOD 10;
      t := t + ORD("0");
      b[e] := CHR(t);
      charrr := b[e];
      int := int DIV 10;
      INC(e)
    UNTIL int = 0;
    b[e] := 0X;
    Reverse(b, s, e-1);
  (*rrr := 0;*)
  (*WHILE b[rrr] # 0X DO INC(rrr); charrr := b[rrr] END;*)
  charrr := b[0];
  charrr := b[1];
  str := b;
  charrr := str[0];
  charrr := str[1];

    (*rrr := 0;
  WHILE str[rrr] # 0X DO INC(rrr); charrr := b[rrr] END;*)

END IntToStr;

END IntStr.

