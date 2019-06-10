MODULE test;
(*   IMPORT out, test2;*)
TYPE R = RECORD
x, y : INTEGER
END;

VAR s : SET;
a,b : INTEGER;
v : CHAR;
arr : ARRAY 6 OF INTEGER;
str, strr : ARRAY 10 OF CHAR;
r : R;
(*
PROCEDURE test2 (VAR i : ARRAY OF CHAR);
VAR p : CHAR;
BEGIN

p := "A";
i[2] := p;
(*
p := i[2]*)
END test2;
*)
PROCEDURE test(VAR i: INTEGER ; VAR ch : CHAR; j : INTEGER);
VAR local : INTEGER;
bukva : CHAR;
(*     PROCEDURE  inkrement (a : INTEGER);
     BEGIN
     a := a + 1;
     END inkrement;*)
BEGIN
local := 0;
i := i + j;
ch := "A";
bukva := ch;
v := bukva
END test;
PROCEDURE test3 (CONST s : ARRAY OF CHAR);
BEGIN

END test3;
(*
PROCEDURE rectest (a : INTEGER);
BEGIN
a := a - 1;
IF a # 0 THEN rectest(a) END;
END rectest;
*)
BEGIN
str := "aaaaaabbb";
test3("qoqoqo");
test3(str);
str[0] := "a";
str[1] := "b";
str[2] := "c";
str[3] := "d";
str[4] := "e";
str[5] := "f";
str[6] := "g";
str[7] := "h";
str[8] := "i";
str[9] := "j";
arr[0] := 0;
arr[1] := 1;
arr[2] := 2;
arr[3] := 3;
strr[0] := "0";
strr[1] := "1";
strr[2] := "2";
strr[3] := "3";
strr[4] := "4";
strr[5] := "5";
strr[6] := "6";
strr[7] := "7";
strr[8] := "8";
strr[9] := "9";

str := strr;
(*
out.str("aaaaaaaaa", 10);*)
b := 5;
a := 3;
(*
a := test2.a;*)
(*
rectest(a);
*)
v := "a";


(*
test2(str);
*)
(*
out.str(str,10);*)
(*
ar[0,0] :=  0;
ar[0,1] := 1;
ar[0,2] := 2;
ar[1,0] := 1;
ar[2,0] := 2;
ar[2,2] := 4;
ar[b, 0] := 0;
ar[1, 2] := 1;
a := ar[1,0];
*)
(*s :={};
s := {0} + {a};
s := s + {a};
a := 5;
b := 2;
a := (a * 2 - (3*b) + 6 DIV b)+(5*a) -3 + a;*)
END test.
