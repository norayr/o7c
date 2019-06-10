MODULE libc;
IMPORT SYSTEM;
PROCEDURE -includeStdio()
   "#include <stdio.h>";

PROCEDURE -sys(str: ARRAY OF CHAR): INTEGER
   "system(str)";

PROCEDURE system*(cmd : ARRAY OF CHAR);
VAR r : INTEGER;
BEGIN
r := sys(cmd);
END system;


BEGIN


END libc.
