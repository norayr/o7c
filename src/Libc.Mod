MODULE Libc;
IMPORT OS:ProcessManagement, Object;

PROCEDURE system*(VAR a : ARRAY OF CHAR);
VAR i : LONGINT;
BEGIN
i := ProcessManagement.system(Object.NewLatin1(a));
END system;

END Libc.

