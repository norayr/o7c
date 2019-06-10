MODULE not;
VAR a, b, c, d : BOOLEAN;

BEGIN

a := TRUE;
d := TRUE;

b := ~(~((a & d) & (d & a)))
END not.
