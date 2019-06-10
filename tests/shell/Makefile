

all: lib shell

lib:
	o7c -s Unix.mod
#	o7c -s IntStr.mod
#	o7c -s Out.mod

shell:
	o7c -sm shell.mod
	ld -o shell shell.o Unix.o

clean:
	rm *.o
	rm *.smb
	rm *.s

