

all: lib hello

lib:
	o7c -s Unix.mod
	o7c -s IntStr.mod
	o7c -s Out.mod

hello:
	o7c -sm hello.mod
	ld -o hello hello.o Out.o IntStr.o Unix.o -lc -dynamic-linker /lib/ld-linux.so.2

clean:
	rm *.o
	rm *.smb
	rm *.s

