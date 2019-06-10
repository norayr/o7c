

all: lib hello

lib:
	o7c -s Unix.mod

hello:
	o7c -sm test.mod
	ld -o test test.o Unix.o -lc -dynamic-linker /lib/ld-linux.so.2

clean:
	rm *.o
	rm *.smb
	rm *.s

