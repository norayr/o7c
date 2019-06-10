

all: lib hello

lib:
	o7c -s Unix.mod
	o7c -s IntStr.mod
	o7c -s Out.mod

hello:
	o7c -sm hanoi.mod
	ld -o hanoi hanoi.o Out.o IntStr.o Unix.o

clean:
	rm *.o
	rm *.smb
	rm *.s

