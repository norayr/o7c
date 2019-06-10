SETPATH = OBERON=".:ofront:.:ofront/system:ofront/ooc"
OFRONT = $(SETPATH) ./Ofront -sa
CC = cc -c -g
CL = cc  
AR = ar rc

all: system ooclibs compiler

system:
	    cp ofront/system/SYSTEM.h .
	    $(OFRONT) SYSTEM.mod -iapx Args.mod Console.mod OakOut.mod Out.mod \
	     Modules.mod Unix.mod Kernel.mod \
	     Files.mod -x Reals.mod CmdlnTexts.mod \
	     libc.mod
	    $(CC) SYSTEM.c Args.c Console.c OakOut.c Out.c \
	     Modules.c Unix.c Kernel.c \
	     Files.c Reals.c CmdlnTexts.c \
	     libc.c
	     $(AR) rtl.a SYSTEM.o Args.o Console.o OakOut.o Out.o Modules.o Unix.o Kernel.o Files.o Reals.o CmdlnTexts.o libc.o

	    
ooclibs:
	    $(OFRONT) Ascii.Mod CharClass.Mod ConvTypes.Mod IntConv.Mod IntStr.Mod OakFiles.mod
	    $(CC) Ascii.c CharClass.c ConvTypes.c IntConv.c IntStr.c OakFiles.c
	    $(AR) ooc.a Ascii.o CharClass.o ConvTypes.o IntConv.o IntStr.o OakFiles.o ooc.a

compiler:
	    $(OFRONT) \
	    TextRider.mod \
	    CompatReals.Mod OakStrings.Mod args.mod \
	    OSAS.mod OSAB.mod OSAG.mod Asm.Mod OSAP.Mod 
	    $(OFRONT) foc.mod -m
	    $(CC) \
	    TextRider.c \
	    CompatReals.c OakStrings.c args.c \
	    OSAS.c OSAB.c OSAG.c Asm.c OSAP.c
	    $(CC) foc.c
	    $(CL) -o foc foc.o \
	    TextRider.o \
	    CompatReals.o OakStrings.o args.o \
	    OSAS.o OSAB.o OSAG.o Asm.o OSAP.o	\
	    Ascii.o CharClass.o ConvTypes.o IntConv.o IntStr.o \
	    SYSTEM.o Args.o Console.o OakOut.o Out.o \
	    Modules.o Unix.o Kernel.o \
	    OakFiles.o Files.o Reals.o CmdlnTexts.o \
	    libc.o
		$(AR) all.a *.o

smbtool:
		$(OFRONT) -m smbtool.mod
		$(CC) smbtool.c
		$(CL) -o smbtool smbtool.o all.a

clean:
	rm *.c
	rm *.h
	rm *.sym
	rm *.o
	rm *.a

	

