include ../config

CFLAGS += -Wno-format -Wno-parentheses -Wno-unsequenced # gcc 4 compatibility
CSUPPLIBDIR=../lib/c
CSUPPLIBNAME=tyconv
C_SRCS=txt2bin.c bin2txt.c pgm2txt.c txt2pgm.c pgm2bin.c bin2pgm.c mkdcimg.c 
ML_SRCS=mkconv.ml caphmake.ml mkproject.ml
ML_BYTES=${ML_SRCS:.ml=.byte}
ML_NATIVES=${ML_SRCS:.ml=.native}
C_BINS=${C_SRCS:.c=}
C_EXES=${C_SRCS:.c=.exe}

OCB_FLAGS = -use-ocamlfind -no-hygiene -I compiler -cflags '-unsafe-string -w -3-14-23-52'
OCB = ocamlbuild $(OCB_FLAGS)

all:
ifeq ($(TARGET), native)
	make native
else
	make byte
endif

bin: $(C_BINS)

byte: 
	make bin
	$(OCB) mkconv.byte
	$(OCB) caphmake.byte
	$(OCB) mkproject.byte

native: 
	make bin
	$(OCB) mkconv.native
	$(OCB) caphmake.native
	$(OCB) mkproject.native

install: $(C_BINS)
	$(INSTALL) -d $(BINDIR)
	(for f in $(C_BINS); do $(INSTALL) $$f $(BINDIR); done)
ifeq ($(TARGET), native)
	$(INSTALL) mkconv.native $(BINDIR)/mkconv
	$(INSTALL) caphmake.native $(BINDIR)/caphmake
	$(INSTALL) mkproject.native $(BINDIR)/mkproject
else
	$(INSTALL) mkconv.byte $(BINDIR)/mkconv
	$(INSTALL) caphmake.byte $(BINDIR)/caphmake
	$(INSTALL) mkproject.byte $(BINDIR)/mkproject
endif

.SUFFIXES:
.SUFFIXES: .c .h .o 

.c.o: 
	$(CC) $(CFLAGS) -I$(CSUPPLIBDIR) -c $<

binfmt.o: fmt.h dc.h binfmt.c
mkdcimg.o: fmt.h dc.h mkdcimg.c 
txt2bin.o: fmt.h dc.h txt2bin.c 
bin2txt.o: fmt.h dc.h bin2txt.c 
txt2pgm.o: fmt.h dc.h txt2pgm.c 
pgm2txt.o: fmt.h dc.h pgm2txt.c
pgm2bin.o: fmt.h dc.h pgm2bin.c
bin2pgm.o: fmt.h dc.h bin2pgm.c 

txt2bin: fmt.o dc.o txt2bin.o 
	$(CC) $(CFLAGS) -L$(CSUPPLIBDIR) -o txt2bin fmt.o dc.o txt2bin.o -l$(CSUPPLIBNAME)

bin2txt: fmt.o dc.o bin2txt.o 
	$(CC) $(CFLAGS) -L$(CSUPPLIBDIR) -o bin2txt fmt.o dc.o bin2txt.o -l$(CSUPPLIBNAME)

txt2pgm: fmt.o dc.o txt2pgm.o 
	$(CC) $(CFLAGS) -L$(CSUPPLIBDIR) -o txt2pgm fmt.o dc.o txt2pgm.o -l$(CSUPPLIBNAME)

pgm2txt: fmt.o dc.o pgm2txt.o 
	$(CC) $(CFLAGS) -L$(CSUPPLIBDIR) -o pgm2txt fmt.o dc.o pgm2txt.o -l$(CSUPPLIBNAME)

pgm2bin: fmt.o dc.o pgm2bin.o 
	$(CC) $(CFLAGS) -L$(CSUPPLIBDIR) -o pgm2bin fmt.o dc.o pgm2bin.o -l$(CSUPPLIBNAME)

bin2pgm: fmt.o dc.o bin2pgm.o 
	$(CC) $(CFLAGS) -L$(CSUPPLIBDIR) -o bin2pgm fmt.o dc.o bin2pgm.o -l$(CSUPPLIBNAME)

mkdcimg: fmt.o dc.o mkdcimg.o 
	$(CC) $(CFLAGS) -L$(CSUPPLIBDIR) -o mkdcimg fmt.o dc.o mkdcimg.o -l$(CSUPPLIBNAME)

clean:
	$(OCB) -clean
	rm -f *.o *~ 
	rm -rf *.dSYM

realclean: clean
	rm -f $(BINS)
	rm -f $(EXES)

clobber: realclean
