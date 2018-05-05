include ../../config

CCFLAGS     = -Wno-format -Wno-parentheses -Wno-unsequenced # gcc 4 compatibility
LDFLAGS	    =
SOURCES     = tyconv.c
OBJS        = $(SOURCES:.c=.o)
TARGETLIB   = libtyconv.a

all: libtyconv.a

libtyconv.a: $(OBJS)
	$(AR) -q $(TARGETLIB) $(OBJS)

.c.o:
	$(CC) $(CCFLAGS) -c -g -o $@ $<

install: libtyconv.a
	$(INSTALL) -d $(CAPH_C_LIB)
	$(INSTALL) $(TARGETLIB) $(CAPH_C_LIB)
	$(INSTALL) tyconv.h $(CAPH_C_LIB)

clean:
	-rm -f *.o *~ *_old $(EXE) $(TARGETLIB)
	-rm -f a.out
	-rm -f core

clobber: realclean

realclean: clean
	-rm -f libtyconv.a

tyconv.o: tyconv.c
