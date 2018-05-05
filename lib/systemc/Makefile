include ../../config

CXXFLAGS    = -I. -I$(SYSTEMC)/include -Wno-deprecated -DTRACE_SYSC_IO
LDFLAGS	    =  -L$(SYSTEMC)/lib-$(TARGET_ARCH) 
SOURCES     = # no source : pure template lib
OBJS        = $(SOURCES:.cpp=.o)

all:
	@echo "Nothing to do"

install: 
	$(INSTALL) -d $(CAPH_SYSC_LIB)
	(for f in *.h; do $(CP) $$f $(CAPH_SYSC_LIB); done)
	$(CP) README $(CAPH_SYSC_LIB)

clean:
	-rm -f *~
#	(cd tests; make clean)

realclean: clean

clobber: realclean
