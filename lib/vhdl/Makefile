include ../../config

ifeq ($(VHDL_SUPPORT),yes)
ifeq ($(USE_VHDL_FP_LIB),yes)
all: lib-core lib-fp
install: install-core install-fp
else
all: lib-core
install: install-core
endif
else
all:
	@echo "Warning: VHDL library not built (run ../../configure with [--vhdl_support])"
install:
	@echo "Warning: VHDL library not installed (run ../../configure with [--vhdl_support])"
endif

COMPONENTS=data_types.vhd \
           stream_out.vhd \
           stream_in.vhd \
           stream_out_mult.vhd \
           stream_in_mult.vhd \
           port_in.vhd \
           port_out.vhd \
           port_buffer.vhd \
           fifo.vhd

OBJS=$(COMPONENTS:.vhd=.o)

%.o: %.vhd
	$(GHDL)  -a --work=caph --workdir=. -fno-color-diagnostics $(GHDLOPTS) $<

caph_fp.o: caph_fp.vhd
	$(GHDL) -a --work=caph_fp --workdir=. $(GHDLOPTS) -fno-color-diagnostics -P$(VHDL_FP_LIB) $<

lib-core: $(OBJS) caph.o

lib-fp: $(OBJS) caph_fp.o

ifeq ($(PLATFORM), win32)
install-core: 
	$(INSTALL) -d $(CAPH_VHDL_LIB)
	(for f in $(COMPONENTS); do $(CP) $$f $(CAPH_VHDL_LIB); done)
	$(CP) caph.vhd $(CAPH_VHDL_LIB)
	$(CP) README $(CAPH_VHDL_LIB)
else
install-core: lib-core
	$(INSTALL) -d $(CAPH_VHDL_LIB)
	(for f in $(COMPONENTS); do $(CP) $$f $(CAPH_VHDL_LIB); done)
	(for f in $(OBJS); do $(CP) $$f $(CAPH_VHDL_LIB); done)
	$(CP) caph.vhd $(CAPH_VHDL_LIB)
	$(CP) caph.o $(CAPH_VHDL_LIB)
	$(CP) caph-obj93.cf $(CAPH_VHDL_LIB)
	$(CP) README $(CAPH_VHDL_LIB)
endif

ifeq ($(PLATFORM), win32)
install-fp: 
	$(INSTALL) -d $(CAPH_VHDL_LIB)
	$(CP) caph_fp.vhd $(CAPH_VHDL_LIB)
else
install-fp: lib-fp
	$(INSTALL) -d $(CAPH_VHDL_LIB)
	$(CP) caph_fp.vhd $(CAPH_VHDL_LIB)
	$(CP) caph_fp-obj93.cf $(CAPH_VHDL_LIB)
endif

clean:
	\rm -f *.cf *.o

realclean: clean
	\rm -f *~

clobber: realclean
