SUBDIRS=c etc systemc vhdl caph 

install:
	(cd c; $(MAKE) install)
	(cd systemc; $(MAKE) install)
	(cd vhdl; $(MAKE) install)
	(cd caph; $(MAKE) install)
	(cd etc; $(MAKE) install)

clean:
	@for i in $(SUBDIRS); do (cd $$i; make -s clean); done

clobber:
	@for i in $(SUBDIRS); do (cd $$i; make -s clobber); done

realclean: clobber
