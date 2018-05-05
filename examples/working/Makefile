include ../../config

clean:
	for i in [a-z]*/[a-z0-9_]*; do (cd $$i; if [ -e Makefile ]; then make -s clean; fi) done

clobber:
	for i in [a-z]*/[a-z0-9_]*; do (cd $$i; if [ -e Makefile ]; then make -s clobber; fi; rm -rf ./{dot,sim,systemc,vhdl,xdf}; rm -f ./caph.output; rm -f ./*~; rm -f Makefile*; rm -f *.cphpro; rm -f *_mocs.dat) done

install: 
	$(INSTALL) -d $(EXAMPLEDIR)
	cp ./Readme $(EXAMPLEDIR)
	@for i in [a-z]*; do cp -r $$i $(EXAMPLEDIR); done
