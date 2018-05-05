include ../../config

install: 
	$(INSTALL) -d $(CAPH_CAPH_LIB)
	(for f in *.cph; do $(INSTALL) $$f $(CAPH_CAPH_LIB); done)

clean:
	-rm -f *~ 

realclean: clean
clobber: realclean
