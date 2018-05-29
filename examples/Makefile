SUBDIRS=working #tocheck tofix working nextvers tosee

install:
	(cd working; make install)

clean:
	@for i in $(SUBDIRS); do (cd $$i; make clean); done

clobber:
	@for i in $(SUBDIRS); do (cd $$i; make clobber); done

realclean: clobber
