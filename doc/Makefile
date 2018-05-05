include ../config 

all: 
	(cd lrm; $(MAKE) all)
	(cd primer; $(MAKE) all)

install: 
	(cd lrm; $(MAKE) install)
	(cd primer; $(MAKE) install)

clean:
	(cd lrm; $(MAKE) clean)
	(cd primer; $(MAKE) clean)
	if [ -d examples ]; then (cd examples; $(MAKE) clean) fi

clobber: 
	(cd lrm; $(MAKE) clobber)
	(cd primer; $(MAKE) clobber)
	if [ -d examples ]; then (cd examples; $(MAKE) clobber) fi

realclean: clobber
