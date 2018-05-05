include VERSION
include config

all:
	(cd compiler; $(MAKE) all)
	(cd lib/c; $(MAKE) all)
	(cd lib/vhdl; $(MAKE) all)
	(cd tools; $(MAKE) all)

opt: TARGET := opt
opt: all

byte: TARGET := byte
byte: all

.PHONY: install

install:
	(cd compiler; $(MAKE) install)
	(cd lib/c; $(MAKE) install)
	(cd lib/systemc; $(MAKE) install)
	(cd lib/vhdl; $(MAKE) install)
	(cd lib/caph; $(MAKE) install)
	(cd lib/etc; $(MAKE) install)
	(cd tools; $(MAKE) install)
	(cd examples/working; $(MAKE) install)
	(cd man; $(MAKE) install)
	(cd doc; $(MAKE) install)
	$(INSTALL) ./config $(CAPH_ETC_LIB)
	[ -e ./caphy_build/caphy.app ] && cp -r ./caphy_build/caphy.app $(BINDIR) || ([ -e ./caphy_build/caphy ] && cp ./caphy_build/caphy $(BINDIR) || echo "** cannot find caphy executable")
	echo "" > $(BINDIR)/caph.ini
	echo "CAPHC=$(BINDIR)/caphc" >> $(BINDIR)/caph.ini
	echo "DOTVIEWER=$(DOTVIEWER)" >> $(BINDIR)/caph.ini
	echo "PGMVIEWER=$(PGMVIEWER)" >> $(BINDIR)/caph.ini


clean:
	(cd compiler; $(MAKE) clean)
	(cd lib/c; $(MAKE) clean)
	(cd lib/systemc; $(MAKE) clean)
	(cd lib/vhdl; $(MAKE) clean)
	(cd lib/etc; $(MAKE) clean)
	(cd lib/caph; $(MAKE) clean)
	(cd tools; $(MAKE) clean)
	(cd etc; $(MAKE) clean)
	(cd man; $(MAKE) clean)
	(cd doc; $(MAKE) clean)
	(cd examples/working; $(MAKE) clean)

realclean: clean-dist
	(cd compiler;$(MAKE) realclean)
	(cd lib/c; $(MAKE) realclean)
	(cd lib/etc; $(MAKE) realclean)
	(cd lib/systemc; $(MAKE) realclean)
	(cd lib/vhdl; $(MAKE) realclean)
	(cd lib/caph; $(MAKE) realclean)
	(cd tools; $(MAKE) realclean)
	(cd etc; $(MAKE) realclean)
	(cd man; $(MAKE) realclean)
	(cd doc; $(MAKE) realclean)
	(cd examples/working; $(MAKE) realclean)

clobber: realclean
