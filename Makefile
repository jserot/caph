include VERSION
-include config

.PHONY: install gui doc txt-doc

# Generic rules

build: 
ifeq ($(PLATFORM), win32)
	make build-win32
else
	make build-unix
endif

build-unix: 
	(cd compiler; $(MAKE) all)
	(cd lib/c; $(MAKE) all)
	(cd tools; $(MAKE) all)
	(cd lib/systemc; $(MAKE) all)
	(cd lib/vhdl; $(MAKE) all)
ifeq ($(BUILD_DOC), yes)
	(cd man; $(MAKE) all)
	(cd doc; $(MAKE) all)
endif
ifeq ($(BUILD_GUI), yes)
	(cd gui; $(QMAKE) -spec $(QMAKE_SPEC) -o Makefile caph.pro; make)
endif

dev:
	(cd compiler; $(MAKE) all; make install PREFIX=`pwd`/..)
	(cd lib/c; $(MAKE) all)
	(cd lib/vhdl; $(MAKE) all)
	(cd tools; $(MAKE) all; make install PREFIX=`pwd`/..)

opt: TARGET := opt
opt: all

byte: TARGET := byte
byte: all

install:
	(cd compiler; $(MAKE) install)
	(cd tools; $(MAKE) install)
	(cd lib; $(MAKE) install)
ifeq ($(BUILD_DOC), yes)
	(cd man; $(MAKE) install)
	(cd doc; $(MAKE) install)
endif
	(cd examples; $(MAKE) install)

doc: 
	(cd doc; $(MAKE) all)

txt-docs: CHANGES KNOWN-BUGS README LICENSE

CHANGES: CHANGES.md
	pandoc -f markdown-smart -t plain -o $@ $<

CHANGES.html: CHANGES.md
	pandoc -f markdown-smart -t html -o $@ $<

KNOWN-BUGS: KNOWN-BUGS.md
	pandoc -f markdown-smart -t plain -o $@ $<

README: README.md
	pandoc -f markdown-smart -t plain -o $@ $<

clean:
	(cd compiler;$(MAKE) -s clobber)
	(cd lib; $(MAKE) -s clobber)
	(cd tools; $(MAKE) -s clobber)
	(cd etc; $(MAKE) -s clobber)
	(cd man; $(MAKE) -s clobber)
	(cd doc; $(MAKE) -s clobber)
	(cd examples; $(MAKE) -s clobber)
	(cd gui; if [ -e Makefile ]; then make clean; fi)
	rm -f *~

clobber: clean
	rm -f config

# Platform and target specific rules

SRCDIR=/Users/jserot/Dev/ml/caph
TMPDIR=/tmp

#DISTDIR=caph-$(VERSION)
DISTDIR=caph-source
TARBALL=$(DISTDIR).tar.gz

source-dist: 
	(cd $(TMPDIR); rm -rf $(DISTDIR); git clone $(SRCDIR) caph-source; tar --exclude .git -zcvf $(TARBALL) $(DISTDIR))
	echo "checksum: \""`md5 -q $(TMPDIR)/$(TARBALL)`"\"" >> $(TMPDIR)/$(TARBALL).ssa
	@ echo "** Files $(TMPDIR)/$(TARBALL) and $(TMPDIR)/$(TARBALL).ssa are ready."


macos-dist:
	make macos-source
	make macos-build
	make macos-install
	make macos-installer

macos-source:
	(cd $(TMPDIR); rm -rf $(DISTDIR); git clone $(SRCDIR); mv caph $(DISTDIR))

INSTDIR=$(TMPDIR)/caph-macos

macos-build:
	(cd $(TMPDIR)/$(DISTDIR); touch config; ./configure -platform macos -prefix $(INSTDIR) -dotviewer "open -a Graphviz" -pgmviewer "open -a Toyviewer")
	(cd $(TMPDIR)/$(DISTDIR); make)
	(cd $(TMPDIR)/$(DISTDIR); make txt-docs)

macos-install:
	@echo "\033[32mInstalling in $(INSTDIR)\033[0m"
	rm -rf $(INSTDIR)	
	(cd $(TMPDIR)/$(DISTDIR); make install)
	cp -r $(TMPDIR)/$(DISTDIR)/gui/caph.app $(INSTDIR)/Caph.app
	cp $(TMPDIR)/$(DISTDIR)/dist/macos/INSTALL $(INSTDIR)/INSTALL
	cp $(TMPDIR)/$(DISTDIR)/dist/macos/caph.ini $(INSTDIR)/Caph.app/Contents/MacOS/caph.ini
	cp $(TMPDIR)/$(DISTDIR)/{CHANGES,KNOWN-BUGS,LICENSE,README,VERSION} $(INSTDIR)
	make macos-projects

macos-projects:
	@echo "\033[32mUpdating projects in $(INSTDIR)/examples\033[0m"
	for i in $(INSTDIR)/examples/[a-z]*/[a-z0-9_]*; do (cd $$i; $(INSTDIR)/bin/mkproject -platform macos -D CAPHLIB=/Applications/Caph.app/Contents/Resources/lib/caph -caph /Applications/Caph.app/Contents/Resources -dotviewer \"$(DOTVIEWER)\" -pgmviewer \"$(PGMVIEWER)\" *.proj) done

CAPH_VOLUME=Caph-$(VERSION)

macos-installer:
	@echo "\033[32mCreating disk image\033[0m"
	rm -f $(TMPDIR)/Caph.dmg
	hdiutil create -size 32m -fs HFS+ -volname "$(CAPH_VOLUME)" $(TMPDIR)/Caph.dmg
	hdiutil attach $(TMPDIR)/Caph.dmg
	cp -r $(INSTDIR)/Caph.app /Volumes/$(CAPH_VOLUME)
	cp -r $(INSTDIR)/lib /Volumes/$(CAPH_VOLUME)/Caph.app/Contents/Resources
	cp $(INSTDIR)/bin/* /Volumes/$(CAPH_VOLUME)/Caph.app/Contents/MacOS
	ln -s /Applications /Volumes/$(CAPH_VOLUME)/Applications
	cp -r $(INSTDIR)/examples /Volumes/$(CAPH_VOLUME)/Examples
	cp -r $(INSTDIR)/doc /Volumes/$(CAPH_VOLUME)/Documentation
	cp $(INSTDIR)/{CHANGES,KNOWN-BUGS,LICENSE,README,VERSION,INSTALL} /Volumes/$(CAPH_VOLUME)
	hdiutil detach /Volumes/$(CAPH_VOLUME)
	hdiutil convert $(TMPDIR)/Caph.dmg -format UDZO -o $(TMPDIR)/Caph_ro.dmg
	$(MV) $(TMPDIR)/Caph_ro.dmg $(TMPDIR)/Caph.dmg
	@echo "\033[32mDone. Disk image is $(TMPDIR)/Caph.dmg\033[0m"

SRC_DIR=~/Dev/ml/caph
WIN_SRC_DIR=~/Desktop/SF1/Caml/caph
WIN_SRC_DIR_CYGWIN=/cygdrive/e/Caml/caph

win32-src:
# To be run from MacOS side
	@echo "Copying source tree to $(WIN_SRC_DIR)/src"
	cd $(WIN_SRC_DIR)
	if [ -d $(WIN_SRC_DIR)/src ]; then rm -rf $(WIN_SRC_DIR)/src; fi
	(cd $(WIN_SRC_DIR); git clone $(SRC_DIR) src; touch src/config)
	(./configure --platform macos; make doc; cp doc/lrm/caph-lrm.pdf doc/primer/caph-primer.pdf $(WIN_SRC_DIR)/build/doc)
	@echo "Now, make win32-dist from Windows"

QMAKE_WIN32 = C:/Qt/Qt5.8.0/5.8/mingw53_32/bin/qmake.exe
QMAKE_SPEC_WIN32 = win32-g++

win32-compiler:
# To be run from Windows side, from a Cygwin terminal
	./configure -platform win32 -prefix $(WIN_SRC_DIR_CYGWIN)/build -dotviewer "/C/Program Files/Graphviz/bin/dotty.exe" -pgmviewer "/C/Program Files/ImageGlass/ImageGlass.exe" -qmake $(QMAKE_WIN32) -qmake-spec $(QMAKE_SPEC_WIN32) --no-doc
# Note : documentation will always be built on the Unix side
	(cd compiler; sed -i 's/\/dev\/null/nul/' misc.ml; $(MAKE) native)
	(cd lib/c; $(MAKE) all)
	(cd tools; $(MAKE) native)
	(cd lib/systemc; $(MAKE) all)
#	(cd lib/vhdl; $(MAKE) all) # Do not try to compile VHDL lib on Windows

win32-gui:
# To be run from Windows side, from a *MinGW* terminal
	(cd gui; $(QMAKE) -spec win32-g++ -o Makefile caph.pro; $(MAKE))

win32-install:
# To be run from Windows side, from a Cygwin terminal
	@echo "** Installing in $(PREFIX)"
	cp ./dist/windows/setup_projects.cmd $(BINDIR)
	cp {CHANGES.md,KNOWN-BUGS.md,LICENSE,README.md,VERSION} $(PREFIX)
	cp ./dist/windows/FIRST.TXT $(PREFIX)
	cp ./dist/windows/icons/*.{bmp,ico} $(PREFIX)
	cp compiler/_build/caphc.native $(BINDIR)/caphc.exe
	cp tools/*.exe $(BINDIR)
	(cd tools/_build; for i in *.native; do cp $$i $(BINDIR)/$${i/native/exe}; done)
	(cd lib; $(MAKE) install)
	(cd examples; $(MAKE) install)
	cp gui/release/caph.exe $(PREFIX)
	@echo "Done"

INNOSETUP="/cygdrive/c/Program Files/Inno Setup 5/iscc"

win32-installer:
# To be run from Windows side, from a Cygwin terminal
	@echo "** Building self-installer"
	$(INNOSETUP) ./dist/windows/CaphSetup.iss
	@echo "Done"
