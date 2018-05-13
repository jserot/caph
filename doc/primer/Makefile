include ../../config

SRCS=caph-primer.tex
DOC=caph-primer

all: caph-primer.pdf

ifeq ($(PLATFORM), win32)
install:
else
install: caph-primer.pdf
endif
	$(INSTALL) -d $(DOCDIR)
	$(CP) caph-primer.pdf $(DOCDIR)

caph-primer.pdf: caph-primer.tex
	pdflatex caph-primer.tex > pdflatex.log
	pdflatex caph-primer.tex > pdflatex.log

clean:
	rm -f *.dvi *.log *.rel *.aux *.toc

realclean: clobber

clobber: clean
	rm -f caph-primer.pdf *~ 
