include ../../config

SRCS=abssyn.tex dynamic.tex introduction.tex overview.tex static.tex syntax.tex typing.tex using.tex
GENSRCS_MAN= bin2txt.tex mkdcimg.tex pgm2txt.tex txt2bin.tex txt2pgm.tex caphmake.tex # generated from ../../man
GENSRCS_COMP=options-dot.tex options-general.tex options-simu.tex options-systemc.tex options-vhdl.tex # generated from ../../compiler
GENSRCS = $(GENSRCS_MAN) $(GENSRCS_COMP)
DOC=caph-lrm

all: $(DOC).pdf

ifeq ($(PLATFORM), win32)
install:
else
install: $(DOC).pdf
endif
	$(INSTALL) -d $(DOCDIR)
	$(CP) $(DOC).pdf $(DOCDIR)

$(DOC).pdf: $(SRCS) $(GENSRCS)
	pdflatex $(DOC).tex > pdflatex.log
	pdflatex $(DOC).tex > pdflatex.log

options-dot.tex:
	(cd ../../compiler; make tex_doc)

bin2txt.tex:
	(cd ../../man; make tex_doc)

clean:
	rm -f *.dvi *.log *.rel *.aux *.toc

.PHONY: options_tex man_tex

realclean: clobber

clobber: clean
	rm -f $(DOC).pdf *~ 
	rm -f $(GENSRCS) 
