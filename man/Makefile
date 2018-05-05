include ../config

POD2MAN=pod2man
POD2TEX=pod2latex
PODS=txt2bin.pod bin2txt.pod pgm2txt.pod txt2pgm.pod pgm2bin.pod bin2pgm.pod mkdcimg.pod mkconv.pod caphmake.pod
TEXS=$(PODS:.pod=.tex)
MANS=$(PODS:.pod=.1)
TEX_MAN_SECTIONS="NAME|SYNOPSIS|DESCRIPTION|ARGUMENTS|OPTIONS|EXAMPLES|EXIT STATUS|CAVEAT"
   # It's a pity the [-nosections] option to pod2latex does not seem to work..

all: man tex

ifeq ($(PLATFORM), win32)
install: 
else
install: man
	$(INSTALL) -d $(MANDIR)
	(for f in $(MANS); do $(CP) $$f $(MANDIR); done)
endif

tex_doc: $(TEXS)
	cp $(TEXS) ../doc/lrm

tex: $(TEXS)

man: $(MANS)

.SUFFIXES:
.SUFFIXES: .c .h .pod .tex .1

.pod.1:
	$(POD2MAN)  -c "CAPH Distribution" $< $@

.pod.tex:
	@$(POD2TEX) -full -preamble "\chapter{The \texttt{$(<:.pod=)} command}" -preamble "\label{cha:$(<:.pod=)}" -postamble "%%% Local Variables:" -postamble "%%% mode: latex" -postamble "%%% TeX-master: \"caph\"" -postamble "%%% End: " -sections $(TEX_MAN_SECTIONS) $< $@
#	$(POD2TEX) -full -prefile $(TEX_PREFILE) -postfile $(TEX_POSTFILE) -sections $(TEX_MAN_SECTIONS) $< $@
	@sed -E -e "s/\\\\label{[^}]+}|\\\\index{[^}]+}//g" -e "s/section/section*/" $@ > _$@
	@mv _$@ $@ 
#	pdflatex $@

clean:
	rm -f *~ *.aux *.idx *.log *.pdf *.toc

realclean: clean
	rm -f $(TEXS) $(MANS)

clobber: realclean

.PHONY: tex-doc
