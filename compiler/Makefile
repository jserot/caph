OCB_FLAGS   = -use-ocamlfind -no-hygiene -cflags '-unsafe-string -w -3-14-23-52'
include ../config

OCB = 		ocamlbuild $(OCB_FLAGS)

GENERATED_SRCS = version.ml options_spec.ml lexer.ml parser.mli parser.ml

all:
ifeq ($(TARGET), native)
	make native
else
	make byte
endif

byte: $(GENERATED_SRCS)
			$(OCB) caphc.byte

native: $(GENERATED_SRCS)
			$(OCB) caphc.native

install:
	$(INSTALL) -d $(BINDIR)
	$(INSTALL) -d $(LIBDIR)
ifeq ($(TARGET), native)
	cp caphc.native $(BINDIR)/caphc
else
	cp caphc.byte $(BINDIR)/caphc
endif
	$(INSTALL) -d $(LIBDIR)/ml
	cp _build/foreign.cmi $(LIBDIR)/ml
	cp _build/foreign.cmo $(LIBDIR)/ml

clean:
	$(OCB) -clean
	rm -f $(GENERATED_SRCS)

clobber: clean 
	rm -f *~
	rm -f parser.output
	rm -f doc/*

profile: $(GENERATED_SRCS)
			$(OCB) -tag profile caphc.native

debug: $(GENERATED_SRCS)
			$(OCB) -tag debug caphc.byte

gen_tex_option = \
	awk -F ";" '{ if ( $$2 == "$(2)" ) printf("%s & %s\\\\\n", $$3, $$6) }' < $(1) | sed 's/_/\\_/g' > $(3)

tex_doc: options_spec.txt
	$(call gen_tex_option,options_spec.txt,general,../doc/lrm/options-general.tex)
	$(call gen_tex_option,options_spec.txt,dot,../doc/lrm/options-dot.tex)
	$(call gen_tex_option,options_spec.txt,simu,../doc/lrm/options-simu.tex)
	$(call gen_tex_option,options_spec.txt,systemc,../doc/lrm/options-systemc.tex)
	$(call gen_tex_option,options_spec.txt,vhdl,../doc/lrm/options-vhdl.tex)

html: $(GENERATED_SRCS)
	ocamldoc -I _build -short-functors -d ./doc -html -colorize-code *.mli *.ml
	open ./doc/index.html

dot: $(GENERATED_SRCS)
	ocamldoc -I ./_build -o ./doc/mod_deps.dot -dot *.mli *.ml
	open -a Graphviz doc/mod_deps.dot

deps: $(GENERATED_SRCS)
	(cd _build; $(CAMLDEP) -sort *.mli *.ml > .depends)

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

parser.ml parser.mli: parser.mly
	$(CAMLYACC) parser.mly
	sed 's/(Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.program/(Lexing.lexbuf  -> token) -> Lexing.lexbuf ref -> Syntax.program/' parser.mli > parser.mli.1; mv parser.mli.1 parser.mli
	sed 's/Parsing/Parsing_m/g' parser.ml > parser.ml.1; mv parser.ml.1 parser.ml
	sed 's/let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =/let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf ref) =/' parser.ml > parser.ml.1; mv parser.ml.1 parser.ml

version.ml:
	echo "(* This file is generated *)" > version.ml
	echo "let version = \"$(VERSION)\"" >> version.ml
#	echo "let stdlib = \"$(LIBDIR)\"" >> version.ml

options_spec.ml: options_spec.txt
	echo "let options_spec = [" > options_spec.ml
	awk -F ";" '{ if ( $$2 != "deprecated" ) printf("\"%s\", %s (Options.%s), \"%s\";\n", $$3, $$4, $$5, $$6) }' < options_spec.txt >> options_spec.ml
	echo "];" >> options_spec.ml


.PHONY: all clean byte native profile debug test tex_doc
