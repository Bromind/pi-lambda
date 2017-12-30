
CMO=config.cmo channel.cmo ast.cmo lexer.cmo typer.cmo parser.cmo interp.cmo 
GENERATED=lexer.ml parser.ml parser.mli

CFLAGS=-g -thread
CLIBS=unix.cma threads.cma
COMPILER=ocamlc $(CFLAGS)

LFLAGS=
LEXER=ocamllex $(LFLAGS)

PFLAGS=-v --dump --explain --graph
PARSER=menhir $(PFLAGS)

all: repl inter 
repl: $(CMO) repl.cmo
	$(COMPILER) -o $@ $(CLIBS) $(CMO) repl.cmo

inter: $(CMO) main.cmo
	$(COMPILER) -o $@ $(CLIBS) $(CMO) main.cmo

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly 
.mli.cmi:
	$(COMPILER) -c  $<

.ml.cmo:
	$(COMPILER) -c $<

.mll.ml:
	$(LEXER) $<

.mly.ml:
	$(PARSER) $<

.mly.mli:
	$(PARSER) $<

.PHONY: clean
clean:
	-rm $(GENERATED)
	-rm -f *.cm[io] *.o *.annot 
	-rm -f *.automaton *.conflicts *.dot # menhir generated files
	-rm .depend

.depend depend:$(GENERATED)
	@echo "Computing dependencies..."
	-rm -f .depend
	-ocamldep *.ml *.mli > .depend
	@echo "Done"

include .depend
