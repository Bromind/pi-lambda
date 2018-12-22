CMO=tree.cmo network_topology.cmo topology_config_parser.cmo config.cmo identifier.cmo channel.cmo pi_lambda_types.cmo constructor.cmo ast.cmo lexer.cmo typer.cmo parser.cmo tcp_channel.cmo interp.cmo 
CMI=
GENERATED=lexer.ml parser.ml parser.mli

CFLAGS=-g -thread
PACKAGES_DEP=yojson,unix,threads
COMPILER=ocamlfind ocamlc -package $(PACKAGES_DEP) $(CFLAGS)

LFLAGS=
LEXER=ocamllex $(LFLAGS)

PFLAGS=-v --dump --explain --graph
PARSER=menhir $(PFLAGS)

all: repl inter test config server

repl: $(CMO) repl.cmo
	$(COMPILER) -linkpkg -o $@ $(CLIBS) $(CMO) repl.cmo

inter: $(CMO) main.cmo
	$(COMPILER) -linkpkg -o $@ $(CLIBS) $(CMO) main.cmo

test: inter tests/test_table tests/test.sh
	cd tests; (./test.sh && cd ..) || (cd .. && exit 1)

config: $(CMO) config_tester.cmo
	$(COMPILER) -linkpkg -o $@ $(CLIBS) $(CMO) config_tester.cmo

server: $(CMO) server.cmo
	$(COMPILER) -linkpkg -o $@ $(CLIBS) $(CMO) server.cmo

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly 
.mli.cmi:
	$(COMPILER) -c $<

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
