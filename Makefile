OC = ocamlc -g -c
OO = ocamlc -g -o
OLEX = ocamllex
OYACC = ocamlyacc

jlite_main: jlite_annotatedtyping.cmo jlite_toir3.cmo jlite_structs.cmo jlite_lexer.cmo jlite_parser.cmo ir3_structs.cmo jlite_main.cmo
	$(OO) $@ $^

ir3_structs.cmo: ir3_structs.ml jlite_structs.cmo
	$(OC) $<

jlite_main.cmo: jlite_main.ml jlite_toir3.cmo
	$(OC) $<

jlite_structs.cmo: jlite_structs.ml
	$(OC) $<

jlite_annotatedtyping.cmo: jlite_annotatedtyping.ml jlite_structs.cmo
	$(OC) $<

jlite_toir3.cmo: jlite_toir3.ml ir3_structs.cmo
	$(OC) $<

jlite_lexer.cmo: jlite_lexer.ml jlite_parser.cmo
	$(OC) $<

jlite_lexer.ml: jlite_lexer.mll jlite_parser.cmo
	$(OLEX) $<

jlite_parser.cmo: jlite_parser.ml jlite_structs.cmo
	$(OC) jlite_parser.mli
	$(OC) $<

jlite_parser.ml: jlite_parser.mly jlite_structs.ml
	$(OYACC) $<

.PHONY: clean
clean:
	-rm *.cmi *.cmo *.cmti *.cmt *.mli jlite_lexer.ml jlite_parser.ml jlite_parser.mli jlite_parser.output jlite_main
