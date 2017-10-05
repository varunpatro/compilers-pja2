jlite_main: jlite_structs.cmo jlite_lexer.cmo jlite_parser.cmo ir3_structs.cmo jlite_main.cmo
	ocamlc -o jlite_main jlite_lexer.cmo jlite_parser.cmo jlite_structs.cmo jlite_main.cmo

ir3_structs.cmo:
	ocamlc -c ir3_structs.ml

jlite_main.cmo:
	ocamlc -c jlite_main.ml

jlite_structs.cmo: jlite_structs.ml
	ocamlc -c jlite_structs.ml

jlite_lexer.cmo: jlite_parser.cmi jlite_lexer.ml
	ocamlc -c jlite_lexer.ml

jlite_lexer.ml: jlite_lexer.mll
	ocamllex jlite_lexer.mll

jlite_parser.cmo: jlite_structs.cmo jlite_parser.cmi jlite_parser.ml
	ocamlc -c jlite_parser.ml

jlite_parser.cmi: jlite_parser.mli
	ocamlc -c jlite_parser.mli

jlite_parser.ml jlite_parser.mli: jlite_structs.ml jlite_parser.mly
	ocamlyacc jlite_parser.mly

clean:
	rm *.cmi *.cmo *.mli jlite_lexer.ml jlite_parser.ml jlite_parser.output jlite_main
