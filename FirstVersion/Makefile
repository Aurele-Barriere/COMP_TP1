all : firstversion secondversion

firstversion : lexer.ml firstparser.ml firstversion.ml
	ocamlc -c -pp "camlp4o" lexer.ml
	ocamlc -c -pp "camlp4o" firstparser.ml
	ocamlc -c -pp "camlp4o" firstversion.ml
	ocamlc -pp "camlp4o" lexer.cmo firstparser.cmo firstversion.cmo -o firstversion

secondversion : lexer.ml astparser.ml secondversion.ml
	ocamlc -c -pp "camlp4o" lexer.ml
	ocamlc -c -pp "camlp4o" astparser.ml
	ocamlc -c -pp "camlp4o" secondversion.ml
	ocamlc -pp "camlp4o" lexer.cmo astparser.cmo secondversion.cmo -o secondversion

clean :
	-rm *.cmi
	-rm *.cmo
