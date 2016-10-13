all : firstversion

firstversion : lexer.ml firstparser.ml firstversion.ml
	ocamlc -c -pp "camlp4o" lexer.ml
	ocamlc -c -pp "camlp4o" firstparser.ml
	ocamlc -c -pp "camlp4o" firstversion.ml
	ocamlc -pp "camlp4o" lexer.cmo firstparser.cmo firstversion.cmo -o firstversion

clean :
	-rm *.cmi
	-rm *.cmo
