all : firstversion

firstversion : lexer.ml parser.ml firstversion.ml
	ocamlc -c -pp "camlp4o" lexer.ml
	ocamlc -c -pp "camlp4o" parser.ml
	ocamlc -c -pp "camlp4o" firstversion.ml
	ocamlc -pp "camlp4o" lexer.cmo parser.cmo firstversion.cmo -o firstversion

clean :
	-rm *.cmi
	-rm *.cmo
