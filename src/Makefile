all : firstversion secondversion anonymousversion

firstversion : lexer.ml firstparser.ml firstversion.ml
	ocamlc -c -pp "camlp4o" lexer.ml
	ocamlc -c -pp "camlp4o" firstparser.ml
	ocamlc -c -pp "camlp4o" firstversion.ml
	ocamlc -pp "camlp4o" lexer.cmo firstparser.cmo firstversion.cmo -o firstversion

secondversion : lexer.ml astparser.ml walker.ml secondversion.ml
	ocamlc -c -pp "camlp4o" lexer.ml
	ocamlc -c -pp "camlp4o" astparser.ml
	ocamlc -c -pp "camlp4o" walker.ml
	ocamlc -c -pp "camlp4o" secondversion.ml
	ocamlc -pp "camlp4o" lexer.cmo astparser.cmo walker.cmo secondversion.cmo -o secondversion

anonymousversion : anonymouslexer.ml anonymousparser.ml anonymouswalker.ml anonymousversion.ml
	ocamlc -c -pp "camlp4o" anonymouslexer.ml
	ocamlc -c -pp "camlp4o" anonymousparser.ml
	ocamlc -c -pp "camlp4o" anonymouswalker.ml
	ocamlc -c -pp "camlp4o" anonymousversion.ml
	ocamlc -pp "camlp4o" anonymouslexer.cmo anonymousparser.cmo anonymouswalker.cmo anonymousversion.cmo -o anonymousversion



clean :
	-rm *.cmi
	-rm *.cmo
