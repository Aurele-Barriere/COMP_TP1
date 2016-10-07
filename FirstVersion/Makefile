all : firstversion

firstversion : firstversion.ml
	ocamlc -pp "camlp4o" -o firstversion firstversion.ml

clean :
	rm *~
	rm *.cmi
	rm *.cmo
