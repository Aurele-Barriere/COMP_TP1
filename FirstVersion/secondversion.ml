(* Translation from Turtle  to Ntriple *)
open String
open Lexer
open Astparser



let test s =
print_string s; print_newline (); print_newline (); print_newline ();
print_ast(parse_document (lex (Stream.of_string s)))

let read_file (filename : string) =
let lines = ref "" in
let chan = open_in filename in
try
while true; do
lines := !lines ^ input_line chan   
done; !lines
with End_of_file ->
close_in chan;
!lines ;;


  (*  let _ = test (read_file "../../tests/test1.ttl")  *)
let _ = test("<poly117> <type> \"test&\" .")
                                                                  
