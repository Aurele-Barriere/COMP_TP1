(* Translation from Turtle  to Ntriple *)
open String
open Lexer
open Astparser
open Walker


let test s =
  let d = parse_document(lex(Stream.of_string s)) in
  Printf.printf "Original string : \n%s\n\n" s;
  Printf.printf "Printing the AST :\n"  ;
  print_ast(d);
  Printf.printf "It has %d description\n" (count_description d);
  Printf.printf "Corresponding Ntriple string :\n";
  produce_ntriple d;
  Printf.printf "Corresponding XML string :\n";
  produce_xml d

                  
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


let _ = test (read_file "../tests/test1.ttl")
(*let _ = test("<poly117> <type> \"test&\"; <a> <b>,<c> .<zoef><szf> <zrgj>.")*)
                                                                  
