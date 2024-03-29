(* Translation from Turtle  to Ntriple *)
open String
open Anonymouslexer
open Anonymousparser
open Anonymouswalker


let test s =
  let d = parse_document(lex(Stream.of_string s)) in
  Printf.printf "Corresponding Ntriple string :\n";
  produce_ntriple d
                  
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


let _ = test (read_file "../tests/test3.ttl")
(*let _ = test("<poly117> <type> \"test&\"; <a> <b>,<c> .<zoef><szf> <zrgj>.")*)
                                                                  
