(*
  The expression parser from the Web page, adapted to respect
  left-associativity of operators.
  *)

open String

(* function to convert digit chars into integers 
let int_of_char c = Char.code c - Char.code '0'*)

(* First, we define the tokens (lexical units) *)

type token = Point | Semicolon | Comma | LeftBracket | RightBracket | S of string | Quote

(* Then we define the lexer, using stream parsers with only right recursion *)

let string_of_char c = String.make 1 c

let rec lex = parser (* char stream -> token stream *)
  | [< 'c when c = ' ' || c = '\t'; toks = lex >] -> toks (* spaces are ignored *)
  | [< tok = token; toks = lex >] -> [< 'tok; toks >]
    (* recognizing one token at a time *)
  | [< >] -> [< >] (* end of stream *)
and token = parser
  | [< ' ('.') >] -> Point
  | [< ' (',') >] -> Comma
  | [< ' (';') >] -> Semicolon
  | [< ' ('<') >] -> LeftBracket
  | [< ' ('>') >] -> RightBracket
  | [< ' ('"') >] -> Quote
  | [< 'c when (c>= '1' && c <= '9') || (c>= 'a' && c<='z') || (c>= 'A' && c<='Z') ; s = token_string (string_of_char c) >] -> S s
and token_string s_read = parser
| [< 'c when (c>= '1' && c <= '9') || (c>= 'a' && c<='z') || (c>= 'A' && c<='Z') || (c=' ') || (c='-'); s = token_string(s_read ^ (String.make 1 c)) >] -> s
| [< >] -> s_read

(* Ntriples strings *)
let make_ntriple_string subj pred obj =
"<<" ^ subj ^ ">>" ^ "<<" ^ pred ^ ">>" ^ "<<" ^ obj ^ ">>" ^ ".\n" 

let rec parse_document = parser
| [< s1 = parse_subject; 'Point ?? "point expected"; s2 = parse_document >] ->  s1 ^ s2
| [< >] -> ""

and parse_subject = parser
| [< 'LeftBracket; 'S(id); 'RightBracket; p1 = parse_predicate_list id >] -> p1

and parse_predicate_list subj = parser
| [< p1 = parse_predicate subj ; p2 = parse_predicate_list_aux subj >] -> p1 ^ p2

and parse_predicate_list_aux subj = parser
| [< 'Semicolon; p1 = parse_predicate_list subj >] -> p1
| [< >] -> ""

and parse_predicate subj = parser
| [< 'LeftBracket; 'S(id); 'RightBracket; o1 = parse_object_list subj id >] -> o1

and parse_object_list subj pred = parser
| [< o1 = parse_object subj pred; o2 = parse_object_list_aux subj pred >] -> o1 ^ o2

and parse_object_list_aux subj pred = parser
| [< 'Comma; o1 = parse_object_list subj pred >] -> o1
| [< >] -> ""

and parse_object subj pred = parser
| [< 'LeftBracket; 'S(id); 'RightBracket >] -> make_ntriple_string subj pred id
| [< 'Quote; 'S(id); 'Quote >] -> make_ntriple_string subj pred id



let test s =
print_string s; print_newline (); print_newline (); print_newline ();
print_string(parse_document (lex (Stream.of_string s)))

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


 let _ = test (read_file "../../tests/test1.ttl")  

