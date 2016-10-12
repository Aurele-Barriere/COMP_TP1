(*
 COMP TD
 Traduction from Turtle to Ntriple
  *)

open String

(* function to convert digit chars into integers 
let int_of_char c = Char.code c - Char.code '0'*)

(* First, we define the tokens (lexical units) *)

type token = Point | Semicolon | Comma | Str of string | Id of string

(* Then we define the lexer, using stream parsers with only right recursion *)

let string_of_char c = String.make 1 c

(* We define here the different languages of entity (id) and strings.
  In order to do this, we define the first and last character (which are part of the lexical object) and the middle of it. *)                                   
let is_id_first_char c = (c='<')
let is_id_char c = (c>= '1' && c <= '9') || (c>= 'a' && c<='z') || (c>= 'A' && c<='Z') || (c='-') || (c=' ')
let is_id_last_char c = (c='>')                               
let is_string_first_char c = (c = '"')
let is_string_char c = (c <> '"')
let is_string_last_char c = (c = '"')                         

(* Lexical Analysis *)                              
let rec lex = parser (* char stream -> token stream *)
  | [< 'c when c = ' ' || c = '\t'; toks = lex >] -> toks (* spaces are ignored *)
  | [< tok = token; toks = lex >] -> [< 'tok; toks >]
    (* recognizing one token at a time *)
  | [< >] -> [< >] (* end of stream *)
and token = parser
  | [< ' ('.') >] -> Point
  | [< ' (',') >] -> Comma
  | [< ' (';') >] -> Semicolon
  | [< 'c when is_id_first_char c; s = token_id "" >] -> Id s
  | [< 'c when is_string_first_char c; s = token_string "" >] -> Str s
and token_id s_read = parser
  | [< 'c when is_id_char c; s = token_id(s_read ^ (string_of_char c)) >] -> s
  | [< 'c when is_id_last_char c >] -> s_read
and token_string  s_read = parser
  | [< 'c when is_string_char c; s = token_string(s_read ^ (string_of_char c)) >] -> s
  | [< 'c when is_string_last_char c >] -> s_read
                         

                                             
(* Ntriples strings *)
let make_ntriple_string subj pred obj =
"[<" ^ subj ^ ">]" ^ "[<" ^ pred ^ ">]" ^ "[\"" ^ obj ^ "\"]" ^ ".\n"

let make_ntriple_id subj pred obj =
"[<" ^ subj ^ ">]" ^ "[<" ^ pred ^ ">]" ^ "[<" ^ obj ^ ">]" ^ ".\n"                                                                 

(* Syntaxical Analysis *)
let rec parse_document = parser
| [< s1 = parse_subject; 'Point ?? "point expected"; s2 = parse_document >] ->  s1 ^ s2
| [< >] -> ""

and parse_subject = parser
| [< 'Id(s); p1 = parse_predicate_list s >] -> p1
                                                 
and parse_predicate_list subj = parser
| [< p1 = parse_predicate subj ; p2 = parse_predicate_list_aux subj >] -> p1 ^ p2

and parse_predicate_list_aux subj = parser
| [< 'Semicolon; p1 = parse_predicate_list subj >] -> p1
| [< >] -> ""

and parse_predicate subj = parser
| [< 'Id(s); o1 = parse_object_list subj s >] -> o1
                                                   
and parse_object_list subj pred = parser
| [< o1 = parse_object subj pred; o2 = parse_object_list_aux subj pred >] -> o1 ^ o2

and parse_object_list_aux subj pred = parser
| [< 'Comma; o1 = parse_object_list subj pred >] -> o1
| [< >] -> ""

and parse_object subj pred = parser
| [< 'Id(s) >] -> make_ntriple_id subj pred s
| [< 'Str(s) >] -> make_ntriple_string subj pred s


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


  (*  let _ = test (read_file "../../tests/test1.ttl")  *)
let _ = test("<poly117> <type> \"poly&\" .")

