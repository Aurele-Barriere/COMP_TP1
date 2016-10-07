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
  | [< 'c when (c>= '1' && c <= '9') || (c>= 'a' && c<='z'); s = token_string (string_of_char c) >] -> S s
and token_string s_read = parser
| [< 'c when (c>= '1' && c <= '9') || (c>= 'a' && c<='z'); s = token_string((String.make 1 c) ^ s_read) >] -> s
| [< >] -> s_read


(* Printing according to Ntriples syntax *)
let print_ntriple (subject : string) (predicate : string) (obj : string) =
Printf.printf "<<%s>> <<%s>> <<%s>>.\n" subject predicate obj




let rec parse_document = parser
| [< s1 = parse_subject; 'Point; s2 = parse_document >] ->  s1 ^ s2
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
| [< 'LeftBracket; 'S(id); 'RightBracket >] -> "<<" ^ subj ^ ">>" ^ "<<" ^ pred ^ ">>" ^ "<<" ^ id ^ ">>" ^ ".\n" 
| [< 'Quote; 'S(id); 'Quote >] -> "<<" ^ subj ^ ">>" ^ "<<" ^ pred ^ ">>" ^ "<<" ^ id ^ ">>" ^ ".\n" 



let test s = print_string(parse_document (lex (Stream.of_string s)))

let _ = test "<1> <2> <3>, <4>; <5> <6>."


