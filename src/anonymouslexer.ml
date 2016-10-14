(* Translation from Turtle to Ntriple *)
(* This module performs lexical analysis *)


(* First, we define the tokens (lexical units) *)

type token = Point | Semicolon | Comma | Str of string | Id of string | LeftBracket | RightBracket

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
  | [< ' ('[') >] -> LeftBracket
  | [< ' (']') >] -> RightBracket 
  | [< 'c when is_id_first_char c; s = token_id "" >] -> Id s
  | [< 'c when is_string_first_char c; s = token_string "" >] -> Str s
and token_id s_read = parser
  | [< 'c when is_id_char c; s = token_id(s_read ^ (string_of_char c)) >] -> s
  | [< 'c when is_id_last_char c >] -> s_read
and token_string  s_read = parser
  | [< 'c when is_string_char c; s = token_string(s_read ^ (string_of_char c)) >] -> s
  | [< 'c when is_string_last_char c >] -> s_read
                         
