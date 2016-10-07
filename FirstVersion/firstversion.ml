(*
  The expression parser from the Web page, adapted to respect
  left-associativity of operators.
  *)

open String

(* function to convert digit chars into integers 
let int_of_char c = Char.code c - Char.code '0'*)

(* First, we define the tokens (lexical units) *)

type token = Point | Semicolon | Comma | LeftBracket | RightBracket | S of string

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
  | [< 'c when (c>= '1' && c <= '9') || (c>= 'a' && c<='z'); s = token_string (string_of_char c) >] -> S s
and token_string s_read = parser
| [< 'c when (c>= '1' && c <= '9') || (c>= 'a' && c<='z'); s = token_string((String.make 1 c) ^ s_read) >] -> s
| [< >] -> s_read

(* Next, we define a type to represent abstract syntax trees: *)

(* type expr = Num of int | Add of expr * expr | Sub of expr * expr | Mul of expr * expr *)

(* The recursive descent parser consists of three mutually-recursive functions: *)
(* 
let rec parse_expr = parser
  | [< e1 = parse_factor; e2 = parse_expr_aux e1 >] -> e2

and parse_expr_aux e1 = parser
  | [< 'Plus; e2 = parse_factor; e3 = parse_expr_aux (Add (e1,e2)) >] -> e3
  | [< 'Minus; e2 = parse_factor; e3 = parse_expr_aux (Sub (e1,e2)) >] -> e3
  | [< >] -> e1

and parse_factor = parser
  | [< e1 = parse_atom; e2 = parse_factor_aux e1 >] -> e2

and parse_factor_aux e1 = parser
  | [< 'Times; e2 = parse_atom; e3 = parse_factor_aux (Mul (e1,e2)) >] -> e3
  | [< >] -> e1

and parse_atom = parser
  | [< 'Int n >] -> Num n
  | [< 'LeftBracket; e = parse_expr; 'RightBracket >] -> e

(* That is all that is required to parse simple arithmetic
expressions. We can test it by lexing and parsing a string to get the
abstract syntax tree representing the expression: *)

let test s = parse_expr (lex (Stream.of_string s))

let _ = test "1+2*(3+4)-5"
(*- : expr = Sub (Add (Num 1, Mul (Num 2, Add (Num 3, Num 4))), 5) *)
*)
