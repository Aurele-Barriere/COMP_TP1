(* Translation from Turtle to Ntriple *)
(* This module performs Syntaxical Analaysis *)

open Lexer
open String
open List

type entity = string
type text = string
type obj = I of entity | S of text
type predicate = entity * (obj list)
type subject = entity * (predicate list)
type document = subject list                      

let print_ast (d : document) =
  Printf.printf("TO DO : print an AST\n")
                        
       
(* Parsing *)
let rec parse_document = parser
| [< s1 = parse_subject; 'Point ?? "point expected"; s2 = parse_document >] -> s1 :: s2
| [< >] -> []

and parse_subject = parser
| [< 'Id(s); p1 = parse_predicate_list >] -> (s, p1)
                                                 
and parse_predicate_list = parser
| [< p1 = parse_predicate; p2 = parse_predicate_list_aux >] -> p1::p2

and parse_predicate_list_aux = parser
| [< 'Semicolon; p1 = parse_predicate_list >] -> p1
| [< >] -> []

and parse_predicate = parser
| [< 'Id(s); o1 = parse_object_list >] -> (s,o1)
                                                   
and parse_object_list = parser
| [< o1 = parse_object; o2 = parse_object_list_aux >] -> o1 :: o2

and parse_object_list_aux = parser
| [< 'Comma; o1 = parse_object_list >] -> o1
| [< >] -> []

and parse_object = parser
| [< 'Id(s) >] -> I(s)
| [< 'Str(s) >] -> S(s)
