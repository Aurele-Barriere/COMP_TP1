(* Translation from Turtle to Ntriple *)
(* This module performs Syntaxical Analaysis *)

open Lexer
open String

(* Ntriples strings *)
let make_ntriple_string subj pred obj =
"[<" ^ subj ^ ">]" ^ "[<" ^ pred ^ ">]" ^ "[\"" ^ obj ^ "\"]" ^ ".\n"

let make_ntriple_id subj pred obj =
  "[<" ^ subj ^ ">]" ^ "[<" ^ pred ^ ">]" ^ "[<" ^ obj ^ ">]" ^ ".\n"
       
(* Parsing *)
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
