(* Translation from Turtle to Ntriple *)
(* This module defines function to walk through an AST of a document *)

open Astparser

let obj_to_string (o:obj)=
  match o with
  | I(e) -> " | | <" ^ e ^ ">"
  | S(t) -> " | | \"" ^ t ^ "\""

let predicate_to_string ((e,ol):predicate) =
  " | " ^ e ^ "\n" ^
    (List.fold_right
       (fun o l -> obj_to_string(o) ^ "\n" ^ l)
       ol "")

let subject_to_string ((e,pl):subject) =
  e ^ "\n" ^
    (List.fold_right
       (fun p l -> predicate_to_string(p) ^ "\n" ^ l)
       pl "")

let document_to_string (sl : document) =
  (List.fold_right
     (fun s l -> subject_to_string(s) ^ "\n" ^ l)
     sl "")
       
let print_ast (d : document) =
  Printf.printf "%s" (document_to_string d)

   
let produce_ntriple (d : document) =
  "" (* TO DO *)

let rec count_description (d : document) =
  match d with
  | [] -> 0
  | h::t -> 1 + count_description t
               
