(* Translation from Turtle to Ntriple *)
(* This module defines function to walk through an AST of a document *)

open Anonymousparser

let obj_to_string (o:obj)=
  match o with
  | I(e) -> " | | " ^ e ^ "\n"
  | S(t) -> " | | \"" ^ t ^ "\"\n"

let predicate_to_string ((e,ol):predicate) =
  " | " ^ e ^ "\n" ^
    (List.fold_right
       (fun o l -> obj_to_string(o) ^ l)
       ol "")

let subject_to_string ((e,pl):subject) =
  e ^ "\n" ^
    (List.fold_right
       (fun p l -> predicate_to_string(p) ^ l)
       pl "")

let document_to_string (sl : document) =
  (List.fold_right
     (fun s l -> subject_to_string(s) ^ "\n" ^ l)
     sl "")
       
let print_ast (d : document) =
  Printf.printf "%s" (document_to_string d)

(* generating Ntriple string *)                

let obj_to_ntriple (s:entity) (p:entity) (o:obj)=
  match o with
  | I(e) -> "[<"^s^">]"^"[<"^p^">]"^"[<"^e^">].\n"
  | S(t) -> "[<"^s^">]"^"[<"^p^">]"^"[\""^t^"\"].\n"

let predicate_to_ntriple (s:entity) ((e,ol):predicate) =
    (List.fold_right
       (fun o l -> (obj_to_ntriple s e o) ^ l)
       ol "")

let subject_to_ntriple ((e,pl):subject) =
    (List.fold_right
       (fun p l -> (predicate_to_ntriple e p) ^ l)
       pl "")

let document_to_ntriple (sl : document) =
  (List.fold_right
     (fun s l -> subject_to_ntriple(s) ^ l)
     sl "")


                
let produce_ntriple (d : document) =
  Printf.printf "%s" (document_to_ntriple d)

let rec count_description (d : document) =
  match d with
  | [] -> 0
  | h::t -> 1 + count_description t
               
