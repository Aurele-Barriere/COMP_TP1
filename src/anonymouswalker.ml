(* Translation from Turtle to Ntriple *)
(* This module defines function to walk through an AST of a document *)

open Anonymousparser

(* generating Ntriple string *)


let rec anonymous_obj_to_string (o:obj) (n_id:int) =
  match o with
  | I(e) -> (" " ^ e, n_id)
  | S(t) -> (" \"" ^ t ^ "\"", n_id)
  | P(pl) -> let (s_res, n) = plist_to_string pl (n_id + 1) in
	(" _:id"^(string_of_int (n_id + 1))^s_res, n)
  
and anonymous_predicate_to_string ((e,ol):predicate) n_id =
    (List.fold_right
       (fun o (l, n) -> 	let (s, n') = (anonymous_obj_to_string o n) in
        (" .\n_:id" ^ (string_of_int (n_id)) ^ " <" ^ e ^ ">" ^ s  ^ l, (max n' n)))
       ol ("", n_id)) 
       
and plist_to_string (pl: predicate list) n_id =
  (List.fold_right
    (fun p (l, n) -> let (s, n') = (anonymous_predicate_to_string p n_id) in (s ^ l, (max n' n)))
     pl ("", n_id))
  

and obj_to_ntriple (s:entity) (p:entity) (o:obj) (n_id) =
  match o with
  | I(e) -> ("<"^s^"> "^"<"^p^"> "^"<"^e^">.\n", n_id)
  | S(t) -> ("<"^s^"> "^"<"^p^"> "^"\""^t^"\".\n", n_id)
  | P(pl) -> let (s_res, n) = (plist_to_string pl (n_id + 1)) in
	("<"^s^"> "^"<"^p^"> _:id"^(string_of_int (n_id + 1))^s_res, n)

let predicate_to_ntriple (s:entity) ((e,ol):predicate) n_id =
    (List.fold_right
       (fun o (l, n) -> let (s_res, n') = (obj_to_ntriple s e o n) in
			(s_res ^ " .\n" ^ l, (max n' n)))
       ol ("", n_id))

let subject_to_ntriple ((e,pl):subject) n_id =
    (List.fold_right
       (fun p (l, n) -> let (s_res, n') = (predicate_to_ntriple e p n) in
			(s_res  ^ l , (max n' n)))
       pl ("", n_id))

let document_to_ntriple (sl : document) n_id =
  (List.fold_right
     (fun s (l, n) -> let (s_res, n') = subject_to_ntriple s n in
		(s_res ^ l, (max n' n)))
     sl ("", n_id))


                
let produce_ntriple (d : document) =
  let (s, n) = (document_to_ntriple d 0) in
  Printf.printf "%s" s

               
