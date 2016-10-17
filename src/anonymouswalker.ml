(* Translation from Turtle to Ntriple *)
(* This module defines function to walk through an AST of a document *)

open Anonymousparser

(* generating Ntriple string *)

let n_id = ref 0  

let rec anonymous_obj_to_string (o:obj)=
  match o with
  | I(e) -> " " ^ e
  | S(t) -> " \"" ^ t ^ "\""
  | P(pl) -> incr n_id;
	" _id"^(string_of_int (!n_id))^" .\n"^(plist_to_string pl)
  
and anonymous_predicate_to_string ((e,ol):predicate) =
    (List.fold_right
       (fun o l -> 	"_:id" ^ (string_of_int (!n_id)) ^ " <" ^ e ^ ">" ^anonymous_obj_to_string(o) ^ " .\n" ^ l)
       ol "") 
       
and plist_to_string (pl: predicate list) =
  (List.fold_right
    (fun p l -> (anonymous_predicate_to_string p) ^ l)
     pl "")
  

and obj_to_ntriple (s:entity) (p:entity) (o:obj)=
  match o with
  | I(e) -> "<"^s^"> "^"<"^p^"> "^"<"^e^">.\n"
  | S(t) -> "<"^s^"> "^"<"^p^"> "^"\""^t^"\".\n"
  | P(pl) -> incr n_id;
	"<"^s^"> "^"<"^p^"> _:id"^(string_of_int (!n_id))^" .\n"^(plist_to_string pl)

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

(* Counting descriptions *)
let rec count_description (d : document) =
  match d with
  | [] -> 0
  | h::t -> 1 + count_description t
               

(* Producing XML *)
let obj_to_xml (p:entity) (o:obj)=
  match o with
  | I(e) -> "\t<" ^ p ^ " rdf:resource=\"" ^ e ^ "\"/>\n"
  | S(t) -> "\t<" ^ p ^ ">" ^ t ^ "</" ^ p ^ ">\n"
  | P(pl) -> "" (*TODO*)

let predicate_to_xml ((e,ol):predicate) =
    (List.fold_right
       (fun o l -> (obj_to_xml e o) ^ l)
       ol "")

let subject_to_xml ((e,pl):subject) =
"<rdf:Description rdf:about=\"" ^ e ^ "\">\n" ^
    (List.fold_right
       (fun p l -> (predicate_to_xml p) ^ l)
       pl "") ^
"</rdf:Description>\n"

let document_to_xml (sl : document) =
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<rdf:RDF\nxml:base=\"http://mydomain.org/myrdf/\"\nxmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">\n" ^
  (List.fold_right
     (fun s l -> subject_to_xml(s) ^ l)
     sl "") ^
"</rdf:RDF>\n"

            
let produce_xml (d : document) =
  Printf.printf "%s" (document_to_xml d)
