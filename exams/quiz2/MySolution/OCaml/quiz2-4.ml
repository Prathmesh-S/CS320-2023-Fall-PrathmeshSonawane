(* ************************************************ *)

(*
Q2-4: 5 points
The function list_last returns the last element of a given
list. Please give a NON-RECURSIVE implementation of list_last
based on pattern matching and list_foldleft. If the given list
is empty, raise the Empty exception
*)

(* ************************************************ *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

exception Empty

let element (xs: 'a list) : 'a = 
  match xs with
  |[] -> raise Empty 
  |a::_ -> a
;;

let list_reverse(xs: 'a list): 'a list = 
let folder x acc = list_foldright acc (x :: []) (fun i acc -> i :: acc) in
    list_foldright xs [] folder
  ;;


let list_last(xs: 'a list): 'a = 
  match xs with 
  |[] -> raise Empty 
  |_ -> element (((list_foldleft(list_reverse(xs)) ([]) (fun acc x -> if acc = [] then x::acc else acc))))
;;
