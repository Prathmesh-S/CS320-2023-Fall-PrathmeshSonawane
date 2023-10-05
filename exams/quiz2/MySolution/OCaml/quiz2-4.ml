(* ************************************************ *)

(*
Q2-4: 5 points
The function list_last returns the last element of a given
list. Please give a NON-RECURSIVE implementation of list_last
based on pattern matching and list_foldleft. If the given list
is empty, raise the Empty exception
*)

(* ************************************************ *)

exception Empty

let element (xs: 'a list) : 'a = 
  match xs with
  |[] -> raise Empty 
  |a::_ -> a
;;


let list_last(xs: 'a list): 'a = 
  match xs with 
  |[] -> raise Empty 
  |_ -> element (((list_foldleft(list_reverse(xs)) ([]) (fun acc x -> if acc = [] then x::acc else acc))))
;;
