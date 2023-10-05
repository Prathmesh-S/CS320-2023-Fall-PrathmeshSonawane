(* ************************************************ *)

(*
Q2-6: 10 points

The function list_reverse return the reverse of a given list.
Please give an implementation of list_reverse based on list_foldright
(not list_foldleft).
*)

(* ************************************************ *)

let list_reverse(xs: 'a list): 'a list = 
let folder x acc = list_foldright acc (x :: []) (fun i acc -> i :: acc) in
    list_foldright xs [] folder
  ;;
