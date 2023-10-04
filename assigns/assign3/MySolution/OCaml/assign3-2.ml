(* ****** 
   
(list_map(i) (fun i -> i))
****** *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "../../assign3.ml";;

let list_map = fun xs -> foreach_to_map_list(list_foreach)(xs);;

let memberHead(xs: 'a list): 'a list  =
  match xs with 
  |[] -> []
  |a::b -> (a::[])
;;

let memberTail(xs: 'a list): 'a list list  =
  match xs with 
  |[] -> []
  |a::b -> b::[]
;;

let list_subsets (xs: 'a list): 'a list list =
    list_foldright (xs) ([[]]) (fun i acc ->
       list_append (acc) (list_map(acc)(fun xs -> i::xs)))
;;

