(* ****** ****** *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "../../assign3.ml";;

let list_map = fun xs -> foreach_to_map_list(list_foreach)(xs);;

let list_length(xs: 'a list):int = 
  list_foldright (xs) (0) (fun i acc -> acc+1)
;;

let list_subsets (xs: 'a list): 'a list list =
  list_foldright (xs) ([[]]) (fun i acc ->
     list_append (acc) (list_map(acc)(fun xs -> i::xs)))
;;

let getTails(xss: 'a list list):  'a list list = 
  match xss with 
  | []::a-> a
  | _ ->[]
;;


let list_nchoose (xs: 'a list)(n0: int): 'a list list =
  let list_of_lists = list_subsets(xs) in 
    getTails (list_foldright (list_of_lists) ([[]]) (fun i acc -> if (list_length(i) <=n0) then i::acc else acc))
;;