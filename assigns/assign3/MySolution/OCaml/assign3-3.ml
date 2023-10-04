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
let subsets = list_subsets(xs) in
  list_foldleft(subsets)([])(fun acc i -> 
    if list_length i = n0 then list_append(acc)([i]) else acc)