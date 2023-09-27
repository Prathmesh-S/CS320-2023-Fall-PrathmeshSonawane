(* ****** ****** *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "../../assign2.ml";;

let rec mylist_length(xs: 'a mylist): int =
  match xs with 
  |MyNil -> 0
  |MyCons(x1,x2) -> 1 + mylist_length(x2)
  | MySnoc(x1,x2) -> 1 + mylist_length(x1) 
  | MyReverse(x1) -> mylist_length(x1) 
  | MyAppend2(x1,x2) -> mylist_length(x1) + mylist_length(x2)
;;