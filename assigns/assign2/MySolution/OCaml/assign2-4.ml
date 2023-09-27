(* ****** ****** *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign2.ml";;

let
string_append
(cs1:string)
(cs2:string): string =
string_make_fwork
(fun work ->
 (string_foreach(cs1)(work); string_foreach(cs2)(work)))

let string_reverse(x:string):string = 
string_rmake_fwork (fun work ->
 (string_foreach(x)(work)))
;;

let rec finalString(n:int)(x:string):string = 
  string_init (string_length (x)-n) (fun i -> string_get_at(x)(i))
;;

let string_sepjoin_list
(sep: string)(xs: string list): string = 

finalString(string_length (sep))(
let oldString = list_foldleft(xs)("")(fun acc i -> 
    string_append(string_append(string_reverse(sep))(i))(acc)
)

in string_reverse(oldString)
)
;;