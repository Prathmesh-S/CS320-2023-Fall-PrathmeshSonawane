(* ****** ****** *)
#use "./../../../classlib/OCaml/MyOCaml.ml";;

let
string_append
(cs1:string)
(cs2:string): string =
string_make_fwork
(fun work ->
 (string_foreach(cs1)(work); string_foreach(cs2)(work)))
;;


let rec combine (x:string) (y:string) (final:string):string = 
  if (string_length (x) <1) 
    then string_append(final) (y)
  else 
    if (string_length (y) <1)
      then string_append(final) (x)
    else
      if (ord(string_get_at (x)(0)) < ord(string_get_at (y)(0)))
        then combine (string_tail(x))(y)(string_snoc(final)(string_get_at(x)(0)))
      else  
        combine (x)(string_tail(y))(string_snoc(final)(string_get_at(y)(0)))
;;

let string_merge (x:string) (y:string):string = 
  combine(x)(y)("")
;;