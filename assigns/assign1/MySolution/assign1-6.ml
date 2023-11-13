(* ****** ****** *)
(* ****** ****** *)
#use "./../../../classlib/OCaml/MyOCaml.ml";;

let rec third (theString:string) (w:int) (x:int) (y:int) (z:int):bool = 
  if ((x = (string_length(theString)-2))|| (y = (string_length(theString)-1)) || (z >= (string_length(theString))))
    then false 
  else 
    if ((ord(string_get_at (theString) (w)) < ord(string_get_at (theString) (y))) && (ord (string_get_at (theString) (y)) < ord(string_get_at (theString) (x))) && (ord (string_get_at (theString) (x)) < ord(string_get_at (theString) (z))))
      then true 
    else  
      third (theString) (w) (x) (y) (z+1)
;;

let rec second (x:string) (a:int) (b:int) (c:int) (d:int):bool = 
  if ((b = (string_length(x)-2))|| (c = (string_length(x)-1)) || (d >= (string_length(x))))
    then false 
  else 
    if (third (x)(a)(b)(c)(d) = true)
      then true 
    else
      (second (x)(a)(b)(c+1)(d+1) = true)
;;


let rec first (x:string) (a:int) (b:int) (c:int) (d:int):bool = 
  if ((b = (string_length(x)-2))|| (c = (string_length(x)-1)) || (d >= (string_length(x))))
    then false 
  else 
    if (second (x)(a)(b)(c)(d) = true)
      then true 
    else
      (first (x)(a)(b+1)(c+1)(d+1) = true)
;;


let rec string_avoid_1324(cs: string): bool = 
  if (string_length (cs) <4)
    then true 
else 
  let a = 0 in 
    let b = 1 in
      let c = 2 in 
        let d = 3 in
        if ((first (cs)(a)(b)(c)(d)) = true)
          then false 
        else 
          string_avoid_1324(string_tail(cs))
;;