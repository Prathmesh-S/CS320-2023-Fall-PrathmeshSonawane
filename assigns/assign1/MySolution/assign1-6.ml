(* ****** ****** *)
(* ****** ****** *)
#use "./../../../classlib/OCaml/MyOCaml.ml";;

let rec third (x:string) (a:int) (b:int) (c:int) (d:int):bool = 
  if ((b = (string_length(x)-2))|| (c = (string_length(x)-1)) || (d >= (string_length(x))))
    then false 
  else 
    if ((ord(string_get_at (x) (a)) < ord(string_get_at (x) (c))) && (ord (string_get_at (x) (c)) < ord(string_get_at (x) (b))) && (ord (string_get_at (x) (b)) < ord(string_get_at (x) (d))))
      then true 
    else  
      third (x) (a) (b) (c) (d+1)
;;

let rec second (x:string) (a:int) (b:int) (c:int) (d:int):bool = 
  if ((b = (string_length(x)-2))|| (c = (string_length(x)-1)) || (d >= (string_length(x))))
    then false 
  else 
    if (third (x)(a)(b)(c)(d) = true)
      then true 
    else
      (third (x)(a)(b)(c+1)(d) = true)
;;


let rec first (x:string) (a:int) (b:int) (c:int) (d:int):bool = 
  if ((b = (string_length(x)-2))|| (c = (string_length(x)-1)) || (d >= (string_length(x))))
    then false 
  else 
    if (second (x)(a)(b)(c)(d) = true)
      then true 
    else
      (second (x)(a)(b+1)(c)(d) = true)
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