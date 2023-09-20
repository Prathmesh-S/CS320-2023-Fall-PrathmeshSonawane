(* ****** ****** *)
#use "./../../../classlib/OCaml/MyOCaml.ml";;

let rec second (x:string) (a:int) (b:int) (c:int):bool = 
  let () = Printf.printf "%s:   %i    %i    %i  \n" x a b c in
  if ((b = (string_length(x)-1)) || (c >= (string_length(x))))
    then false 
  else 
    if ((ord(string_get_at (x) (a)) < ord(string_get_at (x) (c))) && (ord (string_get_at (x) (c)) < ord(string_get_at (x) (b))))
      then true 
    else  
      second (x) (a) (b) (c+1)
;;


let rec first (x:string) (a:int) (b:int) (c:int):bool = 
  if ((b = (string_length(x)-1)) || (c >= (string_length(x))))
    then false 
  else 
    if (second (x)(a)(b)(c) = true)
      then true 
    else
      (first (x)(a)(b+1)(c+1) = true)
;;


let rec string_avoid_132(cs: string): bool = 
  if (string_length (cs) <3)
    then true 
else 
  let a = 0 in 
    let b = 1 in
      let c = 2 in 
        if ((first (cs)(a)(b)(c)) = true)
          then false 
        else 
          string_avoid_132(string_tail(cs))
;;