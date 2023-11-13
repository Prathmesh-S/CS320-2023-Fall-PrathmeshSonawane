(* ****** ****** *)
(* ****** ****** *)
#use "./../../../classlib/OCaml/MyOCaml.ml";;

let rec largest(whole:string)(sub:string)(index:int):string = 
if (index >= string_length(whole))
  then sub
else 
  if (string_length(sub) =0) 
    then 
      let including =  largest(whole)(string_snoc(sub)(string_get_at(whole)(index)))(index+1) in
          let excluding = largest(whole)(sub)(index+1) in
          if (string_length (including) >= string_length (excluding)) then
          including
          else 
          excluding
  else 
    if (string_get_at(whole)(index) >= string_get_at(sub)(string_length(sub)-1)) 
      then
        let including =  largest(whole)(string_snoc(sub)(string_get_at(whole)(index)))(index+1) in
          let excluding = largest(whole)(sub)(index+1) in
          if (string_length (including) >= string_length(excluding)) then
          including
          else 
          excluding
      else
        largest(whole)(sub)(index+1)
;;

let string_longest_ascend(xs: string): string = 
  if (string_length (xs) <2)
    then xs 
  else 
    largest(xs)("")(0)
;;