(* ****** ****** *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "../../assign3.ml";;

let rec list_length(xs: 'a list): int =
  match xs with 
  |[] -> 0
  |a::b -> 1 + list_length(b)
  
;;

let memberListLength(xs: 'a list): int =
  match xs with 
  |[] -> 0
  |a::b -> list_length(a)
;;

let rec getHeads(xss: 'a list list): 'a list = 
  match xss with 
  |[] -> []
  |[]::_ -> []
  | (a::b)::c -> a:: getHeads(c)
;;

let rec getTails(xss: 'a list list):  'a list list = 
  match xss with 
  |[] -> []
  |[]::_ -> []
  | (a::b)::c -> b:: getTails(c)
;;

let rec matrix_transpose(xss: 'a list list): 'a list list = 
    (* match xss with 
    | [] -> []
    | a::b -> match a with 
      | [] -> []
      | c::d -> c:: matrix_transpose(b) *)

    match xss with 
    | [] -> []
    | []::_ -> []
    | _ -> 
      let heads = getHeads(xss) in
      let tails = getTails(xss) in
      heads:: matrix_transpose(tails)
;;

  