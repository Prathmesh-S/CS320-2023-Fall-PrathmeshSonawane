(* ****** ****** *)
(*
//
Assign6:
Parsing and parsing combinators
//
DUE: the 13th of November, 2023
*)
(* ****** ****** *)

(*
//
Assign6-1:
//
Please implement a print and parse function. Using parser combinators. When
given a valid string according to the grammar, your parse function returns an
sexpr value encoding the expression.

//
let sexpr_to_string (e : sexpr)  : string       = ...
let sexpr_parse     (s : string) : sexpr option = ...
//

Example (Accepted Strings):
parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])
//
Example (Rejected Strings):
parse "()" = None
parse "(add)" = None
parse "(add 1 2))" = None
parse "((mul 1 2)" = None
//
*)

(* ****** ****** *)

(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<exprs> ::= <expr> | <expr> <exprs>
<expr>  ::= <num>
          | (add <exprs> )
          | (mul <exprs> )
*)

type sexpr =
  | SInt of int        (* 1, 2, 3, 4 ...  *)
  | SAdd of sexpr list (* (add e1 e2 ...) *)
  | SMul of sexpr list (* (mul e1 e2 ...) *)
;;

let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)
;;

let rec trim cs =
  match whitespace(cs) with
  |Some (a,b)-> trim(b) 
  |None -> cs
;;

let rec parse_digits cs =
  match many (satisfy char_isdigit) (cs) with
    |Some (a,b) -> (a,b)
    |None -> ([],cs)
;;


let parse_int cs =
  let xs = parse_digits(cs) in
  match xs with
  | ([],b) -> None
  | (a,b) ->
    let n = list_foldleft a 0 (fun acc x -> acc * 10 + x) in
    Some (Int n, b)
  ;;

  



let sexpr_parse (s : string) : sexpr option =
  match string_parse () s with
  |Some (x,[]) -> Some x
  | _ -> None 
;;