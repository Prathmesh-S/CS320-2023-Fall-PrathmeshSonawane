#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(*
#use "./../../../../classlib/OCaml/MyOCaml.ml";;
Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<expr>  ::= <num> 
          | (add <exprs> )
          | (mul <exprs> )
<exprs> ::= <expr> | <expr><exprs>

*)

(*

let rec parse_digit cs =
  match cs with
  | [] -> None
  | '0' :: cs' -> Some (0, cs')
  | '1' :: cs' -> Some (1, cs')
  | '2' :: cs' -> Some (2, cs')
  | '3' :: cs' -> Some (3, cs')
  | '4' :: cs' -> Some (4, cs')
  | '5' :: cs' -> Some (5, cs')
  | '6' :: cs' -> Some (6, cs')
  | '7' :: cs' -> Some (7, cs')
  | '8' :: cs' -> Some (8, cs')
  | '9' :: cs' -> Some (9, cs')
  | _ -> None

*)


type expr =
  | Int of int       (* 1, 2, 3, 4 ...  *)
  | Add of expr list (* (add e1 e2 ...) *)
  | Mul of expr list (* (mul e1 e2 ...) *)

(* turn a string into a list of chars *)
let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

(* remove blank chars at the front of a list *)
let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs

(* Please implement a parse function. When given a valid string according
   to the grammar, your parse function returns an expr value encoding the
   expression.

   Example (Accpeted Strings):
   parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
   parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])

   Example (Rejected Strings):
   parse "()" = None
   parse "(add)" = None
   parse "(add 1 2))" = None
   parse "((mul 1 2)" = None

*)

let parse_digit (x:char) : expr option =
  let t = int_of_char x - 48 in
  if (0 <= t) && (t <= 9) then Some (Int t) else None 
;;

let rec parse_num (cs) =
  match trim cs with
  | [] -> None
  | _ -> (
    match cs with
    | a::b -> parse_digit(a)
    | _ -> None
  )
  ;;



let parse_expr (x:char list): expr option = 
  match x with
  | x::y::z::[] -> ( match parse_digit x with
                  |Some a -> (match parse_digit z with
                             |Some c -> (match y with
                                        |'(' -> Some (Int 1)
                                        |')' -> Some (Int 3)
                                        | _ -> None)
                             |  _ -> None)
                  | _ -> None)                           
  |_ -> None
;;

let parse (s : string) : expr option = 
  parse_expr(string_listize (s))
;;