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
parse "(add 1 2 3)" = Some (SAdd [SInt 1; SInt 2; SInt 3])
parse "(mul (add 1 2) 3 (mul 1))" = Some (SMul [SAdd [SInt 1; SInt 2]; SInt 3; SMul [SInt 1]])
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

#use "../../../../classlib/OCaml/MyOCaml.ml";;

type sexpr =
  | SInt of int        (* 1, 2, 3, 4 ...  *)
  | SAdd of sexpr list (* (add e1 e2 ...) *)
  | SMul of sexpr list (* (mul e1 e2 ...) *)
;;

let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)
;;

(*Int to String Function*)
let rec
nat2str(n0: int): string =
if n0 < 10 then str(chr(n0+48))
else string_snoc(nat2str(n0/10))(chr(n0 mod 10 + 48))

let
int2str(n0: int): string =
if n0 >= 0 then nat2str(n0) else string_cons('-')(nat2str(-n0))

(* Concat Function for expr --> String*)
let rec concat sep strings =
  match strings with
  | [] -> ""
  | [s] -> s
  | head :: tail -> head ^ sep ^ (concat sep tail)
;;

(* Map Function*)

let rec custom_map f lst =
  match lst with
  | [] -> []
  | head :: tail -> f head :: custom_map f tail

(*WhiteSpace Function*)

let rec trim cs =
 whitespaces(cs)
;;

(*
  let rec trim cs =
    match whitespace(cs) with
    |Some (a,b)-> trim(b)
    |None -> cs
  ;;
*)
(*
parse int function 

parse_int () ['1';'2';'3'];; --> Some (SInt 123, [])
*)
let rec parse_int () : sexpr parser =
  let* n = natural in
  pure (SInt n) << trim

(*
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
*)

and parse_add () : sexpr parser =
  let* _ = keyword "(add" in
  let* n = many1' parse_expr in
  let* _ = keyword ")" in
  pure (SAdd n)

and parse_mul () : sexpr parser =
  let* _ = keyword "(mul" in
  let* n = many1' parse_expr in
  let* _ = keyword ")" in
  pure (SMul n)

and parse_expr () : sexpr parser =
    parse_int () <|> parse_add () <|> parse_mul ()

let sexpr_parse (s : string) : sexpr option =
  match string_parse (parse_expr ()) s with
  | Some (e, []) -> Some e
  | _ -> None
;;

let rec sexpr_to_string (cs:sexpr): string = 
  match cs with
    | SInt n -> int2str (n)
    | SAdd exprs -> "(add " ^ concat " " (custom_map sexpr_to_string exprs) ^ ")"
    | SMul exprs -> "(mul " ^ concat " " (custom_map sexpr_to_string exprs) ^ ")"
  ;;
