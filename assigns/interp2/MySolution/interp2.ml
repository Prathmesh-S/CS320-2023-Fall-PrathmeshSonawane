#use "./../../../classlib/OCaml/MyOCaml.ml";;

let ws : unit parser =
   (many whitespace) >| ()

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

Constants
⟨digit⟩::= 0|1|2|3|4|5|6|7|8|9 
⟨nat⟩ ::= ⟨digit⟩ | ⟨digit⟩⟨nat⟩
⟨int⟩ ::= ⟨nat⟩ | -⟨nat⟩
⟨bool⟩ ::= True | False
⟨const⟩ ::= ⟨int⟩ | ⟨bool⟩ | Unit

Programs
⟨prog⟩ ::= ⟨coms⟩
⟨com⟩ ::= Push ⟨const⟩|Pop|Trace|Add|Sub|Mul|Div|And|Or|Not|Lt|Gt
⟨coms⟩ ::= ε | ⟨com⟩; ⟨coms⟩

*)

(*Types*)
type s = string
type const = I of int | B of bool | Unit | S of s | Closure of s * (s*const) list * coms
and com = Push of const | Pop | Trace | Add | Sub | Mul | Div | And | Or | Not | Lt | Gt | Swap | If of coms * coms | Bind | Lookup | Fun of coms | Call | Return
and coms = Empty | Sequence of com * coms

type variablelist = (s * const) list


(*Parse Integer which takes Char List*)
let integer : const parser =
   (let* _ = char '-' in
   let* x = natural in pure  (I (-x)))
   <|>
     (let* x = natural in pure (I x))
;;
(* Boolean Parse which takes Char List*)
let boolean :  const parser =
   (let* x = keyword "True" in pure (B true))
   <|>
   (let* x = keyword "False" in pure (B false))
;;
(* Unit Parse which takes Char List*)
let unitparse :  const parser =
   (let* x = keyword "Unit" in pure (Unit))
;;

(*Char Parser that will be used in Symbol Parser*)
let symb_parser : char parser =
  let is_lowercase_letter c = ((c >= 'a' && c <= 'z') || char_isdigit c ) in
  satisfy is_lowercase_letter
;;

(*Char Parser that will be used in Symbol Parser*)
let char_parser : char parser =
   let is_lowercase_letter c = ((c >= 'a' && c <= 'z')) in
   satisfy is_lowercase_letter
 ;;

(*Symbol Integer which takes Char List*)
let symbolparse : const parser =
  let* firstChar = char_parser in 
  let* rest = many symb_parser in
  let rec string_flat list = match list with 
                          |[] -> ""
                          |a::b -> string_append (string_cons a "")(string_flat b) in
  pure (S (string_append (string_cons firstChar "") (string_flat rest)))
;;

(* Parse a const value*)
let constFun: const parser = 
integer<|>boolean<|>unitparse<|>symbolparse
;;
let rec comParse () =
   (let* _ = ws in
    let* _ = keyword "Push" in
    let* _ = ws in
    let* x = constFun in
    let* _ = ws in
    pure (Push x))
   <|>
   (
   let* _ = ws in
   let* _ = keyword "Pop" in
     pure (Pop)
   )
   <|>
   (
   let* _ = ws in
   let* _ = keyword "Trace" in
     pure (Trace)
   )
   <|>
   (
   let* _ = ws in
   let* _ = keyword "Add" in
     pure (Add)
   )
   <|>
   (
   let* _ = ws in
   let* _ = keyword "Sub" in
     pure (Sub)
   )
   <|>
   (
   let* _ = ws in
   let* _ = keyword "Mul" in
     pure (Mul)
   )
   <|>
   (
   let* _ = ws in
   let* _ = keyword "Div" in
     pure (Div)
   )
   <|>
   (
   let* _ = ws in
   let* _ = keyword "And" in
     pure (And)
   )
   <|>
   (
   let* _ = ws in
   let* _ = keyword "Or" in
     pure (Or)
   )
   <|>
   (
   let* _ = ws in
   let* _ = keyword "Not" in
     pure (Not)
   )
   <|>
   (
   let* _ = ws in
   let* _ = keyword "Lt" in
     pure (Lt)
   )
   <|>
   (
   let* _ = ws in
   let* _ = keyword "Gt" in
     pure (Gt)
   )
   <|>
   (
   let* _ = ws in
   let* _ = keyword "Swap" in
      pure (Swap)
   )
   <|>
   (let* _ = ws in
    let* _ = keyword "If" in
    let* _ = ws in
    let* first = comsParse () in
    let* _ = ws in
    let* _ = keyword "Else" in
    let* _ = ws in
    let* second = comsParse () in
    let* _ = ws in
    let* _ = keyword "End" in
    let* _ = ws in
    pure (If (first, second))
    )
    <|>
    (
    let* _ = ws in
    let* _ = keyword "Bind" in
        pure (Bind)
    )
    <|>
    (
    let* _ = ws in
    let* _ = keyword "Lookup" in
        pure (Lookup)
    )
    <|>
   (let* _ = ws in
    let* _ = keyword "Fun" in
    let* _ = ws in
    let* first = comsParse () in
    let* _ = ws in
    let* _ = keyword "End" in
    let* _ = ws in
    pure (Fun first)
    )
    <|>
    (
    let* _ = ws in
    let* _ = keyword "Call" in
        pure (Call)
    )
    <|>
    (
    let* _ = ws in
    let* _ = keyword "Return" in
        pure (Return)
    )


and comsParse () = 
   (let* _ = ws in
   let* command = comParse () in
   (
   let* _ = ws in
   let* _ = char ';' in
   let* _ = ws in
   let* rest = comsParse () in
   pure (Sequence (command, rest)))
  <|> 
   pure (Sequence (command, Empty)))
   <|>
   (
      let* _ = ws in
      let* _ = keyword "" in
      pure Empty
   )

(*
let parser (s : string) : coms option =
   match string_parse (comsParse()) s with
  | Some (e, []) -> Some e
  | _ -> None
;;

*)

(*Int to String Function*)
let rec
nat2str(n0: int): string =
if n0 < 10 then str(chr(n0+48))
else string_snoc(nat2str(n0/10))(chr(n0 mod 10 + 48))

let
int2str(n0: int): string =
if n0 >= 0 then nat2str(n0) else string_cons('-')(nat2str(-n0))
;;

let toString (const:const):string = 
   match const with 
   |B a ->( match a with 
      |true -> "True"
      |false -> "False"
         )
   |I b-> int2str (b)
   |Unit -> "Unit"
   |S a -> a
   |Closure (a,b,c) -> string_append (string_append ("Fun<") (a)) (">")
;;

let listTail (list: const list ):  const list = 
   match list with 
      | a::b -> b 
      |_ -> []
;;
(*This function return Unit but it should never have to*)
let listHead (list: const list ):  const = 
   match list with 
      | a::b -> a 
      |_ -> Unit
;;

(* Add two programs together*)

let rec concatenate_coms coms1 coms2 =
match coms1 with
| Empty -> coms2
| Sequence (cmd, rest) -> Sequence (cmd, concatenate_coms rest coms2)
;;


(*New type for variable enviornment*)
type variablelist = (s * const) list

(*Search for a symbol in the valueList *)
let rec find_s_in_variablelist (variable_list : variablelist) (symbol : s) : const option =
   match variable_list with
   | [] -> None  (* Symbol not found in the list *)
   | (s, const_value) :: rest ->
     if s = symbol then
       Some const_value
     else
       find_s_in_variablelist rest symbol
 ;;

let rec interp1 (coms:coms)(stacklist: const list) (tracelist:string list) (valueList: variablelist): string list =
   match coms with
   | Sequence(a, b) ->
     (match a with 
      | Push(c) -> interp1 b (c :: stacklist) (tracelist)(valueList)
      | Pop -> (match stacklist with 
               |[] -> ("Panic"::tracelist)
               |_ -> interp1 (b)(listTail stacklist) (tracelist)(valueList))
      |Trace -> (match stacklist with
               |[] -> ("Panic"::tracelist)
               |_ -> let head = listHead (stacklist) in interp1 (b)(Unit::(listTail stacklist))(toString head::tracelist)(valueList))
      |Add -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | e::[] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in 
                        let head2 = listHead (listTail stacklist) in 
                           match head with 
                              | I firstNum -> (match head2 with 
                                          |I secondNum -> interp1 (b)((I (firstNum+secondNum))::(listTail (listTail stacklist))) (tracelist)(valueList)
                                          |_ -> ("Panic"::tracelist))
                              | _ -> ("Panic"::tracelist)))
      |Sub -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | e::[] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in 
                        let head2 = listHead (listTail stacklist) in 
                           match head with 
                              | I firstNum -> (match head2 with 
                                          |I secondNum -> interp1 (b)((I (firstNum-secondNum))::(listTail (listTail stacklist))) (tracelist)(valueList)
                                          |_ -> ("Panic"::tracelist))
                              | _ -> ("Panic"::tracelist)))
      | Mul -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | e::[] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in 
                        let head2 = listHead (listTail stacklist) in 
                           match head with 
                              | I firstNum -> (match head2 with 
                                          |I secondNum -> interp1 (b)((I (firstNum * secondNum))::(listTail (listTail stacklist))) (tracelist)(valueList)
                                          |_ -> ("Panic"::tracelist))
                              | _ -> ("Panic"::tracelist)))
      |Div -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | e::[] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in 
                        let head2 = listHead (listTail stacklist) in 
                           match head with 
                              | I firstNum -> (match head2 with 
                                          |I secondNum -> if (secondNum = 0) then ("Panic"::tracelist) else interp1 (b)((I (firstNum / secondNum))::(listTail (listTail stacklist))) (tracelist)(valueList)
                                          |_ -> ("Panic"::tracelist))
                              | _ -> ("Panic"::tracelist)))
      |And -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | e::[] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in 
                        let head2 = listHead (listTail stacklist) in 
                           match head with 
                              | B firstBool -> (match head2 with 
                                          |B secondBool -> interp1 (b)((B (firstBool && secondBool))::(listTail (listTail stacklist))) (tracelist)(valueList)
                                          |_ -> ("Panic"::tracelist))
                              | _ -> ("Panic"::tracelist)))
      |Or -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | e::[] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in 
                        let head2 = listHead (listTail stacklist) in 
                           match head with 
                              | B firstBool -> (match head2 with 
                                          |B secondBool -> interp1 (b)((B (firstBool || secondBool))::(listTail (listTail stacklist))) (tracelist)(valueList)
                                          |_ -> ("Panic"::tracelist))
                              | _ -> ("Panic"::tracelist)))
      |Not -> (match stacklist with
               |[] -> ("Panic"::tracelist)
               |_ -> let head = listHead (stacklist) in 
                  match head with
                     |B boolean -> if (boolean = true) then interp1 (b)(B false::listTail stacklist) (tracelist) (valueList) else interp1 (b)(B true::listTail stacklist) (tracelist)(valueList)
                     |_ -> ("Panic"::tracelist))
      |Lt -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | e::[] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in 
                        let head2 = listHead (listTail stacklist) in 
                           match head with 
                              | I firstNum -> (match head2 with 
                                          |I secondNum -> interp1 (b)((B (firstNum < secondNum))::(listTail (listTail stacklist))) (tracelist)(valueList)
                                          |_ -> ("Panic"::tracelist))
                              | _ -> ("Panic"::tracelist)))
      |Gt -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | e::[] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in 
                        let head2 = listHead (listTail stacklist) in 
                           match head with 
                              | I firstNum -> (match head2 with 
                                          |I secondNum -> interp1 (b)((B (firstNum > secondNum))::(listTail (listTail stacklist))) (tracelist)(valueList)
                                          |_ -> ("Panic"::tracelist))
                              | _ -> ("Panic"::tracelist)))
      |If (branch1,branch2) ->  (match stacklist with 
               |[] -> ("Panic"::tracelist)
               |_ -> let head = listHead (stacklist) in 
                        match head with
                           |B boolean -> if (boolean = true) then interp1 (concatenate_coms branch1 b)(listTail stacklist)(tracelist)(valueList) else interp1 (concatenate_coms branch2 b)(listTail stacklist)(tracelist)(valueList)
                           |_ -> ("Panic"::tracelist))
      |Bind -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | e::[] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in 
                        let head2 = listHead (listTail stacklist) in 
                           (match head with 
                              | S firstSymbol -> interp1 (b)((listTail (listTail stacklist))) (tracelist)((toString head,head2)::valueList)
                              | _ -> ("Panic"::tracelist))))
      |Lookup -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in  
                           match head with 
                              | S symbol -> (match find_s_in_variablelist valueList symbol with
                                             |Some value -> interp1 (b)(value::listTail stacklist) (tracelist)(valueList)
                                             |_ -> ("Panic"::tracelist))
                              | _ -> ("Panic"::tracelist)))
      |Swap -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | e::[] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in 
                        let head2 = listHead (listTail stacklist) in 
                            interp1 (b)((head2::head::(listTail (listTail stacklist)))) (tracelist)(valueList))))
   | Empty -> tracelist
   ;;


   (* This is the main function*)


let interp (s : string) : string list option = 
   match string_parse (comsParse()) s with
   | Some (e, []) -> Some (interp1 e [] [] [])
   | _ -> None
;;