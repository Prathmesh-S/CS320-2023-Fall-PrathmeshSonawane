#use "./../../../classlib/OCaml/MyOCaml.ml";;

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
type const = I of int | B of bool | Unit 
type com = Push of const | Pop | Trace | Add | Sub | Mul | Div | And | Or | Not | Lt | Gt
type coms = Empty | Sequence of com * coms


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
(* Parse a const value*)
let constFun: const parser = 
integer<|>boolean<|>unitparse
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
;;

let rec comsParse () = 
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


let rec interp1 (coms:coms)(stacklist: const list) (tracelist:string list): string list =
   match coms with
   | Sequence(a, b) ->
     (match a with 
      | Push(c) -> interp1 b (c :: stacklist) (tracelist)
      | Pop -> (match stacklist with 
               |[] -> ("Panic"::tracelist)
               |_ -> interp1 (b)(listTail stacklist) (tracelist))
      |Trace -> (match stacklist with
               |[] -> ("Panic"::tracelist)
               |_ -> let head = listHead (stacklist) in interp1 (b)(Unit::(listTail stacklist))(toString head::tracelist))
      |Add -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | e::[] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in 
                        let head2 = listHead (listTail stacklist) in 
                           match head with 
                              | I firstNum -> (match head2 with 
                                          |I secondNum -> interp1 (b)((I (firstNum+secondNum))::(listTail (listTail stacklist))) (tracelist)
                                          |_ -> ("Panic"::tracelist))
                              | _ -> ("Panic"::tracelist)))
      |Sub -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | e::[] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in 
                        let head2 = listHead (listTail stacklist) in 
                           match head with 
                              | I firstNum -> (match head2 with 
                                          |I secondNum -> interp1 (b)((I (firstNum-secondNum))::(listTail (listTail stacklist))) (tracelist)
                                          |_ -> ("Panic"::tracelist))
                              | _ -> ("Panic"::tracelist)))
      | Mul -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | e::[] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in 
                        let head2 = listHead (listTail stacklist) in 
                           match head with 
                              | I firstNum -> (match head2 with 
                                          |I secondNum -> interp1 (b)((I (firstNum * secondNum))::(listTail (listTail stacklist))) (tracelist)
                                          |_ -> ("Panic"::tracelist))
                              | _ -> ("Panic"::tracelist)))
      |Div -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | e::[] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in 
                        let head2 = listHead (listTail stacklist) in 
                           match head with 
                              | I firstNum -> (match head2 with 
                                          |I secondNum -> if (secondNum = 0) then ("Panic"::tracelist) else interp1 (b)((I (firstNum / secondNum))::(listTail (listTail stacklist))) (tracelist)
                                          |_ -> ("Panic"::tracelist))
                              | _ -> ("Panic"::tracelist)))
      |And -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | e::[] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in 
                        let head2 = listHead (listTail stacklist) in 
                           match head with 
                              | B firstBool -> (match head2 with 
                                          |B secondBool -> interp1 (b)((B (firstBool && secondBool))::(listTail (listTail stacklist))) (tracelist)
                                          |_ -> ("Panic"::tracelist))
                              | _ -> ("Panic"::tracelist)))
      |Or -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | e::[] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in 
                        let head2 = listHead (listTail stacklist) in 
                           match head with 
                              | B firstBool -> (match head2 with 
                                          |B secondBool -> interp1 (b)((B (firstBool || secondBool))::(listTail (listTail stacklist))) (tracelist)
                                          |_ -> ("Panic"::tracelist))
                              | _ -> ("Panic"::tracelist)))
      |Not -> (match stacklist with
               |[] -> ("Panic"::tracelist)
               |_ -> let head = listHead (stacklist) in 
                  match head with
                     |B boolean -> if (boolean = true) then interp1 (b)(B false::listTail stacklist) (tracelist) else interp1 (b)(B true::listTail stacklist) (tracelist)
                     |_ -> ("Panic"::tracelist))
      |Lt -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | e::[] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in 
                        let head2 = listHead (listTail stacklist) in 
                           match head with 
                              | I firstNum -> (match head2 with 
                                          |I secondNum -> interp1 (b)((B (firstNum < secondNum))::(listTail (listTail stacklist))) (tracelist)
                                          |_ -> ("Panic"::tracelist))
                              | _ -> ("Panic"::tracelist)))
      |Gt -> (match stacklist with
               | [] -> ("Panic"::tracelist)
               | e::[] -> ("Panic"::tracelist)
               | _ -> (let head = listHead (stacklist) in 
                        let head2 = listHead (listTail stacklist) in 
                           match head with 
                              | I firstNum -> (match head2 with 
                                          |I secondNum -> interp1 (b)((B (firstNum > secondNum))::(listTail (listTail stacklist))) (tracelist)
                                          |_ -> ("Panic"::tracelist))
                              | _ -> ("Panic"::tracelist))))
   | Empty -> tracelist
   ;;


   (* This is the main function*)


let interp (s : string) : string list option = 
   match string_parse (comsParse()) s with
   | Some (e, []) -> Some (interp1 e [] [])
   | _ -> None
;;