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




let interp (s : string) : string list option = 
None
;;