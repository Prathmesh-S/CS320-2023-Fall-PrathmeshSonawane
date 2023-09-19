#use "./../../../classlib/OCaml/MyOCaml.ml";;
(* No other library imports are allowed. *)

(* ************************************************ *)

(* Question 7: 10 points

   Given the following snippet, implement the test
   function so that isPrime returns true for prime
   number inputs and false otherwise. *)

let rec checkPrime (old:int) (new1:int):bool = 
  if (new1 <2) then true 
  else if (old mod new1 = 0)
      then false 
  else checkPrime(old)(new1-1)
;;
let isPrime(n) =
  let test(i:int): bool = (* YOUR CODE *) checkPrime (n) (n-1)
  in
  if n < 2 then false else int1_forall(n)(test)
;;
(* ************************************************ *)
