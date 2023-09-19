(* ************************************************ *)

(*

 Question 8: 20 points
 Please give a NON-RECURSIVE implementation of sort5
 that takes 5 integers and returns a tuple that consists
 exactly of the 5 given integers ordered increasingly

 For instance, sort5(1, 2, 1, 2, 1) = (1, 1, 1, 2, 2)
 For instance, sort5(1, 3, 4, 5, 2) = (1, 2, 3, 4, 5)
 For instance, sort5(1, 3, 5, 4, 2) = (1, 2, 3, 4, 5)

 You can implement your own helper functions as long as
 you do not make use of recursion.

*)

let sort5: int*int*int*int*int -> int*int*int*int*int =
  (* YOUR CODE *)
  if (int1<=int2<=int3<=int4<=int5) 
    then (int1, int2, int3, int4, int5)
  else 
    (int1, int2, int3, int4, int5)
  ;;
    (* I was not sue how to access arguments. Additionally, I could only find a recursive way to do this solution. As I could not find the right answer, my else has a random return *)

(* ************************************************ *)
