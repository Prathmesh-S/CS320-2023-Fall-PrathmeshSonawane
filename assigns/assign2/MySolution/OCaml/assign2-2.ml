(* ****** ****** *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign2.ml";;

let rec mylist_length(xs: 'a mylist): int =
  match xs with 
  |MyNil -> 0
  |MyCons(x1,x2) -> 1 + mylist_length(x2)
  | MySnoc(x1,x2) -> 1 + mylist_length(x1) 
  | MyReverse(x1) -> mylist_length(x1) 
  | MyAppend2(x1,x2) -> mylist_length(x1) + mylist_length(x2)
;;


let rec
mylist_get_at(xs: 'a mylist)(i0: int): 'a =
if (i0 >=mylist_length(xs))
  then mylist_subscript_exn()
else
  match xs with
  | MyNil -> mylist_subscript_exn()
  | MyCons(x1,x2) -> if (i0 = 0) then x1 else mylist_get_at(x2)(i0-1)
  | MySnoc(x1,x2) -> if (mylist_length(x1)= i0) then x2 else mylist_get_at(x1)(i0)
  | MyReverse(x1) -> mylist_get_at(x1)(mylist_length(x1)-1-i0)
  | MyAppend2(x1,x2) -> if (i0 <=mylist_length(x1)-1) then mylist_get_at(x1)(i0) else mylist_get_at(x2)(i0-mylist_length(x1))

  let xs0 = MyNil
  let xs1 = MyCons(10, xs0)
  let xs2 = MySnoc(xs0, -10)
  let xs3 = MyAppend2(xs1, xs2)
  let xs4 = MyReverse(xs3)
  let xs5 = MyAppend2(xs4, xs4)
  let xs6 = MyAppend2(xs5, xs5)
  let xs7 = MyAppend2(xs6, xs6)
  ;;
  (* ****** ****** *)
  let ( ) = assert(10 = mylist_get_at(xs7)(1))
  let ( ) = assert(10 = mylist_get_at(xs7)(3))
  let ( ) = assert(10 = mylist_get_at(xs7)(5))
  let ( ) = assert(10 = mylist_get_at(xs7)(7))
  ;;
  (* ****** ****** *)
  let ( ) = assert(10 = -mylist_get_at(xs7)(0))
  let ( ) = assert(10 = -mylist_get_at(xs7)(2))
  let ( ) = assert(10 = -mylist_get_at(xs7)(4))
  let ( ) = assert(10 = -mylist_get_at(xs7)(6))
  ;;