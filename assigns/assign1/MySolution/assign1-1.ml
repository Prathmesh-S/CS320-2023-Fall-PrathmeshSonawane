(* ****** ****** *)
#use "./../../../classlib/OCaml/MyOCaml.ml";;
  (* ****** 
  let rec intrev10(n: int): int = 
    if (n <10) then n 
    else 
      tenpower (n mod 10) (string_length(int2str(n))) (1) + intrev10(n / 10)
    ;;
    ****** *)

  let rec findsol(n:int)(answer:int)(magnitude:int):int = 
    if (n <=0) then answer
    else  
      findsol(n / 10)(answer + (magnitude * (n mod 10)))(magnitude / 10)
    ;;

  let rec magnitudefunc (x:int) (answer:int):int =
    if (x<10) then answer 
    else magnitudefunc(x/10)(answer * 10)
  ;;

  let intrev10(n: int): int = 
    if (n <10) then n 
    else findsol(n)(0)(magnitudefunc(n)(1))
  ;;