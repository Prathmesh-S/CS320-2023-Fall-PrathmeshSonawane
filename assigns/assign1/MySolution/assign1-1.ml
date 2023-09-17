(* ****** ****** *)
#use "./../../../classlib/OCaml/MyOcaml.ml";;

  let concat str1 str2 =
    let len1 = string_length str1 in
    let len2 = string_length str2 in
    let result_len = len1 + len2 in
  
    let result = string_init result_len (fun i ->
      if i < len1 then
        string_get_at (str1) (i)
      else
        string_get_at (str2) (i-len1)
    ) in
  
    result
  ;;
  
  let rec build (number:int) (word: string):string = if number <> 0 then 
    if number > 0 then  build (number/10) (concat (str((chr((number mod 10)+48)))) (word))
    else build (number/10) (concat (str((chr(((number mod 10) * -1)+48)))) (word))
     else word;;
  
  let int2str(i0: int): string = 
  if i0 = 0 then "0" else 
      if i0 >0 then
      (build) i0 ""
      else 
        concat ("-")  ((build) i0 "")
  ;;

  let rec tenpower (digit:int) (x:int) (answer:int):int = 
    if (x<=1) then (digit * answer) else tenpower(digit)(x-1)(answer * 10);;
  
  let rec intrev10(n: int): int = 
    if (n <10) then n 
    else 
      tenpower (n mod 10) (string_length(int2str(n))) (1) + intrev10(n / 10)
    ;;