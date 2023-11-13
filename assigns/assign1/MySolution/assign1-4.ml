(* ****** 
Do we need to worry about leading zero inputs?   
****** *)
#use "./../../../classlib/OCaml/MyOCaml.ml";;


let rec noLeading (oldx:string):string = 
  if (string_get_at (oldx)(0) = '0')
    then noLeading(string_tail(oldx))
  else oldx
;;

let rec stringHeader (x:string):string = 
  string_init(string_length(x)-1)(fun i -> string_get_at(x)(i))
;;


let rec int_add (ds1: string)(ds2: string) (counter:int)(answer: string): string = 
  if ((string_length (ds1) <1) && (string_length (ds2) <1) && (counter=0))
    then answer
  else  
    if ((string_length (ds1) <1) && (string_length (ds2) <1)) then
      if (counter / 10 =0)
        then (string_cons(char_of_digit(counter))(answer))
      else
        (string_cons(char_of_digit(counter / 10))(string_cons(char_of_digit(counter mod 10))(answer)))
    else
      if (string_length (ds1) <1) then
        let newCount = (counter + (ord(string_get_at (ds2) (string_length (ds2)-1))-48)) in
        int_add(ds1)(stringHeader(ds2))(newCount / 10)((string_cons(char_of_digit(newCount mod 10))(answer)))
      else
        if (string_length (ds2) <1) then
          let newCount = (counter + (ord(string_get_at (ds1) (string_length (ds1)-1))-48)) in
          int_add(stringHeader(ds1))(ds2)(newCount / 10)((string_cons(char_of_digit(newCount mod 10))(answer)))
        else 
        let newCount = (counter + (ord(string_get_at (ds1) (string_length (ds1)-1))-48) + (ord(string_get_at (ds2) (string_length (ds2)-1))-48)) in
        int_add(stringHeader(ds1))(stringHeader(ds2))(newCount / 10)((string_cons(char_of_digit(newCount mod 10))(answer)))
;;

let intrep_add(ds1: string)(ds2: string): string = 
  if ((string_length (ds1) <1) && (string_length (ds2) <1))
    then "0"
  else 
    if (string_length (ds1) <1)
      then noLeading (ds2)
    else
      if (string_length (ds2) <1)
        then noLeading(ds1)
      else 
      int_add ( noLeading (ds1)) (noLeading (ds2)) (0) ("")
;;