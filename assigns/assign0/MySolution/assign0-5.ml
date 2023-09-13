(* Changed Must support all int, including 0 and negative *)
#use "../assign0.ml";;

let stringrev(cs: string): string = 
  if string_length cs <=0 then ""
  else 
    let stringLength = string_length cs in 
      let result = string_init (string_length cs)(fun i ->
       string_get (cs, stringLength-i-1)
      )
  in result
  ;;