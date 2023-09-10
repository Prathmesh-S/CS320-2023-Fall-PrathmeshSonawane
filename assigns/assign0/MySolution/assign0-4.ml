(* Must support all natural numbers,
   #use "../assign0.ml";;

including empty string *)
#use "../assign0.ml";;


let rec intAnswer (theString:string) (stringLength: int) (power:int) (final: int):int = 
  if ord (string_get (theString, 0)) <> 45 
    then
      if stringLength <1 then final else intAnswer (theString) (stringLength-1) (power * 10) (final + ((ord ((string_get ((theString),(stringLength-1)))) - 48) * power))
  else
    if stringLength <=1 then final else intAnswer (theString) (stringLength-1) (power * 10) (final - ((ord ((string_get ((theString),(stringLength-1)))) - 48) * power))

let str2int(cs: string): int =
  if string_length cs <=0 
    then 0 
  else 
    let stringSize = string_length cs in 
      let result = (intAnswer) cs stringSize 1 0
      in 
    result
;;

