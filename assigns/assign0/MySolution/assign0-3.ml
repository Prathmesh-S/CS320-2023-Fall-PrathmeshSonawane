(* Must support all int, including 0 and negative *)

let rec build (number:int) (word: string):string = if number <> 0 then build (number/10) ((string_of_int (number mod 10)) ^ word) else word;;

let int2str(i0: int): string = 
if i0 = 0 then "0" else 
if (i0 <0) then 
 (build) i0 "" 

else 
  (build) i0 "" 

;;

