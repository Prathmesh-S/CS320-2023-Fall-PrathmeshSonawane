let rec fact (x:int) = if x>0  then (x * fact(x-1)) else 1;;
let rec itterate ans1 = if fact(ans1) = 0 then ans1 else itterate(ans1+1);;
let myans = itterate(0);;