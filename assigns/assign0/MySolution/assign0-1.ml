let rec fact (x:int) = if x>0  then (x * fact(x-1)) else 1;;

let ans = fact(64);;