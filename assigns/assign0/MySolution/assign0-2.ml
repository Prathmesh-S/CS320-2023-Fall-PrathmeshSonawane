let rec divisible (old:int)(newer:int) = if newer <= 1 then true else if old mod newer = 0 then false else divisible old (newer-1);;

let isPrime(n0:int): bool = if (n0>=2) then (divisible) (n0) (n0-1) else false;;
