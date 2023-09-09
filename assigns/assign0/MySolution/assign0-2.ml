let isPrime (n0: int): bool 
=
   if n0>=2 then
    let numberDown = n0-1 in 
      let rec countDown numberDown x : bool = 
        if numberDown = 1
          then true
        else 
        if (n0 % numberDown) = 0 then false


   
   
   
   
   
    else false
;;

