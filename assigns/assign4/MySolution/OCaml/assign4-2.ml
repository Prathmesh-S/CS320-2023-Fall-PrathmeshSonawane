(* ****** ****** *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let rec stream_listize (n : int) (st : 'a stream) : 'a list =
  if n <= 0 then []
  else
    match st () with
    | StrNil -> []
    | StrCons (hd, tl) ->
      hd :: stream_listize (n - 1) tl
    ;;

let theNatPairs: (int*int) stream = 
let rec lazyloop num x y = 
  fun () ->
    if (x >=num) 
      then StrCons ((x,y), (lazyloop(num+1) (0) (num+1)))
    else 
      StrCons((x,y), lazyloop(num) (x+1) (y-1) )
in lazyloop(0) (0) (0)
;;