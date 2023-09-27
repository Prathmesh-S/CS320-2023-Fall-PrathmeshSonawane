(* ****** ****** *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign2.ml";;


let
foldleft_to_iforeach (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =
  fun (xs)(work) -> let_ = foldleft xs 0 (fun(idx)(x0) ->((work (idx)(x0)); idx+1)) in ()
;;