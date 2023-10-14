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

let the_ln2_stream: float stream = 
  let rec lazyloop i num pos = 
    fun () ->
      if (pos == false)
        then StrCons(i, lazyloop(i +. (1./.(num+.1.0))) (num+.1.0)(true))
      else 
        let () = print_string "hello" in
        StrCons(i, lazyloop (i -. (1./.(num+.1.0))) (num+.1.0)(false))
  in lazyloop(1.0)(1.)(true)
;;