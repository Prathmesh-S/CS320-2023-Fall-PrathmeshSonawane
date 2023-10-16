(* ****** 
   
  let rec gtree_streamize_bfs (xs: 'a gtree): 'a stream =
****** *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "../../assign4.ml";;

let rec list_map f lst =
  match lst with
  | [] -> []
  | head :: tail -> (f head) :: list_map f tail
;;

let rec gtree_streamize_dfs (xs: 'a gtree): 'a stream =
  match xs with
  | GTnil -> fun () -> StrNil
  | GTcons(a, b) ->
    let rec concat_streams (allstreams: 'a stream list): 'a stream =
      match allstreams with
      | [] -> fun () -> StrNil
      | s::rest -> fun () ->
        match s () with
        | StrNil -> concat_streams rest ()
        | StrCons(e, f) -> StrCons(e, fun () -> concat_streams (f :: rest) ())
    in
    fun () -> StrCons(a, fun () -> concat_streams (list_map gtree_streamize_dfs b) ())
  ;;


