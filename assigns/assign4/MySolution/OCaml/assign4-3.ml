(* ****** 
   

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


  (* ****** 
Taken fron Piazza
****** *)
let fchildren
(x0: 'a gtree): 'a gtree list =
match x0 with
| GTnil -> [] | GTcons(_, xs) -> xs
;;

(* ****** 
Taken fron Lecture Notes
****** *)
let rec gtree_bfs( nxs: 'node list)( fchildren : 'node -> 'node list): 'node stream = fun() ->
(
match nxs with
|  [] -> StrNil
| nx1 :: nxs ->
  StrCons(nx1, gtree_bfs(nxs @ fchildren(nx1))(fchildren))
)
;;

let rec helperFunc(theStream) = fun () ->
  match theStream() with
  | StrNil -> StrNil
  | StrCons(a, b) -> 
    match a with
    | GTnil -> helperFunc(b)()
    | GTcons(c, gxs) -> StrCons(c, helperFunc(b))
;;


let rec gtree_streamize_bfs(xs: 'a gtree): 'a stream =
  fun() -> helperFunc(gtree_bfs([xs])(fchildren))()
;;
    


