(* ****** ****** *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "../../assign3.ml";;

let char_replacement(str: string)(i: int)(c: char): string =
  string_tabulate(string_length str) (fun j -> 
    if i = j then c else (string_get_at(str)(i))
  )
  ;;

  let string_creation(str: string)(i: int): 'a list =
  let alphabet = "abcdefghijklmnopqrstuvwxyz" in
  list_make_fwork (fun work -> 
      (int1_foreach (26) (fun j -> 
        let current_char = string_get_at str i in
        let swapping_char = string_get_at alphabet j in
        if current_char != swapping_char then work(char_replacement str i swapping_char)
      ))
    )

let newWords (word:string):string list = 
  let alphabet = "abcdefghijklmnopqrstuvwxyz" in 
  string_foldright (alphabet) ([]) (fun i acc -> if (string_get_at(word)(0) != i) then string_cons(i)(word)::acc else acc)
;;

let list_of_buddies(word: string): string list = 
  let answer = list_make_fwork ( fun work ->
    (int1_foreach (string_length word) (fun i -> work(string_creation(word)(i))))) 
  in list_concat (answer)
;;