let solve (n: int) (l: int list) : int =
  let hmax =
    let rec find_hmax hmax h = function
      | []   -> hmax
      | hdiff::t -> let h' = h+hdiff in find_hmax (max hmax h') h' t
    in
    find_hmax 0 0 l
  in

  let rec sauts h = function
    | [] -> []
    | hdiff::t -> hdiff::(if h = hmax then [] else sauts (h+hdiff) t)
  in
  List.fold_left max 0 (sauts 0 l)

let _ =
  let n = read_int() in
  read_line()
  |> String.split_on_char ' '
  |> List.map int_of_string
  |> solve n
  |> Printf.printf "%i"
