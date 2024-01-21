let print_int_list (l:int list) : unit =
  print_string "[";
  let rec aux (l:int list) =
    match l with
    | [] -> print_string "]"
    | t::[] -> (Printf.printf "%d]" t)
    | t::q -> (Printf.printf "%d; " t; aux q)
  in aux l

let get_heights len l =
  let heigths = Array.make (len+1) 0 in
  List.iteri (fun i x -> heigths.(i+1) <- heigths.(i)+x) l;
  heigths

let solve1 len l =

  let h = get_heights len l in

  let rec jumps i_last_stand max_jump i_current_branch h_last_stand =
    if i_current_branch = len+1 then max_jump
    else
      let diff = h.(i_current_branch) - h_last_stand (*h.(i_last_stand)*) in (* essayer d'utiliser direct la diff donnée par l *)
      if diff > 0 then
        (* on saute *)
        jumps i_current_branch (max max_jump diff) (i_current_branch+1) h.(i_current_branch)
      else
        (* on saute pas *)
        jumps i_last_stand max_jump (i_current_branch+1) h_last_stand
  in
  jumps 0 0 0 0

let solve2 len l =
  let rec sauts old_pos_h old_h = function
    | [] -> []
    | diff::t -> let new_h = old_h + diff in
      if new_h > old_pos_h then
        (* on garde la hauteur du saut qu'on vient d'effectuer *)
        diff :: sauts new_h new_h t
      else
        sauts old_pos_h (new_h) t
  in
  List.fold_left max 0 (sauts 0 0 l)

let _ =
  let len = read_int() in
  String.split_on_char ' ' (read_line())
  |> List.map int_of_string
  |> fun l -> let v = solve1 len l in if v = 55 then (print_int_list l; Printf.printf "n= %i" len); v
  |> Printf.printf "%i"
