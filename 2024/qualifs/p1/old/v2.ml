let get_heights len l =
  let heigths = Array.make (len+1) 0 in
  List.iteri (fun i x -> heigths.(i+1) <- heigths.(i)+x) l;
  heigths

let solve len l =
  let h = get_heights len l in

  let rec jumps i_last_stand max_jump i_current_branch =
    if i_current_branch = len+1 then max_jump
    else
      let diff = h.(i_current_branch) - h.(i_last_stand) in (* essayer d'utiliser direct la diff donnée par l *)
      if diff > 0 then
        (* on saute *)
        jumps i_current_branch (max max_jump diff) (i_current_branch+1)
      else
        (* on saute pas *)
        jumps i_last_stand max_jump (i_current_branch+1)
  in
  jumps 0 0 0

let _ =
  let n = read_int() in
  let l = String.split_on_char ' ' (read_line()) |> List.map int_of_string in
  solve n l
  |> Printf.printf "%i"
