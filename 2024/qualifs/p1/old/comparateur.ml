open Random

let print_int_list (l:int list) : unit =
  print_string "[";
  let rec aux (l:int list) =
    match l with
    | [] -> print_string "]"
    | t::[] -> (Printf.printf "%d]" t)
    | t::q -> (Printf.printf "%d; " t; aux q)
  in aux l

let print_int_array (t:int array) : unit =
  Printf.printf "[|";
  for i = 0 to Array.length t-1 do
    Printf.printf "%i" t.(i);
    if i <> Array.length t-1 then Printf.printf ";"
  done;
  Printf.printf "|]\n"

let get_heights len l =
  let heigths = Array.make (len+1) 0 in
  List.iteri (fun i x -> heigths.(i+1) <- heigths.(i)+x) l;
  (* if len > 1 then heigths.(0) <- heigths.(1); *)
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
        sauts old_pos_h (old_h)(*(new_h)*) t
  in
  List.fold_left max 0 (sauts 0 (List.hd l) l)

let exec_tests f =
  assert(f 4 [3; 2; -5; 4]            = 3);
  assert(f 7 [2; 9; 18; 12; 9; 19; 1] = 19);
  assert(f 6 [1; 6; -7;  9; 10; -15]  = 10);
  assert(f 5 [-3; -2; 3; -4; -5]      = 0);
  assert(f 4 [-3; 4; 0; 3]            = 4);
  assert(f 8 [3; -3; 9; -9; 64; -64; 8; -8] = 64)

let test n l f1 f2 =
  let v1 = f1 n l
  and v2 = f2 n l in
  Printf.printf "v1: %i | v2: %i -> n= %i, l= " v1 v2 n;
  print_int_list l;
  Printf.printf ", h= "; get_heights n l |> print_int_array

let _ =
  exec_tests solve1;
  (* exec_tests solve2; *)
  test 8 [3; -3; 9; -9; 64; -64; 8; -8] solve1 solve2;
  Random.self_init();
  for _ = 0 to 10 do
    let n = (Random.int 5) + 1 in
    let l = List.init n (fun _ -> (Random.int 10) - 5) in
    test n l solve1 solve2
  done
