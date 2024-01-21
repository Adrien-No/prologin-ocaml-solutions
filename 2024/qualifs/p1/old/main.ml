(* let solve len l = *)
(*   (\** construction de la liste des hauteurs *\) *)
(*   let rec get_lh acc old_h = function *)
(*     | []   -> List.rev acc *)
(*     | diff::t -> let new_h = old_h+diff in *)
(*       get_lh (new_h::acc) new_h t *)

(*   (\** construit une suite extraite strictement croissante *\) *)
(*   and purge acc maxh = function *)
(*     | [] -> List.rev acc *)
(*     | h::t -> if h > maxh then purge (h::acc) h t *)
(*           else purge acc maxh t *)

(*   (\** construit la liste des sauts en terme de hauteur *\) *)
(*   and sauts acc h = function *)
(*     | [] -> List.rev acc *)
(*     | h'::t -> sauts (h'-h::acc) h' t *)
(*   in *)
(*      get_lh [] 0 l *)
(*   |> purge  [] 0 *)
(*   |> sauts  [] 0 *)
(*   |> List.fold_left max 0 *)

let find_index p =
  let rec aux i = function
    [] -> None
    | a::l -> if p a then Some i else aux (i+1) l in
  aux 0

let solve len l =

  let rec get_lh acc old_h = function
    | []   -> List.rev acc
    | diff::t -> let new_h = old_h+diff in
      get_lh (new_h::acc) new_h t
  in
  let l0 = get_lh [] 0 l in
  let i_hmax = find_index ((=)(List.fold_left max 0 l0)) l0 |> Option.get in

  let rec list_cut i acc = function
    | [] -> List.rev acc
    | h::t -> if i < 0 then List.rev acc else list_cut (i-1) (h::acc) t
  in
  let l = list_cut i_hmax [] l in

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
  let len = read_int() - 1 in
  String.split_on_char ' ' (read_line())
  |> List.map int_of_string
  |> solve len
  |> Printf.printf "%i"
