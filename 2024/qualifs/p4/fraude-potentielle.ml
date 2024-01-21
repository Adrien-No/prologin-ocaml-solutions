(* marche pas en vrai *)

Printexc.record_backtrace true

(* on va essayer de preshot comme pour sparse table (eliminer le facteur R dans la complexité), *)
(* mais on va brutalement calculer le max en parcourant toute la plage. *)
(* cependant, on va faire qlqs optis comme : *)
(* - comparer le max entre i et i+1 *)


(* PB: il faut faire deux tableaux .. (voire +), *)
(* pour conserver les anciennes valeurs quand on dépasse n *)

let print_int_list l =
  let len = List.length l in
  List.iteri (fun i x -> Printf.printf "%i" x; if i <> len-1 then Printf.printf " ") l

let print_int_array l =
  let len = Array.length l in
  Array.iteri (fun i x -> Printf.printf "%i" x; if i <> len-1 then Printf.printf " ") l

let passages n r =
  (* renvoie le tableau du nombre de passages du serpent dans chaque ville *)
  (* on fait attention à ne pas faire un algo en O(r) car r est grand (comme Quentin) *)
  let tours_complets = r / n in
  let nb_max_last_turn = r - n * tours_complets in
  Array.init n (fun i -> if i < nb_max_last_turn then tours_complets + 1 else tours_complets)

let max_ab t i j =
  (* i <= j < len t*)
  assert(i <= j);
  assert(j < Array.length t);
  let rec aux i acc =
    if i = j then max acc t.(i)
    else aux (i+1) (max t.(i) acc)
  in
  aux i 0

let get_max t n a b =
  if a > b then
    let m1 = max_ab t a (n-1)
    and m2 = max_ab t 0 b in
    max m1 m2
  else
    max_ab t a b

let solve n r k villes =
  (** TODO Afficher le nombre de bâtiments cassés à chaque ville après les $R$
  mouvements sous la forme d'une suite d'entiers séparés par des espaces.  *)
  let passages = passages n r in (* Nombre de passages du serpent dans chaque ville.  *)

  let villes_old = Array.copy villes in

  let rec batiments old_max i =
    if i = n then () else

      let range_old = if passages.(i) <= 1 then 0 else (k-1) * (passages.(i) - 1) in
      let range = (k-1) * passages.(i) in

      let get_new_max range =
        (*opti *)
        let j = i+range in

        if i > 0 && (villes.(i-1) < old_max) then
          if j < n then
            max old_max villes.(j)
          else
            max old_max villes.(j mod n)
        else
          if j < n then
            get_max villes n i (j)
          else
            get_max villes_old n i (j mod n)
      in

      let new_max_old = get_new_max range_old
      and new_max = get_new_max range in
      villes_old.(i) <- new_max_old;
      villes.(i) <- new_max;
      batiments new_max (i+1)
  in
  batiments 0 0;
  villes |> Array.to_list

let tests () =
  assert(solve 5 2 3 [|2; 4; 3; 6; 8|] = [4; 6; 3; 6; 8]);
  assert(solve 5 2 4 [|2; 4; 3; 6; 8|] = [6; 8; 3; 6; 8]);
  assert(solve 5 6 3 [|5; 4; 3; 2; 1|] = [5; 4; 3; 5; 5]);
  (* assert(solve 10 6 3 (Array.init 10 ((+)1)) = [3; 4; 5; 6; 7; 8; 7; 8; 9; 10]); *)
  (* solve 20 42 3 (Array.init 20 (function x when x < 10 -> x+1 | x -> 20-x)) |> print_int_list *)
  let res = solve 20 42 3 (Array.init 20 (function x when x < 10 -> x+1 | x -> 20-x)) in
  print_int_list res; print_newline();
  assert(res = [7; 8; 7; 8; 9; 10; 10; 10; 10; 10; 10; 9; 8; 7; 6; 5; 4; 4; 5; 6]);
  let res = solve  3  3 3 [|5; 1; 1|] in
  print_int_list res;
  assert(res = [5; 5; 5]);
  let res = solve 4 7 7 [|6; 1; 5; 1|] in
  print_int_list res;
  assert(res = [6; 6; 6; 6]);
  let res = solve 2 5 9 [|3; 4|] in
  print_int_list res;
  assert(res = [4; 4]);
  let res = solve 3 8 2 [|1; 4; 0|] in
  print_int_list res;
  assert(res = [4; 4; 4]);
  let res = solve 3 5 2 [|2; 9; 2|] in
  print_int_list res;
  assert(res = [9; 9; 9]);
  let res = solve 4 4 3 [|9; 6; 9; 2|] in
  print_int_list res;
  assert(res = [9; 9; 9; 9])

let _ =
  tests()
  (* let n = read_int () in *)
  (* let r = read_int () in *)
  (* let k = min (read_int ()) n in *)
  (* let villes = (read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.map int_of_string) |> Array.of_list in *)
  (* solve n r k villes |> print_int_array *)
