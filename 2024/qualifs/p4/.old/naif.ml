(* -> regarder les ranges queries pour le max entre deux indices. *)


let print_int_array t =
  Array.iteri (fun i x -> Printf.printf "%i" x; if i <> Array.length t-1 then Printf.printf " ") t

let rec moves n r k villes acc =
  (* acc va varier de 0 à r *)
  if acc = r then villes
  else
    begin
      let new_count = ref villes.(acc mod n) in
      for i = 0 to k-1 do
        new_count := max villes.((acc+i) mod n) !new_count
      done;
      villes.(acc mod n) <- !new_count;
      moves n r k villes (acc+1)
    end

let batiments n r k villes =
  (** TODO Afficher le nombre de bâtiments cassés à chaque ville après les $R$
  mouvements sous la forme d'une suite d'entiers séparés par des espaces.  *)
  moves n r k villes 0
  |> print_int_array

let () =
  let n = read_int () in
  let r = read_int () in
  let k = read_int () in
  let villes = (read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev) |> Array.of_list in
  batiments n r k villes
