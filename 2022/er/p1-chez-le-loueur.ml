type vehicule = {
  m : int; (** le numéro de série du véhicule *)
  p : int; (** le prix initial *)
  r : int; (** la taille du réservoir *)
  c : int; (** le coût au kilomètre *)
}

(*
   @param n le nombre de véhicules disponibles
   @param d la distance à parcourir
   @param v la liste des véhicules
*)
let rent n d v =
  (* TODO Le numéro de modèle du véhicule revenant le moins cher ou $-1$ si
  aucun véhicule n'est disponible (la liste est vide ou aucun véhicule ne
  possède un réservoir suffisamment grand). Si deux modèles sont équivalents,
  le véhicule retenu sera celui-ci ayant été considéré en premier dans l'ordre
  de la liste. *)

  let f b x =
    if x.r >= d then
      match b with
        Some y -> Some (if (y.p+y.c*d) < (x.p+x.c*d) then y else x)
      | None -> Some x
    else b
  in
  match List.fold_left f None v with
    Some x -> Printf.printf "%i" x.m
  | None -> Printf.printf "-1"

let () =
  let n = read_int () in
  let d = read_int () in
  let v = List.init n (fun _ -> Scanf.sscanf (read_line ()) "%d %d %d %d" (fun m p r c -> {m; p; r; c})) in
  rent n d v
