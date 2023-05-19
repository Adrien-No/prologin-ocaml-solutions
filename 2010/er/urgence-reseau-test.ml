(* Deux solutionns donnent pas le même résultat, on voudrait savoir pourquoi *)

(* ################ naive1 ################ *)

let naive n fibres =
  let can_be_took (choisis,max_y) fibre =
    (* on suppose choisis triée par décroissance selon les abscisses gauches, et que choisis est sans croisement. *)
    (* fibre possède une abscisse gauche supérieur(car on avait trié le tout), donc on regarde si son abscisse droite est supérieure à toutes les autres, cad >= à max_y*)
    snd fibre >= max_y
  in
  let rec aux_naive choisis max_y fibres_restantes =
    match fibres_restantes with
      [] -> List.length choisis
    | t::q ->
      if max_y <= snd t then (
        (* Printf.printf "%i >= %i\n" (snd t) max_y; *)
        max (aux_naive (t::choisis) (snd t) q) (aux_naive choisis max_y q)
      )
      else
        aux_naive choisis max_y q
  in
  aux_naive [] 0 fibres

(* ################ naive1 amélioré : marche ################ *)
let naive' n fibres =
  let can_be_took (choisis,max_y) fibre =
    (* on suppose choisis triée par décroissance selon les abscisses gauches, et que choisis est sans croisement. *)
    (* fibre possède une abscisse gauche supérieur(car on avait trié le tout), donc on regarde si son abscisse droite est supérieure à toutes les autres, cad >= à max_y*)
    snd fibre >= max_y
  in
  let rec aux_naive choisis max_y fibres_restantes =
    match fibres_restantes with
      [] -> List.length choisis
    | u_n'::q ->
      match choisis with
        [] -> max (aux_naive (u_n'::choisis) (snd u_n') q) (aux_naive choisis max_y q)
      | u_n::_ ->
        if snd u_n <= snd u_n' then (
          Printf.printf "%i <= %i\n" (snd u_n) (snd u_n');
          max (aux_naive (u_n'::choisis) (snd u_n') q) (aux_naive choisis max_y q)
        )
        else aux_naive choisis max_y q
  in
  aux_naive [] (fibres |> List.hd |> snd) fibres

(* ################ marche ################ *)
let test_croisements fibres =
  (* On veut qu'une fois avoir trié les X (gauche), les Y soient dans l'ordre croissant. *)
  (* Les X sont deja dans l'ordre décroissant. On verifie que les Y le sont aussi. *)
  let rec aux_test f last_y =
    match f with
      [] -> false
    | (_,y)::q -> not (y <= last_y) || (aux_test q y)
  in
  aux_test fibres max_int

let test_croisements2 fibres = match fibres with
    [] | [_] -> false
  | t1::t2::q -> snd t1 < snd t2

let naive2 n fibres =
  let meilleur = ref 0 in
  let rec aux_naive choisis fibres_restantes =
    if test_croisements2 choisis then () else
    match fibres_restantes with
      [] -> meilleur := max !meilleur (List.length choisis)
    | t::q ->
      max (aux_naive (t::choisis) q) (aux_naive choisis q)
  in
  aux_naive [] fibres;
  !meilleur

(* ###################################### *)

let _ =
  let n = read_int() in
  let fibres = List.init n (fun _ -> Scanf.scanf "%i %i " (fun x y -> (x,y))) in
  naive n (List.sort (fun x y -> compare (fst x) (fst y)) fibres)
  |> Printf.printf "%i"
