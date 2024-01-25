(* regroupe *)
  let regroupe mots : (char * char list list) list =
    (* il faut mémoriser si un mot s'arrête sur ce noeud, càd un des mots est nul *)
    let groupes, dernier_groupe = List.fold_left (fun (acc, temp_acc) mot ->
      match mot, temp_acc with
      | [], _ -> (acc, None)
      | c'::t, Some (c, suites) -> if c = c' then (acc, Some (c, t::suites)) else (c, suites)::acc, Some (c', [t])
      | c'::t, None -> (acc, Some (c', [t]))
    )
    (List.hd mots |> function [] -> [] | c::t -> c , []) (* nous permet d'obtenir le groupe ([], []) ssi il y a un mot vide (car en vertue du `sort compare` il sera en premier)*)
    mots
    in
