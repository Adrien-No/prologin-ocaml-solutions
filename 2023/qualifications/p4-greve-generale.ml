type graphe = {
  sommets : int array;
  ladj : int list array;
}

let print_int_array t =
  let n = Array.length t in
  for i = 0 to n-1 do
    Printf.printf "%i" t.(i);
    if i < n-1 then Printf.printf " "
  done

let make_graphe t : graphe =
  let n = Array.length t in
  {
    sommets = Array.init n Fun.id ;
    ladj = Array.map (fun x -> [x]) t;
  }

let graphe_miroir g =
  let n = Array.length g.sommets in
  let ladj = Array.make n [] in
  Array.iteri (fun i l -> List.iter (fun j -> ladj.(j) <- i :: ladj.(j)) l) g.ladj;
  {
    sommets = g.sommets;
    ladj = ladj;
  }

let get_cycle t i : (int array)* (('a,'b) Hashtbl.t) =
  (* renvoie le cycle et le tableau des précédent des sommets du cycle s'ils existent *)
  let h = Hashtbl.create 1
  and h_prec = Hashtbl.create 1 in

  let rec build_chemin i acc =
    if not (Hashtbl.mem h i)
    then begin
      Hashtbl.add h i t.(i);
      Hashtbl.add h_prec t.(i) i;
      build_chemin t.(i) (i::acc)
    end
    else
      begin
      Hashtbl.add h_prec t.(i) i;
      i, List.rev (acc)
    end
  in
  let i_boucle, chemin = build_chemin i [] in

  let rec extract_boucle l i =
    match l with
      [] -> failwith "error, pas de boucle avec (l,i)"
    | t::q -> if t = i then (t::q) else extract_boucle q i
  in
  let boucle = extract_boucle chemin i_boucle in
  Array.of_list boucle, h_prec

let bfs g' prec r : (int*int) list =
    (* renvoie les couples (sommets, distance au cycle) pour une racine r*)
  let s = Stack.create () in
  Stack.push r s;
  let rec aux acc dist =
    if Stack.is_empty s then acc
    else begin
      let x = Stack.pop s in
      g'.ladj.(x) |> List.iter (fun y ->
          (* prec est toujours un Some car on fait les bfs sur les elements d'un cycle *)
          if Some y <> prec
          then Stack.push y s);
      aux ((x, dist)::acc) (dist+1)
    end
  in
  let res = aux [] 0 in
  res

let distances_au_cycle (g':graphe) h_prec cycle =
  (* on va calculer la dist_au_cycle de tout les fils de sommets du cycle *)
  cycle |> Array.fold_left (fun b x -> bfs g'
                               (if Hashtbl.mem h_prec x then Some (Hashtbl.find h_prec x) else None)
                          x
                          @ b) [] (* "@" améliorable -> pas tellement *)

let nb_redir_update nb_redir (l:(int*int) list) cycle =
  (* l est une liste de couple (sommets, distance au cycle) *)

  let taille_cycle = Array.length cycle in
  (* ce parcours inclu les sommets du cycle *)
  List.iter (fun (s, d) -> if nb_redir.(s) <> -1
              then Printf.printf "\n\n bizarre deja valeur\n\n";
              nb_redir.(s) <- d + taille_cycle ) l;
  nb_redir

let composante g' nb_redir redirs i : int array =
  let cycle, h_prec = get_cycle redirs i in
  let l = distances_au_cycle g' h_prec cycle in
  nb_redir_update nb_redir l cycle

let trajetsRetour n redirection =
  let redirections = Array.of_list redirection in
  let nb_redir = ref (Array.make (n) (-1))
  and g' = (redirections |> make_graphe |> graphe_miroir) in

  for i = 0 to n-1 do
    if !nb_redir.(i) = -1 then
      nb_redir := composante g' !nb_redir redirections i
  done;
  print_int_array !nb_redir

let () =
  let n = read_int () in
  let redirection = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev in

  trajetsRetour n
  (List.map (fun x -> ((+)(-1)) x) redirection) (* on décale car consigne *)
