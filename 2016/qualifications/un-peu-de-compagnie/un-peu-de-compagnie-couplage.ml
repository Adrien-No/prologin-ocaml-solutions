
type tree = Nil | Node of int * tree list

let is_externe (t:tree) x =
  (* optimisable avec un arbre trié *)
  let rec aux (t:tree) (n:int) : bool =
    match t with
      Nil -> failwith "sommet non trouvé"
    | Node(v,tree_list) ->
      match tree_list with
        [] -> failwith "sommet non trouvé"
    | t::q -> if x = v then n mod 2 = 0 else List.exists Fun.id (List.map (fun t -> aux t (n+1)) q)
          in
  aux t 0

let contruction_chemin (g:int list array) (m : (int*int) list) =
  (* g : graphe représenté par une ladj *)
  (* m : couplage (liste d'arêtes) *)
  let n = Array.length g in

  let foret = ref [] in
  let mark_vertex = Array.make n false
  and mark_edges = Hashtbl.create (n*(n-1)/2) in List.iter (fun x -> Hashtbl.add mark_edges x true) m;

  let couverture = Array.make n false in List.iter (fun (a,b) -> couverture.(a) <- true; couverture.(b) <- true) m;
  List.iter (fun x -> foret := Node (x,[]):: !foret) (List.filter (fun x -> not couverture.(x)) (List.init n Fun.id));

  while List.exists (fun x -> not mark_vertex.(x) && is_externe)
