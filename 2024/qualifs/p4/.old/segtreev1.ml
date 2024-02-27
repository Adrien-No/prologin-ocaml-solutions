(* ça pourrait être intéressant de refaire avec l'équilibrage des foncteurs *)

type tree = Nil | Node of tree*int*tree

let feuille x = Node(Nil, x, Nil)

let noeud t1 t2 =
  match t1, t2 with
    _ , Nil -> t1
  | Nil, _  -> t2
  | Node(_, x, _), Node(_, y, _) ->
    Node(t1, max x y, t2)

let tree_of_array a = (* manière détournée pour éviter d'utiliser dans notre cas les arbres binaires équilibrés *)
  let feuille_list = a |> Array.to_list |> List.map (fun x -> feuille x, 0) in
  let rec concat_feuilles awaited_h l =
    (* awaited_h is the height of the tree expected at the top of the list ; once we rushed it we returns. *)
    match l with
      [] -> Nil, []
    | [(t, _)] -> t, []
    | (t1, h1)::(t2, h2)::tl ->
      if awaited_h = h1 then t1, (t2, h2)::tl

      else if h1 = h2 then
        concat_feuilles awaited_h ((noeud t1 t2, h1+1)::tl)
      else (* h1 > h2 *)
        (* we wants a tree sized of h2 (or less if can't do more) to concat with t2 and be closer the height of t1 *)
        let t3, tl = concat_feuilles h2 tl in
        let new_t2 = noeud t2 t3
        and new_h2 = h2+1 in
        (* toujours un parcours en O(|a|) car on reprend dans tl là où on s'était arrêté pour calculer t3 *)
        match tl with
          [] -> noeud t1 new_t2, []
        | _  -> concat_feuilles awaited_h ((t1, h1)::(new_t2, new_h2)::tl)
  in
  fst (concat_feuilles max_int feuille_list)

let update_tree tree i new_v =
  (* i est la position de l'élement à modifié càd le (i+1)-ème *)
  (* descendre à gauche où à droite selon i et (sup_pow2 n) *)
  (* on renvoie noeud (appel_rec) (autre_cote) *)
  (* erreur si à la fin on est pas sur le i-ème *)
  let rec loop tree i =
    match tree with
      Nil -> failwith "update_tree: error not found"
    | Node(Nil, x, Nil) ->
    | Node(l, x, r) ->


let print_tree t =
  let rec loop indent t =
    match t with
      Nil -> ()
    | Node(l, x, r) ->
      loop (indent+2) l;
      print_string (String.make (indent+1) ' ' ^ "/ \n");
      print_string (String.make indent ' '); Printf.printf "%i\n " x;
      print_string (String.make (indent) ' ' ^ "\\\n");
      loop (indent+2) r
  in
  loop 0 t

let tests() =
  (* tree_of_array *)
  assert(tree_of_array [||] = Nil);
  assert(tree_of_array [|0; 1|] = Node(feuille 0, 1, feuille 1));
  (* print_tree (tree_of_array [|0; 1; 2; 3; 4; 5; 6; 7|]); *)
  assert(tree_of_array [|0; 1; 2; 3; 4; 5; 6; 7|] =
         noeud
         (
           noeud
           (noeud (feuille 0) (feuille 1))
           (noeud (feuille 2) (feuille 3))
         )
         (
           noeud
           (noeud (feuille 4) (feuille 5))
           (noeud (feuille 6) (feuille 7))
         )
        )
  (* print_tree (tree_of_array (Array.init 5 Fun.id)) *)

let _ =
  tests()
