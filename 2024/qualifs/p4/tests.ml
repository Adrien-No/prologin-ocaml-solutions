(* ============================= TESTS ==================================== *)
let tests() =
  (* ================ tree_of_array ================ *)
  assert(tree_of_array [||] = feuille 0);
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
        );

  (* ================ update_tree ================ *)
  let t = tree_of_array [|0; 1; 2; 3; 4; 5; 6|] in
  let h = 4 in
  let i = 4 in
  let new_v = 10 in

  let t' = update_tree t h i new_v in
  (* print_tree t'; *)
  assert(t' =
         noeud
         (
           noeud
           (noeud (feuille 0) (feuille 1))
           (noeud (feuille 2) (feuille 3))
         )
         (
           noeud
           (noeud (feuille 10) (feuille 5))
           (noeud (feuille 6) (feuille 0)) (* 0 car l'arbre est complété avec cette valeur (cf tree_of_array)*)
         )
        );

  (* ================= get_max ================== *)
  (* assert(get_max t' 8 0 7 = 10); *)
  (* assert(get_max t' 8 2 3 = 3); *)
  let t'' = tree_of_array (Array.init 16 Fun.id) in
  (* assert (get_max t'' 16 4 11 = 11); *)
  let tree = tree_of_array [|2; 4; 3; 6; 4|] in
  (* assert(parcours tree 8 = parcours tree 9); *)
  assert(get_max tree 8 0 2 = 4);
  (* assert(get_max tree 8 0 4 = 4); *)
  (* print_tree (tree_of_array (Array.init 5 Fun.id)) *)
  (* ================ parcours ================== *)
  assert(parcours t 7 = [0; 1; 2; 3; 4; 5; 6]);
  assert(parcours t 4 = [0; 1; 2; 3]);
  assert(parcours t' 8 = List.init 8 (function 4 -> 10 | 7 -> 0 | i -> i ));
  assert(parcours t'' 12 = List.init 12 Fun.id)

(* ======================================================================== *)
