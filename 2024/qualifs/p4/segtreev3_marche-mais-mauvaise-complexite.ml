(* probleme, les arbres ne sont pas actualisés. *)
(* probleme, noeuds internes incohérents *)

(* Version où un arbre obtenu à partir d'un tableau est complet. *)
(* On peut perdre un peu d'optimisation (mais pas en complexité) et il faut faire attention lors de l'indicage, à ne pas compter les indices dépassant l'initial, *)
(* mais cela simplifie l'accès aux élements (situés aux racines) car les feuilles sont de même profondeur. *)
Printexc.record_backtrace true
open Printf
type tree = Nil | Node of tree*int*tree

(* ================================ OUTILS ================================ *)
let feuille x = Node(Nil, x, Nil)

let noeud t1 t2 =
  match t1, t2 with
    _ , Nil -> t1
  | Nil, _  -> t2
  | Node(_, x, _), Node(_, y, _) ->
    Node(t1, max x y, t2)

let sup_pow2 n =
  let rec aux acc =
    if acc < n then
      aux (acc*2)
    else acc
  in
  aux 1

(* pas obligé de faire un cache.. *)
let cachepow2 = Hashtbl.create 256
let rec pow2 n =
  if Hashtbl.mem cachepow2 n then Hashtbl.find cachepow2 n
  else match n with
      0 -> 1
    | _ -> let v = 2 * pow2 (n-1) in
      Hashtbl.add cachepow2 n v;
      v

let log2 x =
  let x' = float_of_int x in
  log x' /. log 2.
  |> int_of_float

let print_int_list l =
  let len = List.length l in
  List.iteri (fun i x -> Printf.printf "%i" x; if i <> len-1 then Printf.printf " ") l

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

let parcours tree n =
  (* renvoie la liste des feuilles dans l'ordre croissant *)
  let nb = ref 0 in
  let rec loop tree acc =
    if !nb = n then acc else
      match tree with
      | Nil -> failwith "parcours: bizarre"
      | Node(Nil, x, Nil) -> incr nb; x::acc
      | Node(l  , x,   r) ->
        let acc = loop l acc in
        loop r acc
  in
  List.rev (loop tree [])
(* ======================================================================== *)

(* ====================== RANGE QUERIES OPERATIONS ======================== *)
let tree_of_array a =

  (* on complète le tableau avec des "0" pour que sa longueur soit une puissance de 2 (et obtenir un arbre complet) *)
  let len_a = Array.length a in
  let a_pow2 = Array.make (sup_pow2 len_a) 0 in
  Array.blit a 0 a_pow2 0 len_a;

  let feuille_list = a_pow2 |> Array.to_list |> List.map (fun x -> feuille x, 0) in
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

let update_tree tree h i0 new_v =
  (* h est la hauteur de l'arbre *)
  (* i est la position de l'élement à modifié càd le (i+1)-ème *)
  (* l'arbre est supposé complet (cf tree_of_array) *)

  (* descendre à gauche où à droite selon i et (sup_pow2 n) *)
  (* on renvoie noeud (appel_rec) (autre_cote) *)
  (* erreur si à la fin on est pas sur le i-ème *)
  let rec loop tree h i =
    match tree with
      Nil -> printf "h= %i\n" h; failwith "update_tree: error not found"
    (* | Node(Nil, x, Nil) -> feuille new_v *)
    | Node(l, x, r) ->
      if h = 1 then begin (* Printf.printf "%i: %i <- %i\n" i0 x new_v; *) Node(l, new_v, r) end
      else
        if i < pow2 (h-2) then
          (* on descend à gauche, noeud permet aussi d'actualiser les noeuds internes. *)
          ((* Printf.printf "gauche i=%i pow=%i\n" i (pow2 (h-2)); *)
          noeud (loop l (h-1) i) r)
        else
          (* on descend à droite *)
          ((* Printf.printf "droite i=%i pow=%i\n" i (pow2 (h-2)); *)
          noeud l (loop r (h-1) (i-pow2(h-2))))
  in
  loop tree h i0

let get_max tree n a b =
  (* on suppose a <= b <= max_pow2 *)
  (* let affiche = ref false in *)
  (* if a = 18 then (print_newline(); affiche := true); *)

  let rec loop tree range i j =
    (* Printf.printf "range= %i, i= %i, j= %i\n" range i j; *)
    match tree with
      Nil -> 0
    (* | Node(Nil, x, Nil) -> if i = j then x else failwith "get_max: error" *)
    | Node(l, x, r) when i = 0 && j >= range-1 ->
      (* if !affiche then Printf.printf "%i\n" x *) x
    | Node(l, x, r) ->
      let mid = range/2 in
      (* seulement à gauche *)
      if i < mid && j < mid then
        loop l mid i j
        (* seulement à droite *)
      else if i >= mid && j >= mid then
        loop r mid (i-mid) (j-mid)
        (* un bout à gauche et un bout à droite ; on remarque que on itère sur des arbres de hauteur<, on retrouve la propriété "au + deux noeud par étage" *)
      else
        let max1 = loop l mid i mid in
        let max2 = loop r mid 0 (j-mid) in
        (* if !affiche then printf "max1= %i, max2= %i\n" max1 max2; *)
        max max1 max2
  in

  let res = loop tree n a b in
  (* if !affiche then print_newline(); *)
  res
(* ======================================================================== *)

(* ============================= MAIN  =================================== *)


let batiments n r k villes =
  (** TODO Afficher le nombre de bâtiments cassés à chaque ville après les $R$
      mouvements sous la forme d'une suite d'entiers séparés par des espaces.  *)

  let len = sup_pow2 n in (* longueur pratique de l'arbre *)
  let h = log2 len + 1 in

  let move tree i =
    (* simule un mouvement du serpent *)
    let get_max_cycle tree i =
      (* obtention du maximum *)
      if i + k -1 > n -1 then
        begin
          (* printf "divisé\n"; *)
          (* on divise la requête en deux plages *)
          (* entre i et n-1 *)
          let m1 = get_max tree len i (n-1)
          (* entre 0 et (r+i-(n-1)) TODO et pas i ! *)
          and m2 = get_max tree len 0 (k+i-n) -1) in
          max m1 m2
        end
      else
        begin
          (* printf "pas divisé\n"; *)
          (* une plage suffit *)
          get_max tree len i (i+k-1)
        end
    in
    let new_v = get_max_cycle tree i in
    (* on renvoie l'arbre avec la valeur modifiée *)
    update_tree tree h i new_v
  in
  let rec loop tree i cpt =
    (* renvoie une arbre représentant les batiments après r mouvements *)
    if cpt = r then tree
    else
      loop (move tree i) ((i+1) mod n) (cpt+1)
  in

  (* ================ coeur du programme ================ *)
  let tree0 = tree_of_array villes in
  let tree = loop tree0 0 0 in
  let batiments = parcours tree n in
  (* print_tree tree; *)
  batiments
(* ==================================================== *)

(* ======================================================================== *)


let tests() =
  assert(batiments 5 2 3 [|2; 4; 3; 6; 8|] = [4; 6; 3; 6; 8]);
  assert(batiments 5 2 4 [|2; 4; 3; 6; 8|] = [6; 8; 3; 6; 8]);
  assert(batiments 5 6 3 [|5; 4; 3; 2; 1|] = [5; 4; 3; 5; 5]);
  assert(batiments 10 6 3 (Array.init 10 ((+)1)) = [3; 4; 5; 6; 7; 8; 7; 8; 9; 10]);
  assert(batiments 20 42 3 (Array.init 20 (function x when x < 10 -> x+1 | x -> 20-x)) = [7; 8; 7; 8; 9; 10; 10; 10; 10; 10; 10; 9; 8; 7; 6; 5; 4; 4; 5; 6])

let _ =
  tests();
  batiments 50000 5000000 50000 (Array.init 50000 Fun.id)
  (* let n = read_int () in *)
  (* let r = read_int () in *)
  (* let k = read_int () in *)
  (* let villes = (read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev) |> Array.of_list in *)
  (* batiments n r k villes |> print_int_list *)

(* expected for test 5 :
0 1 2 3 4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19
7 8 7 8 9 10 10 10 10 10 10  9  8  7  6  5  4  4  5  6
obtenu:
7 8 7 8 9 10 10 10 10 10 10  9  8  7  6  5 10 10 10 10
  *)
