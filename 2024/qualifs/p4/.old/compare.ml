Printexc.record_backtrace true

let print_int_array (t:int array) : unit =
  Printf.printf "[|";
  for i = 0 to Array.length t-1 do
    Printf.printf "%i" t.(i);
    if i <> Array.length t-1 then Printf.printf ";"
  done;
  Printf.printf "|]\n"


(* ######################################################################################### *)
(* ######################################################################################### *)
(* ###################################### SPARSE TABLE ##################################### *)
(* ######################################################################################### *)
(* ######################################################################################### *)

open Printf

let print_int_array (t:int array) : unit =
  Printf.printf "[|";
  for i = 0 to Array.length t-1 do
    Printf.printf "%i" t.(i);
    if i <> Array.length t-1 then Printf.printf ";"
  done;
  Printf.printf "|]\n"

let print_iaa t =
  Array.iteri (fun i t ->
      Array.iteri (fun j x ->
          Printf.printf "%i" x;
          if j <> Array.length t -1 then
            Printf.printf " "
        ) t;
      if i  <> Array.length t -1 then
            Printf.printf "\n"
    ) t

(* ================================ SPARSE TABLE ================================ *)
let print_int_list l =
  let len = List.length l in
  List.iteri (fun i x -> Printf.printf "%i" x; if i <> len-1 then Printf.printf " ") l

let print_hashtbl h =
  printf "=== table de hachage de taille %i ===\n" (Hashtbl.length h);
  Hashtbl.iter (fun (a,b) v -> printf "(%i, %i) -> %i\n" a b v) h

let max_pow_2 n =
  let rec aux acc n =
    if acc*2 <= n then aux (acc*2) n
    else acc
  in
  aux 1 n

let max_bet_naif t a b =
  let res = ref t.(a) in
  for i = a to b do
    res := max !res t.(i)
  done;
  !res

let log2 x = int_of_float (log (float_of_int x) /. log 2.0)

let precompute t n =
  let n = Array.length t in
  let m = log2 n + 1 in
  let t' = Array.make_matrix n m 0 in
  (* printf "n= %i, m= %i\n" n m; *)
  assert (Array.length t = n);
  for i = 0 to n-1 do
    t'.(i).(0) <- t.(i)
  done;
  for j = 1 to m-1 do
    for i = 0 to n - (1 lsl j)  (* -2 *) do
      (* printf "i= %i, j= %i\n" i j; *)
      t'.(i).(j) <- max t'.(i).(j-1) t'.(i+(1 lsl (j-1))).(j-1)
      (* t'.(i).(j) <- max *)
      (*     t'.(i).(max (i+w-1) 0) *)
      (*     t'.(i+w).(j) *)
    done
  done;
  t'

let max_bet_opti0 t a b =
  (* a <= b *)
  (* printf "max_bet_opti: (%i, %i)\n%!" a b; *)
  let k = max_pow_2 (b-a+1) in
  let max1 = t.(a).(a+k-1)
  and max2 = t.(b-k+1).(b) in
  let res = max max1 max2 in
  (* printf "res: %i\n%!" res; *)
  res

let max_bet_opti t a b =
  (* let k = max_pow_2 (b-a+1) in *)
  (* let max1 = t.(a).(log2 (a+k-1)) *)
  (* and max2 = t.(b-k+1).(log2 b) in *)
  (* max max1 max2 *)
  (* printf "a= %i, b= %i\n" a b; *)
  let k = log2 (b - a + 1) in
  max t.(a).(k) t.(b- (1 lsl k) +1).(k)

let get_max h n a b =
  (* printf "a= %i, b= %i\n" a b; *)
  if a > b then
    begin
      (* on divise la requête en deux plages *)
      (* entre b et n-1 *)
      (* printf "divise\n"; *)
      let m1 = max_bet_opti h a (n-1) in
      (* entre 0 et (k+a-(n-1)) *)
      let m2 = max_bet_opti h 0 b in
      max m1 m2
    end
  else
    begin
      (* printf "divise pas\n"; *)
      max_bet_opti h a b
    end

(* ================================================================ *)

let passages n r =
  (* renvoie le tableau du nombre de passages du serpent dans chaque ville *)
  (* on fait attention à ne pas faire un algo en O(r) car r est grand (comme Quentin) *)
  let tours_complets = r / n in
  let nb_max_last_turn = r - n * tours_complets in
  Array.init n (fun i -> if i < nb_max_last_turn then tours_complets + 1 else tours_complets)

let solve n r k villes =
  let t = precompute villes n in

  (* print_iaa t; *)
  let passages = passages n r in

  (* printf "passages :\n"; *)
  (* print_int_array passages; *)
  (* test *)
  (* for i = n-1 downto n-k do *)
  (*   passages.(i) <- passages.(i) + 1 *)
  (* done; *)
  let batiments i =
    (* print_newline(); *)
    let k' = ((k-1) * (passages.(i))) in (* k > 0, k' >= 0 *)
    (* printf "k'= %i\n" k'; *)
    if k' >= n-1 then
      max_bet_opti t 0 (n-1)
    else if i + k' >= n then
      let k' = k' + (k-1) in (* le dépassement du cycle des villes nous fait gagner de la distance *)
      if k' >= n-1 then max_bet_opti t 0 (n-1) else
        begin
          assert(k'<n); (* on pense que si k'>= n alors on aura été dans la branche précédente du if *)
          let m1 = max_bet_opti t i (n-1) in
          max m1 (max_bet_opti t 0 (k'-(n-i)))
        end
    else
      max_bet_opti t i (i+k')
      (* if (i + k') >= n then *)
      (*   begin *)
      (*     let d = n-1-i in *)
      (*     i+k'+(k-1) - d *)
      (*   (\* (((i+k') mod n) + (k-1)) *\) *)
      (*   end *)
      (* else *)
      (*   i+k' *)
  in

  List.init n batiments

(* ######################################################################################### *)
(* ######################################################################################### *)
(* ###################################### SEGMENT TREE ##################################### *)
(* ######################################################################################### *)
(* ######################################################################################### *)

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

let maxpow2 = ref 0
let update_mp2 n =
  if n > !maxpow2 then( maxpow2 := n;
  (* printf "pow2: appel pour n = %i\n" n *))

let update_tree tree pow2 h i0 new_v =
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

        if update_mp2 (h-2); i < pow2.(h-2) then
          (* on descend à gauche, noeud permet aussi d'actualiser les noeuds internes. *)
          ((* Printf.printf "gauche i=%i pow=%i\n" i (pow2 (h-2)); *)
          noeud (loop l (h-1) i) r)
        else
          (* on descend à droite *)
          ((* Printf.printf "droite i=%i pow=%i\n" i (pow2 (h-2)); *)
            update_mp2 (h-2);
          noeud l (loop r (h-1) (i-pow2.(h-2))))
  in
  loop tree h i0

let count_getmax_loop = ref 0
let update_tic_getmax_loop() =
  incr count_getmax_loop;
  if !count_getmax_loop mod 10000000 = 0 then begin printf "getmax_loop: %i calls\n%!" !count_getmax_loop end

let get_max tree temp_max n a b =
  (* on suppose a <= b <= max_pow2 *)
  (* let affiche = ref false in *)
  (* if a = 18 then (print_newline(); affiche := true); *)

  let rec loop tree temp_max2 range i j =
    (* update_tic_getmax_loop(); *)
    (* Printf.printf "range= %i, i= %i, j= %i\n" range i j; *)
    match tree with
      Nil -> 0
    (* | Node(Nil, x, Nil) -> if i = j then x else failwith "get_max: error" *)
    (* TODO opti *)
    | Node(_, x, _) when x <= temp_max2 -> 0
    | Node(l, x, r) when i = 0 && j >= range-1 ->
      (* if !affiche then Printf.printf "%i\n" x *) x
    | Node(l, x, r) ->
      let mid = range/2 in
      (* seulement à gauche *)
      if i < mid && j < mid then
        loop l temp_max2 mid i j
        (* seulement à droite *)
      else if i >= mid && j >= mid then
        loop r temp_max2 mid (i-mid) (j-mid)
        (* un bout à gauche et un bout à droite ; on remarque que on itère sur des arbres de hauteur<, on retrouve la propriété "au + deux noeud par étage" *)
      else
        let max1 = loop l temp_max2 mid i mid in
        let max2 = loop r (max max1 temp_max2) mid 0 (j-mid) in
        (* if !affiche then printf "max1= %i, max2= %i\n" max1 max2; *)
        max max1 max2
  in

  let res = loop tree temp_max n a b in
  (* if !affiche then print_newline(); *)
  res
(* ======================================================================== *)

(* ============================= MAIN  =================================== *)

let count_move = ref 0
let update_tic_move() =
  incr count_move;
  if !count_move mod 1000000 = 0 then printf "move: %i calls\n%!" !count_move

let solve2 n r k villes =
  (** TODO Afficher le nombre de bâtiments cassés à chaque ville après les $R$
      mouvements sous la forme d'une suite d'entiers séparés par des espaces.  *)

  let len = sup_pow2 n in (* longueur pratique de l'arbre *)
  let h = log2 len + 1 in
  (* printf "hauteur: %i\n%!" h; *)

  let temp = ref 1 in
  let pow2 = Array.init (h-1) (fun _ -> let v = !temp in temp := !temp * 2; v) in

  let move tree i =
    (* update_tic_move(); *)
    (* simule un mouvement du serpent *)
    let get_max_cycle tree i =
      (* obtention du maximum *)
      if i + k -1 > n -1 then
        begin
          (* printf "divisé\n"; *)
          (* on divise la requête en deux plages *)
          (* entre i et n-1 *)
          let m1 = get_max tree 0 len i (n-1) in
          (* entre 0 et (r+i-(n-1)) TODO et pas i ! *)
          let m2 = get_max tree m1 len 0 (k+i-(n) -1) in
          max m1 m2
        end
      else
        begin
          (* printf "pas divisé\n"; *)
          (* une plage suffit *)
          get_max tree 0 len i (i+k-1)
        end
    in
    let new_v = get_max_cycle tree i in
    (* on renvoie l'arbre avec la valeur modifiée *)
    update_tree tree pow2 h i new_v
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

(* ######################################################################################### *)
(* ######################################################################################### *)
(* ######################################### COMPARE ####################################### *)
(* ######################################################################################### *)
(* ######################################################################################### *)

let test() =
  let n = Random.int 100000 |> Int.succ
  and r = Random.int 1000 |> Int.succ
  and k = Random.int 1000 |> Int.succ in
  let villes = Array.init n (fun _ -> Random.int 10) in

  let res1 = solve  n r k villes
  and res2 = solve2 n r k villes in
  if res1 <> res2 then
    begin
      printf "Entrée : \nn= %i\nr= %i\nk= %i\n" n r k;
      print_int_array villes;
      print_int_list res1;
      print_newline();
      print_int_list res2;
      print_newline()
    end

  (* with Not_found -> *)
  (*   begin *)
  (*     printf "###ERROR###\n"; *)
  (*     printf "Entrée : \nn= %i\nr= %i\nk= %i\n" n r k; *)
  (*     print_int_array villes; *)
  (*     print_newline(); *)
  (*     print_newline() *)
  (*   end *)
let _ =
  for _ = 0 to 10 do
    test()
  done
