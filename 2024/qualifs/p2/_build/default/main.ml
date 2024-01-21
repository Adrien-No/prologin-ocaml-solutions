(* fonctions assez clean mais pas de tests aux limites. *)

(* précalcul : on regarde la position de chaque personne *)
(* puis, pour chaque personne, on regarde s'il existe p tq k*p+pos = pos' *)

(* Remarques : *)
(* - on pourrait penser plus large, avec i' ≡ i [k*p] où p qqn, mais on va d'abord faire plus simple : i' = i + k*p avec -1 < i' < len *)
(* lemme : si on peut inverser les personnes de position i et (i+k), alors on peut inverser les positions i et i' = (i+k*p) pour p relatif (si i' reste dans les limites du tableau ? on boucle ?) *)
(*preuve : assez mécanique, *)

(* lemme : Si chaque personne peut-être placée individuellement, alors l'ensemble peut-être placé selon l'ordre (par taille).  *)

(* preuve: Par l'absurde. Soit p1 et p2 des personnes qui se mettent en conflit. *)
(* On note pos1 (reps pos1') et pos2 (resp pos2') les positions initiales et finales de p1 (resp p2). *)
(* En fait il ne peut même pas exister de conflit car on peut placer en un coup p1 et en un coup p2, *)
(* puisque pos1 <> pos2 et pos1' <> pos2' il n'y a pas de pb. *)
(* ( on compte en cycle, si l'on arrive à la fin de la liste on retourne au début ) *)

let print_int_array (t:int array) : unit =
  Printf.printf "[|";
  for i = 0 to Array.length t-1 do
    Printf.printf "%i" t.(i);
    if i <> Array.length t-1 then Printf.printf ";"
  done;
  Printf.printf "|]\n"


let print_int_int_array (t:(int * int) array) : unit =
  Printf.printf "[|";
  for i = 0 to Array.length t-1 do
    Printf.printf "(%i, %i) " (fst t.(i)) (snd t.(i));
    if i <> Array.length t-1 then Printf.printf ";"
  done;
  Printf.printf "|]\n"

let compare_fst x y = compare (fst x) (fst y)

(** [build_pos's l] renvoie un tableau [pos's] tq pos's.(i) = pos'(i) pour tout i element de l. *)
let build_pos's (l: int list) : 'a array =
  let len = List.length l in
  List.combine l (List.init len Fun.id) (* on va trier selon fst (la taille d'une personne) et garder la position initiale avec snd *)
  |> List.stable_sort compare_fst       (* trie *)
  |> List.map snd                       (* l'indice dans le tableau est associé au i une fois la personne triée. *)
  |> Array.of_list

(** [sortable t i i'] décide si un élement à la position i peut se retrouver à la position i' par comb.lin. de k, *)
(** cad il existe p entier tq i' = p*k + i *)
(* contraite : 0 <= i' <= N-1 *)
(* i' = p*k + i   <=> p = (i' - i) / k *)
(* 0 <= i' <= N-1 <=> -i / k <= (i'-i) / k <= (N-1-i) / k *)
let sortable n i i' k : bool = (* i = i' || *)
  let foi = float_of_int in
  let n, i, i', k = foi n, foi i, foi i', foi k in
  let p   = (i'-.i) /. k
  and binf= -.i /. k
  and bsup= (n-.1.-.i) /. k in
  binf <= p && p <= bsup            (* p est bien compris entre les deux bornes *)
  && (p |> int_of_float |> foi) = p (* p est entier *)

(* on vérifie que pour tout i entre 0 et N-1, sortable N i (l.(i)) k *)
let solve (k: int) (n: int) (l: int list) : string =
  let pos's = build_pos's l in
  if List.for_all (fun i -> sortable n i pos's.(i) k) (List.init n Fun.id)
  then "OUI"
  else "NON"

(* let _ = *)
(*   let k = read_int() *)
(*   and n = read_int() in *)
(*   let l = *)
(*     read_line() *)
(*     |> String.split_on_char ' ' *)
(*     |> List.map int_of_string *)
(*   in *)
(*   Printf.printf "%s" (solve k n l); *)

(*   (\* tests complets *\) *)
(*   (\*assert(solve )*\) *)
(*   (\*  *\) *)
(*   assert(sortable 3 1 1 3 = true); *)
(*   let t = [3; 2; 1; 5] in *)
(*   assert(build_pos's t =[|2;1;0;3|]); *)
(*   assert(sortable 4 0 2 2); *)
(*   assert(sortable 4 4 2 2); *)

(*   assert(not(sortable 4 0 2 3)); *)
(*   assert(   (sortable 4 5 3 2)); *)

(*   assert(sortable 7 0 6 2); *)
(*   assert(sortable 7 0 6 3); *)
(*   assert(not(sortable 7 1 7 3)) *)
