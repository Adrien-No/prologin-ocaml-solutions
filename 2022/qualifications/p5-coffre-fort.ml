(*
* PRINCIPE DU PROGRAMME
-----------------------

On veut, pour chaque question, renvoyer le produit des signaux des puces activées modulo 1671404011.

-) obtenir les puces activées (chip enabled) entre a et b
   -> création d'un arbre (get_tree(), renvoyant un tableau de noeuds, c.f type node)
      On peut ainsi accèder à la racine (définissable via get_root()) très rapidement (unicité du parent d'un noeud) par la fonction path_to_root()
   -> calcul du plus récent ancetre commun (nearest_ancestor(), dont il y a existence et unicité)
      On a donc un chemin pour aller de a à b, et c'est le seul d'après l'énoncé.
      Ce chemin constitue l'ensemble des puces activées.

-) Calculer le produit modulo 1671404011
  on a les puces activées, il nous suffit d'effectuer le produit de leurs signaux. (produit_signaux_iter)
*)

(* Emulate List.init from OCaml 4.06 *)
module List = struct
  include List

  let init n f =
    let rec aux i =
      if i >= n then [] else
        let r = f i in
        r :: aux (i+1) in
    aux 0
end

(* Copy String.split_on_char from OCaml 4.04 *)
module String = struct
  include String

  let split_on_char sep s =
    let r = ref [] in
    let j = ref (String.length s) in
    for i = String.length s - 1 downto 0 do
      if String.unsafe_get s i = sep then begin
        r := String.sub s (i + 1) (!j - i - 1) :: !r;
        j := i
      end
    done;
    String.sub s 0 !j :: !r
end

(** Fil reliant deux puces *)
type fil = {
  puce1 : int; (** première extrémité du fil *)
  puce2 : int; (** seconde extrémité du fil *)
}

(** Question posée par Joseph *)
type question = {
  puceA : int; (** première extrémité alimentée *)
  puceB : int; (** seconde extrémité alimentée *)
}

(** Noeud représentant une puce*)
type node = {
  parent : int option;
  fils : int option list; (** "fisses" dans le sens enfant *)
}
(**
   @param nb_node nombre de puces
   @param m nombre de fils
   @param q nombre de questions
   @param signaux liste des signaux
   @param fils liste des fils entre les puces
   @param questions liste des questions
*)

(* ----- Outils ----- *)

let rec list_to_nth (l:int list) (n:int) : int list=
  (* renvoie la liste composée de la tête à l'élement n exclu *)
  match l with
  | t::q -> if t = n then [] else t::list_to_nth q n
  | _ -> failwith "n-1 > List.length l"

let array_from_list (l:'a list) =
  (* "convert" a list to array*)
  let len_l = List.length l in
  let a = Array.make len_l (if len_l = 0 then 0 else List.hd l) in
  let rec set_values (l:'a list) (i_array:int)=
    match l with
    | t::q -> a.(i_array) <- t ; set_values q (i_array+1)
    | _ -> ()
  in
  set_values l 0;
  a
(* ------------------ *)

let get_tree (n:int) (fils:fil list) : node array =
  (* renvoie un tableau de n node représentant un arbre
   acces au parent : a.(noeud).(parent)
   acces aux fils : a.(noeud).(fils) *)
  let a = Array.make n {parent=None;fils = [None]} in
  let rec set_value (a:node array) (fils:fil list) =
    match fils with
    | t::q ->
      let t = if a.(t.puce2).parent = None then t else {puce1=t.puce2;puce2=t.puce1} in (* Si le noeud a déjà un parent, il ne peut pas en avoir un autre *)
      a.(t.puce1) <- {parent = a.(t.puce1).parent ; fils = Some t.puce2::a.(t.puce1).fils} ; (* On ajoute puce2 aux enfants de puce1 *)
      a.(t.puce2) <- {parent = Some t.puce1 ; fils = a.(t.puce2).fils}; (* On ajoute puce1 comme parent (unique) de puce2 *)
      set_value a q
    | [] -> ()
  in
  set_value a fils;
  a

let rec path_to_root (t:node array) (r:int) (a:int) : int list=
  (* liste représentant le chemin à parcourir pour atteindre la racine r en partant du noeud a*)
  if a = r then [r]
  else a::path_to_root t r (Option.get t.(a).parent)

let nearest_ancestor (path1:int list) (path2:int list) =
  (* la structure de donnée étant un arbre, a et b ont au moins un ancetre commun : la racine *)
  let rev1 = List.rev path1
  and rev2 = List.rev path2 in
  let rec while_same (l1:int list) (l2:int list) : int=
    match l1,l2 with
    | t1::q1::q11, t2::q2::q22 ->
      if t1 = t2 then
        if q1 = q2 then while_same (q1::q11) (q2::q22)
        else t1
      else failwith "pas d'ancetre commun"
    | t1::q1, t2::q2 ->
      if t1 = t2 then t1
      else failwith "pas d'ancetre commun"
    | _ -> failwith "pas d'ancetre commun"
  in
  while_same rev1 rev2

let get_root (t:node array) (nb_node:int) : 'a option=
  (* return the first "None" parent, he should exist and be unique *)
  let i_racine = ref (None) in
  for i = 0 to nb_node-1 do
    if t.(i).parent = None then i_racine := Some i
  done;
  !i_racine

let chip_enabled (tree:node array) (root:int) (fils:fil list) (nb_node:int) (pucea:int) (puceb:int) : int list =
  (* renvoie la liste des puces activées *)

  (* chemins de a et b vers la racine, permet de relier a à b en O(n), n la longueur de la plus longue des deux branches *)
  let path_to_root_a = path_to_root tree root pucea
  and path_to_root_b = path_to_root tree root puceb in

  (* On cherche ensuite l'ancetre le plus proche afin de ne pas avoir un chemin qui "revient sur ses pas"*)
  let ancestor = nearest_ancestor path_to_root_a path_to_root_b in

  (* Finalement les puces activées sont celles de a à l'ancetre commun exclu, de b à l'ancetre commun exclu ainsi que l'ancetre commun. *)
  List.concat [list_to_nth path_to_root_a (ancestor) ; list_to_nth path_to_root_b (ancestor) ; [ancestor]]

let rec produit_signaux (chips:int list) (signaux:int array) : int=
  (* fonction non satifaisante pour un produit trop grand*)
  match chips with
  | t::q -> signaux.(t)*produit_signaux q signaux
  | _ -> 1

let produit_signaux_iter (chips:int array) (signaux:int array) : int =
  (* nous préfèrerons cette fonction itérative qui effectue le modulo dès que notre nombre est suffisamment grand*)
  let produit = ref 1 in
  for i = 0 to (Array.length chips)-1 do
    let res = !produit*signaux.(chips.(i)) in
    if res < 1671404011 then
      produit := !produit*signaux.(chips.(i))
    else
      produit := !produit*signaux.(chips.(i)) mod 1671404011
  done;
  !produit

let calculerSignaux (nb_node:int) (m:int) (q:int) (signaux:int array) (fils:fil list) (questions:question list) =
  (** TODO Affiche le signal envoyé au coffre-fort pour chaque requête *)

   (* arbre représentant les relation entre les noeuds données par les fils.
   * fil.puce1 est la racine, file.puce2 un enfant de fil.puce1 *)
  let tree = get_tree nb_node fils in
  (* il y a existence et uniticé de la racine *)
  let root = Option.get (get_root tree nb_node) in

  let rec answer_questions (n:int) (signaux:int array) (fils:fil list) (questions:question list) =
    match questions with
    | t::q -> print_int ((produit_signaux_iter (Array.of_list (chip_enabled tree root fils n t.puceA t.puceB)) signaux) );
              if not (q = []) then print_newline();
              answer_questions n signaux fils q
    | _ -> ()
  in
  answer_questions nb_node signaux fils questions

let () =
  let nb_node = read_int () in
  let m = read_int () in
  let q = read_int () in
  let signaux = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev in
  let fils = List.init m (fun _ -> Scanf.sscanf (read_line ()) "%d %d" (fun puce1 puce2 -> {puce1; puce2})) in
  let questions = List.init q (fun _ -> Scanf.sscanf (read_line ()) "%d %d" (fun puceA puceB -> {puceA; puceB})) in
  calculerSignaux nb_node m q (array_from_list signaux) fils questions
