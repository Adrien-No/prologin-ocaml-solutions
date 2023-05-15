
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

(**
   @param n nombre de puces
   @param m nombre de fils
   @param r nombre de questions
   @param signaux liste des signaux
   @param fils liste des fils entre les puces
   @param questions liste des questions
*)

let get_tree (n:int) (fils:fil list) =
  (* renvoie un arbre de n noeuds numérotés de 0 à n-1 représenté par un tableau de listes
   * On accèdera ensuite aux fils d'un noeud avec a.(noeud)*)
  let a = Array.make n [] in
  let rec set_value (a:'list array) (fils:fil list) =
    match fils with
    | t::q -> a.(t.puce1) <- t.puce2::a.(t.puce1) ; set_value a q
    | [] -> ()
  in
  set_value a fils;
  a

  
let calculerSignaux (n:int) (m:int) (r:int) (signaux:int list) (fils:fil list) (questions:question list) =
  (** TODO Affiche le signal envoyé au coffre-fort pour chaque requête *)
  ()

let () =
  let n = read_int () in
  let m = read_int () in
  let r = read_int () in
  let signaux = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev in
  let fils = List.init m (fun _ -> Scanf.sscanf (read_line ()) "%d %d" (fun puce1 puce2 -> {puce1; puce2})) in
  let questions = List.init r (fun _ -> Scanf.sscanf (read_line ()) "%d %d" (fun puceA puceB -> {puceA; puceB})) in
  calculerSignaux n m r signaux fils questions
