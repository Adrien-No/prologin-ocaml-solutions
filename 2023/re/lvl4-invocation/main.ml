open Printf

type trie = Nil | Node of int * bool * ((char * trie) list) (* nb_descendants, est_suffixe, (caractere, enfants) *)

let get_descendants (t: trie) : int =
  match t with
    Nil -> (* Printf.printf "appelé"; *) 0
  | Node(x, true, _) -> x+1
  | Node(x, false, _) -> x

(* ================================================================ TRIE ================================================================ *)

let spc n = String.init n (fun _ -> ' ') |> print_string
let print_trie t =
  let rec aux t space =
    match t with
      Nil -> ()
    | Node(x, b, l) ->
      spc space; Printf.printf "%i %b [\n" x b;
      List.iter (fun (c, t) -> spc space; Printf.printf "%c\n" c; aux t (space+8)) l;
      spc space; Printf.printf "]\n"
  in
  aux t 0

let get_trie (vocab: char list list) =
  let t_vocab = Array.of_list vocab in
  let len = Array.length t_vocab in
  let langage = ref [] in
  for i = 0 to len-1 do
    for j = 0 to len-1 do
        langage := (t_vocab.(i)@t_vocab.(j)@t_vocab.(i))::!langage
      done
    done;
  (* TODO sort_uniq ? *)
  let langage = !langage |> List.sort compare in
  printf "\nlangage (%i elt, ) = [" (List.length langage);
  List.iter (fun l -> Printf.printf "%s, " (l |> List.to_seq |> String.of_seq )) langage;
  printf "]\n";
  let regroupe mots : (char option * char list list) list =
    if List.exists ((=)[]) mots then printf "is_suffix\n";
    (* il faut mémoriser si un mot s'arrête sur ce noeud, càd un des mots est nul *)
    let groupes, dernier_groupe = List.fold_left (fun (acc, (temp_acc: char option * char list list)) mot ->
      match temp_acc, mot with
      | _, [] -> (acc, (None, [])) (* inutile d'ajouter un autre `[]` *)
      | (None  , l), c'::t              -> (None  , []) :: acc, (Some c', [t]) (* nouveau groupe *)
      | (Some c, l), c'::t when c <> c' -> (Some c, l ) :: acc, (Some c', [t]) (* ''             *)
      | (Some c, l), c'::t              ->                 acc, (Some c', t::l)(* when c = c'    *)
    )
    (List.hd mots |> function [] -> (* printf "is_suffix\n"; *) [], (None, []) | c::t -> [], (Some c, [])) (* nous permet d'obtenir le groupe (None, []) ssi il y a un mot vide (car en vertue du `sort compare` il sera en premier)*)
    mots                                                                       (* le "t" sera répété donc pas besoin de le mettre maintenant *)
    in
    match groupes, dernier_groupe with
    | [], g  -> [g]
    | _, (None, []) -> [] (* failwith "bizarre" *) (* devrait pas arriver *)
    | _, g -> g::groupes
  in
  (* TODO ajouter les mots prefixés (param bool dans trie) *)
  let rec loop bouts_restants : trie =
    match regroupe bouts_restants with
      [] -> Nil
    | l  ->
      let is_suffix, (enfants: (char * trie) list) = List.fold_left (fun (is_suffix, acc) couple ->
          match couple with
          | (None, l) -> assert(l = []); true, acc
          | (Some c, l) -> is_suffix, (c, loop l)::acc
        )
        (false, [])
        l
      in
      (* if is_suffix then Printf.printf "================\n\nis suffix\n\n================"; *)
      let nb_descendants = enfants |> List.map (fun c -> snd c |> get_descendants) |> List.fold_left (+) 0 in
      Node (nb_descendants, is_suffix, enfants)
  in
  loop langage

(* ================================================================ *)

let rec count_postfixs t prefix =
  match t, prefix with
    Nil, [] -> 0
  | Nil, h::t -> 0 (* failwith "count_postfixs: prefix invalide" *)
  | _, [] -> get_descendants t
  | Node(x, b, l), c::prefix_suite ->
    (* TODO améliorable avec dichotomie *)
    match List.find_opt (fun (c', _) -> c' = c)  l with
      None -> 0 (* failwith "count_postfixs: pas trouvé" *)
    | Some (_, trie') ->
      count_postfixs trie' prefix_suite
(* ====================================================================================================================================== *)

let solve nb_prefixs prefixs nb_vocabs vocabulaire =
  let t = get_trie vocabulaire in
  print_trie t;
  List.map (count_postfixs t) prefixs
  |> List.iteri (fun i x -> print_int x; if i < nb_prefixs-1 then print_newline())

let () =
  let n = read_int () in
  let mots = List.init n (fun _ -> read_line () |> String.to_seq |> List.of_seq) in
  let m = read_int () in
  let vocabulaire = List.init m (fun _ -> read_line () |> String.to_seq |> List.of_seq) in
  solve n mots m vocabulaire
