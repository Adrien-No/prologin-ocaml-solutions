open Printf
open List

type trie = Nil | Node of int * bool * ( (char, trie) Hashtbl.t) (* nb_descendants, is_suffix, (character, childs) *)

let list_of_string s = s |> String.to_seq |> List.of_seq

(** [lol letters] returns {aba | a, b in letters} *)
let language_of_letters (letters: string array) : char list list =
  (* here letters are formaly partial words of the language, not chars *)
  let lang = ref [] in
  let len = Array.length letters in
  for i = 0 to len-1 do
    for j = 0 to len-1 do
      lang := (letters.(i) ^ letters.(j) ^ letters.(i))::!lang
    done
  done;
  !lang |> map (list_of_string)

let get_descendants (t: trie) : int =
  match t with
    Nil -> (* Printf.printf "appelé"; *) 0
  | Node(x, true, _) -> x+1
  | Node(x, false, _) -> x

(** [tol lang] returns a trie that contains words of lang who is sorted alphanumericaly *)
let rec trie_of_language (prof: int) (lang: char list list) : trie =
  match lang with
    [] | _ when prof = 61 -> Nil
  | _ ->

    let rec group_by_initial l acc (cur_char, temp_acc) marked =
      match l with
        [] when temp_acc <> [] -> (cur_char,temp_acc) :: acc, marked
      | [] -> acc, marked
      | word::nexts_w ->
        match word with
        | [] ->                           group_by_initial nexts_w acc (cur_char, temp_acc) true            (* is_suffix   *)
        | c::nexts_c when c = cur_char -> group_by_initial nexts_w acc (cur_char, nexts_c::temp_acc) marked (* same letter *)
        | c::nexts_c                   -> group_by_initial nexts_w ((cur_char, temp_acc)::acc) (c, [nexts_c]) marked
    in
    let groups, is_suffix = group_by_initial lang [] ('`', []) false in
    let nb_descendants = ref 0 in
    let childs = List.map (fun (c,l) -> c, let t = trie_of_language (prof+1) l in nb_descendants := !nb_descendants + (get_descendants t); t) groups |> List.to_seq |> Hashtbl.of_seq in

(* let nb_descendants = Hashtbl.fold (fun src dst acc -> get_descendants dst + acc) childs 0 in (\* TODO can optimize *\) *)
    Node(!nb_descendants, is_suffix, childs)

let find_dicho (x: char) (t: (char * trie) array) : (char * trie) option =
  let len = Array.length t in
  let rec loop i j =
    if j < i then
      None
    else
      let cpt = (j+i) / 2 in
      if fst t.(cpt) = x then
        Some t.(cpt)
      else if fst t.(cpt) > x then
        loop i (cpt-1)
      else
        loop (cpt+1) j
  in
  loop 0 (len-1)

let rec count_postfixs t prefix =
  match t, prefix with
    Nil, [] -> 0
  | Nil, h::t -> 0 (* failwith "count_postfixs: prefix invalide" *)
  | _, [] -> get_descendants t
  | Node(x, b, l), c::prefix_suite ->
    match Hashtbl.find_opt l c with
      None -> 0 (* failwith "count_postfixs: pas trouvé" *)
    | Some trie' ->
      count_postfixs trie' prefix_suite (* TODO par sûr du snd *)

let solve nb_prefixs prefixs nb_vocabs vocabulaire =
  let t =
    vocabulaire
    |> language_of_letters
    |> List.sort_uniq Stdlib.compare (*TODO sort_uniq ? *)
    |> trie_of_language 0
  in
  prefixs
  |> map (count_postfixs t)
  |> List.iteri (fun i x -> print_int x; if i < nb_prefixs-1 then print_newline())

let () =
  let n = read_int () in
  let mots = List.init n (fun _ -> read_line () |> String.to_seq |> List.of_seq) in
  let m = read_int () in
  let vocabulaire = Array.init m (fun _ -> read_line ()) in
  solve n mots m vocabulaire
