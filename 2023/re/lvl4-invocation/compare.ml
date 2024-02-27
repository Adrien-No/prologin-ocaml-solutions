open Printf
open List

type trie = Nil | Node of int * bool * ((char * trie) list) (* nb_descendants, is_suffix, (character, childs) *)

let list_of_string s = s |> String.to_seq |> List.of_seq

(** [lol letters] returns {aba | a, b in letters} *)
let language_of_letters (letters: char list list) : char list list =
  (* here letters are formaly partial words of the language, not chars *)
  List.fold_left (fun acc a ->
      List.fold_left
        (fun acc b -> (a@b@a) :: acc) (* TODO can optimize if need *)
        [] letters :: acc
    ) [] letters
  |> List.concat

let get_descendants (t: trie) : int =
  match t with
    Nil -> (* Printf.printf "appelé"; *) 0
  | Node(x, true, _) -> x+1
  | Node(x, false, _) -> x

(** [tol lang] returns a trie that contains words of lang who is sorted alphanumericaly *)
let rec trie_of_language (lang: char list list) : trie =
  match lang with
    [] -> Nil
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
    let childs = List.map (fun (c,l) -> c, trie_of_language l) groups in
    let nb_descendants = childs |> map snd |> map get_descendants |> List.fold_left (+) 0 in (* TODO can optimize *)
    Node(nb_descendants, is_suffix, childs)

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

let solve nb_prefixs prefixs nb_vocabs vocabulaire =
  let t =
    vocabulaire
    |> language_of_letters
    |> List.sort Stdlib.compare (*TODO sort_uniq ? *)
    |> trie_of_language
  in
  prefixs
  |> map (count_postfixs t)
  (* |> List.iteri (fun i x -> print_int x; if i < nb_prefixs-1 then print_newline()) *)


(* ################################################################ naive ################################################################ *)

let rec list_cut x l =
  if x <= 0 then []
  else match l with
      [] -> '`':: list_cut (x-1) []  (* on complète avec un caractère n'importe lequel mais pas dans l'alphabet de vocab*) (* failwith "x > |l|" *)
    | h::t -> h :: (list_cut (x-1) t)

let count_postfixs nb_vocabs (words: char list list) prefix =
  let len_prefix = List.length prefix in
  let t_vocab = Array.of_list words in
  let langage = ref [] in
  for i = 0 to nb_vocabs-1 do
    for j = 0 to nb_vocabs-1 do
        langage := (t_vocab.(i)@t_vocab.(j)@t_vocab.(i))::!langage
      done
    done;
  List.find_all (fun word -> List.for_all2 (=) prefix word) (!langage |> List.map (list_cut len_prefix))
  |> List.length

let solve2 nb_prefixs prefixs nb_vocabs vocabulaire =
  List.map (count_postfixs nb_vocabs vocabulaire) prefixs
  (* |> List.iteri (fun i x -> print_int x; if i < nb_prefixs -1 then print_newline()) *)

(* ######################################################################################################################################### *)

open List

let compare_solve n mots m vocabulaire =
  if solve n mots m vocabulaire <> solve2 n mots m vocabulaire then
    begin
      printf "n= %i, mots= %s, m= %i, vocabulaire= %s\n\n" n (mots |> List.map (List.cons ' ') |> List.concat |> List.to_seq |> String.of_seq ) m (vocabulaire |> List.map (List.cons ' ') |> List.concat |> List.to_seq |> String.of_seq);
    end

let _ =
  let open Random in
  for i = 0 to 100 do
    let n = (int 3)+1 in
    let mots = List.init n (fun _ -> let k = (int 3)+1 in String.init k (fun _ -> Char.chr (97 + (int 1000 mod 2)))   |> String.to_seq |> List.of_seq) in
    let m = (int 3)+1 in
    let vocabulaire = List.init m (fun _ -> let k = (int 3)+1 in String.init k (fun _ -> Char.chr (97 + (int 1000 mod 2))) |> String.to_seq |> List.of_seq) in
    compare_solve n mots m vocabulaire
  done
