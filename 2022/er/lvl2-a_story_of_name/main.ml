(* puisqu'on trie par ordre lexicographique, si un mot suivant a un prefixe commun plus court alors les autres aussi *)
(* et si on commencé à repérer un préfixe d'une certaine taille, alors il ne peut pas subvenir de prefixe plus long et avec + d'occurence *)

let taille = read_int ()
let noms = List.init taille (fun _ -> read_line ()) |> List.sort compare

let longuest_prefix s s' =
  let len_max = min (String.length s) (String.length s') in
  let i = ref 0 in
  while !i < len_max && s.[!i] = s'.[!i] do
    incr i;
  done;
  !i

let rec find_prefix (l: string list) ((s_curr, n_curr, len_curr):string*int*int) ((s_acc, n_acc, len_acc):string*int*int) : string =
  (* Printf.printf "curr = (%s, %i, %i) ; acc = (%s, %i, %i)\n" s_curr n_curr len_curr s_acc n_acc len_acc; *)
  match l with
  | [] -> if n_curr > n_acc then s_curr
          else if n_curr = n_acc then if len_curr > len_acc then s_curr else s_acc
          else s_acc
  | s::t ->
    let new_max_len = longuest_prefix s s_curr in (* on pourrait opti en prenant que les len_curr 1ère lettres de s_curr *)
    let new_prefix = String.sub s_curr 0 new_max_len in
    if new_max_len >= 2 then
      begin
        (* Printf.printf "salut\n"; *)
        find_prefix t (new_prefix, n_curr+1, new_max_len) (s_acc, n_acc, len_acc)
      end
    else
      begin
        if n_curr > n_acc then find_prefix t (s, 0, String.length s) (s_curr, n_curr, len_curr)
        else if n_curr = n_acc then if len_curr > len_acc then (find_prefix t (s, 1, String.length s) (s_curr, n_curr, len_curr))
                                                          else (find_prefix t (s, 1, String.length s) (s_acc, n_acc, len_acc))
        (* n_curr < n_acc *)
        else (find_prefix t (s, 1, String.length s) (s_acc, n_acc, len_acc))
      end

let _ =
  find_prefix noms (List.hd noms, 0, String.length (List.hd noms)) ("", 0, 0)
  |> Printf.printf "%s"
