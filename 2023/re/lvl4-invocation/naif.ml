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

let solve nb_prefixs prefixs nb_vocabs vocabulaire =
  List.map (count_postfixs nb_vocabs vocabulaire) prefixs
  |> List.iteri (fun i x -> print_int x; if i < nb_prefixs -1 then print_newline())

let () =
  let n = read_int () in
  let mots = List.init n (fun _ -> read_line () |> String.to_seq |> List.of_seq) in
  let m = read_int () in
  let vocabulaire = List.init m (fun _ -> read_line () |> String.to_seq |> List.of_seq) in
  solve n mots m vocabulaire
