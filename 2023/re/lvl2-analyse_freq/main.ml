
(**
   @param n le nombre de caractères gravés sur la stèle
   @param contenu le texte gravé sur la stèle
   @param occurences la liste contenant les nombres d'occurrences des lettres de A à Z
*)

let contenuDechiffre (n: int) (contenu: char list) (occurences: int list) =
  (** TODO Une chaîne de caractères contenant le texte déchiffré  *)
  let occ_to_char = Hashtbl.create 32 in
  List.iteri (fun i occ -> Hashtbl.add occ_to_char occ (Char.chr (Char.code 'a' + i)) ) occurences;

  let occ_contenu = Array.make 26 0 in
  List.iter (fun c ->
      let i = Char.code c - (Char.code 'a') in
      occ_contenu.(i) <- 1 + occ_contenu.(i)
    ) contenu;

  let dechiffre c : char =
    let occ = occ_contenu.(Char.code c - Char.code 'a') in
    Hashtbl.find occ_to_char occ
  in
  List.map dechiffre contenu
  |> List.map Char.uppercase_ascii
  |> List.to_seq
  |> String.of_seq
  |> print_string


let () =
  let n = read_int () in
  let contenu = List.init n (String.get (read_line ())) in
  let occurences = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev in
  contenuDechiffre n contenu occurences
