(* On va appliquer la méthode du petit train ; on prend les k premiers élements de chaine, et on compte le nombre de chaque exigence. Si toutes les exigences sont remplis, alors on ajoute une sn*)
(* Ensuite on ajoute un caractère, le k+1 et on enleve le caractere 1, on met à jour ainsi le tableau des exigences et on reste la sn*)
(* On fait cela jusqu'au caractère n-1*)

(**
   @param n taille de la chaîne
   @param k taille du mot de passe
   @param chaine la chaîne contenant le mot de passe
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

type exigences = { mutable minuscule : int ;
                   mutable majuscule : int ;
                   mutable nombre : int ;
                   mutable spe_char : int }

let char_type (char:'a) (exi:exigences) (add:Bool.t): unit =
  (* On incremente/decremente (suivant add) un champ de exi en fonction du type de caractere de char *)
  let ascii_char = Char.code char in
  (* nombre *)
  if 47 < ascii_char && ascii_char < 58 then if add then exi.nombre <- exi.nombre + 1 else   exi.nombre <- exi.nombre - 1
  (* majuscule *)
  else if 64 < ascii_char && ascii_char < 91 then if add then exi.majuscule <- exi.majuscule + 1 else exi.majuscule <- exi.majuscule - 1
  (* minuscule *)
  else if 96 < ascii_char && ascii_char < 123 then if add then exi.minuscule <- exi.minuscule + 1 else exi.minuscule <- exi.minuscule - 1
  (* caractere special *)
  else if add then exi.spe_char <- exi.spe_char + 1 else exi.spe_char <- exi.spe_char - 1

let test_password (exi:exigences) =
  (* renvoie true/false selon si le mot de passe correspond aux exigences au non.*)
  (* pas besoin de tester la longeur, on la suppose bonne mais on pourrait fait la somme des champs de exi *)
  if exi.minuscule > 0 && exi.majuscule > 0 && exi.nombre > 0 && exi.spe_char > 0 then
    true
  else
    false

let fuiteDeClavier (n:int) (k:int) (chaine:char array) : unit =
  (** TODO afficher le nombre de mots de passes possibles parmi la chaîne *)

  (* donne les informations sur le mot de passe actuel  *)
  let exi : exigences = { minuscule = 0 ; majuscule = 0 ; nombre = 0 ; spe_char = 0} in
  (* somme des mots de passe possibles *)
  let possibilities = ref 0 in
  (* on test les k-premiers caractères de n*)
  for i = 0 to k-1 do
    char_type chaine.(i) exi true
  done;
  (* si il y a deja une solution*)
  if test_password exi then possibilities := !possibilities + 1;

  (* on ajoute un nouveau caractere et on enleve le plus ancien, et cela jusqua la fin de la liste *)
  for i = k to n-1 do
    (* nouveau caractere *)
    char_type chaine.(i) exi true;
    (* ancien, on l'enleve d'ou le "false" *)
    char_type chaine.(i-k) exi false;
    (* on teste de mot de passe correspondant a la sous-chaine de chaine [k:i+1]*)
    if test_password exi then possibilities := !possibilities +1
  done;
  print_int !possibilities

let () =
  let n = read_int () in
  let k = read_int () in
  let chaine = List.init n (String.get (read_line ())) in
  fuiteDeClavier n k (Array.of_list chaine)
