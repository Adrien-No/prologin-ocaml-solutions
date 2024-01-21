
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

(** Structure pour designer une piece *)
type structpiece = {
  ncotespiece : int; (** le nombre de cotes de la pièce *)
  couleurpiece : string; (** la couleur de la pièce *)
}

(*
   @param ncouleurs le nombre de couleurs
   @param couleurs les différentes couleurs possibles
   @param ncotes le nombre de côtés de la pièce manquante
   @param couleurscotes les couleurs des pièces adjacentes à la pièce manquante
   @param npieces le nombre de pièces à trier
   @param pieces les pièces à trier
*)
let rec is_in el liste =
    (* renvoie true si l'element el est dans la liste, false sinon *)
    match liste with
    | [] -> false
    | t::q -> if t = el then true else is_in el q

let rec piece_test (ncotes:int) (couleurscote:string list) (pieces:structpiece list) =
  (* teste les pieces contenues dans la liste pieces et renvoie le nombre de correspondances
   * affiche aussi les "X" / "O" correspondant à la validité de chacunes des pieces *)
  match pieces with
  | [] -> print_newline();
          0
  | t::q ->
    if t.ncotespiece = ncotes &&  not (is_in t.couleurpiece couleurscote) then
      begin
        print_string "O";
        1 + piece_test ncotes couleurscote q
      end
    else
      begin
        print_string "X";
        piece_test ncotes couleurscote q
      end

let resoudre (ncouleurs:int) (couleurs:string list) (ncotes:int) (couleurscotes:string list) (npieces:int) (pieces:structpiece list) =
  (** TODO Affiche sur la première ligne, pour chaque pièce un caractère 'O' si
  la pièce peut correspondre à celle recherchée, 'X' sinon. Affiche sur la
  ligne suivante le nombre de pièces qui peuvent correspondre.  *)

  (* pour chaque piece on regarde si le nombre de cote est le bon et si sa couleur n'est pas une adjacente*)
  print_int (piece_test ncotes couleurscotes pieces)
  (* let nb_correspondances = ref 0 in
   * for i = 0 to npieces-1 do
   *   if pieces.(i).(0) = ncotes && not (is_in pieces.(i).(1) couleurs) then
   *       begin
   *       print_string "O";
   *       nb_correspondances := !nb_correspondances + 1;
   *       end
   *   else
   *       print_string"X"
   * done;
   * print_newline();
   * print_int !nb_correspondances *)

let () =
  let ncouleurs = read_int () in
  let couleurs = List.init ncouleurs (fun _ -> read_line ()) in
  let ncotes = read_int () in
  let couleurscotes = List.init ncotes (fun _ -> read_line ()) in
  let npieces = read_int () in
  let pieces = List.init npieces (fun _ -> let ncotespiece = read_int () in let couleurpiece = read_line () in {ncotespiece; couleurpiece}) in
  resoudre ncouleurs couleurs ncotes couleurscotes npieces pieces
