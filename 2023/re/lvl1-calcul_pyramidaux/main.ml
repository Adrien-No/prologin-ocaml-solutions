
(**
   @param hauteur la hauteur de la salle
   @param largeur la largeur de la salle
   @param longueur la longueur de la salle
*)
let tailleMinimale h l l' =
  (** TODO Afficher, sur une ligne, un entier: la hauteur minimale de la
  pyramide pour pouvoir contenir la salle.  *)
  h + (max l l') - 1
  |> print_int


let () =
  let hauteur = read_int () in
  let largeur = read_int () in
  let longueur = read_int () in
  tailleMinimale hauteur largeur longueur
