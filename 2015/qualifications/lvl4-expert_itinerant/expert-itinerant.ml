(* ladj est une liste d'adjacence qui représente un graphe orienté à n sommets*)
(* on les numérotes de 0 à n-1 et on suppose n pré-défini *)
(* le premier entier du couple est le sommet d'arrivé, le deuxième entier est le coût. *)
(* les requetes seront des couples (depart,arrivee) *)

(* Calcul de la complexité : *)
(* On fait R dfs sur un graphe de N sommets et M arrêtes. *)
(* Mais lors de notre dfs, chaque sommets peut être atteint de "nombres d'arêtes entrantes" manières différentes. Chaque couple de sommets (x,y) *)
type ladj = (int*int) list array

(* ################ affichage ################ *)
let print_graph ladj =
  Printf.printf "[|\n";
  Array.iteri (fun i l -> List.iter (fun (y,p) -> Printf.printf "(%i, %i) " y p) l; Printf.printf "\n") ladj;
  Printf.printf "|]\n"

let print_requetes (l:(int*int) list) =
  List.iter (fun (x,y) ->  Printf.printf "(%i,%i) " x y) l; Printf.printf "\n"

let print_int_list l =
  let len = List.length l in
  List.iteri (fun i x -> Printf.printf "%i" x; if i < len-1 then Printf.printf "\n") l

(* ########################################### *)

let distance_min (n:int) (ladj:ladj) ((a,b):int*int): int =
  (* on veut le plus court chemin au sens "somme des poids des chemins" du point a au point b dans le graph ladj *)
  (* On réalise un simple parcours en profondeur (c'est assez naïf) *)
  let marque = Array.make n false in
  (* la variable de référence nous permet de limiter la profondeur du parcours ; si l'on a dépassé la distance minimale obtenue pour l'instant alors pas besoin de continuer sur cette branche (poids tous positifs) *)
  let dist_min = ref max_int in
  let rec dfs x distance =
    (* Les conditions de la prochaine ligne de code ne sont pas évidentes. Le raisonnement est le suivant : *)
    (* Bien que le sommet soit déjà marqué, il peut faire parti de plusieurs chemins, dont le deuxième peut être meilleur que le premier, d'où la première condition du "OU". *)
    (* Mais il ne faut pas que l'on "boucle" sur des sommets qui nous donnent aucun chemins de distance "non infinie" *)
    (* car l'on ne découvrirait alors jamais un potentiel chemin existant bien, *)
    (* et si l'on a pas encore trouvé de chemin on a distance = !dist_min. D'où la deuxième condition du "OU". *)
    if (marque.(x) && distance > !dist_min) ||  (marque.(x) && !dist_min = max_int) then ()
    else begin if x = b then dist_min := min distance !dist_min;
      marque.(x) <- true;
      List.iter (fun (y,p) -> dfs y (distance+p)) ladj.(x)
    end
  in
  dfs a 0;
  !dist_min

let _ =
  let n, n_edges, n_requetes = Scanf.scanf "%i %i %i " (fun x y z -> x,y,z) in (* n is the vertices number *)
  let ladj = Array.make n [] in
  (* read and add edges *)
  for i = 0 to n_edges-1 do
    Scanf.scanf "%i %i %i " (fun x y p -> ladj.(x-1) <- (y-1,p) :: ladj.(x-1))
  done;
  let requetes = List.init (n_requetes) (fun _ -> Scanf.scanf "%d %d " (fun depart arrivee -> (depart-1,arrivee-1)) ) in


  List.map (distance_min n ladj) requetes
  |> print_int_list
