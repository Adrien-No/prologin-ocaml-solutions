(* ################ on fait le max entre prendre et pas prendre à chaque fois et on dit que c'est impossible si jamais deux se croisent. ################ *)
(*     (\* on suppose choisis triée par décroissance selon les abscisses gauches, et que choisis est sans croisement. *\) *)
(*     (\* fibre possède une abscisse gauche supérieur(car on avait trié le tout), donc on regarde si son abscisse droite est supérieure à toutes les autres, cad >= à max_y*\) *)

let test_croisements fibres =
  (* On veut qu'une fois avoir trié les X (gauche), les Y soient dans l'ordre croissant. *)
  (* Les X sont deja dans l'ordre décroissant. On verifie que les Y le sont aussi. *)
  let rec aux_test f last_y =
    match f with
      [] -> false
    | (_,y)::q -> not (y <= last_y) || (aux_test q y)
  in
  aux_test fibres max_int

let test_croisements2 fibres = match fibres with
    [] | [_] -> false
  | t1::t2::q -> snd t1 < snd t2

let rec pow a b =
  if b = 0 then 1
  else a * pow a (b-1)

let naive n fibres =
  let meilleur = ref 0 in
  let cache = Hashtbl.create (pow 2 n) in
  let rec aux_naive choisis fibres_restantes =
    if test_croisements2 choisis then () else
    match fibres_restantes with
      [] -> meilleur := max !meilleur (List.length choisis)
    | t::q ->
      max (aux_naive (t::choisis) q) (aux_naive choisis q)
  in
  aux_naive [] fibres;
  !meilleur


(* ################ ce qui précède donne un résultat optimal mais pas assez rapide, donc on va essayer d'être + malin. ################  *)
(* On veut la plus longue sous-suite croissante selon y, une fois qu'on a déjà trié les fibres selon x. *)
(* Quand on prend un élement, le risque est qu'il soit grand, si bien qu'il nous "empêche trop" de prendre des suivants. Il faut donc faire des choix judiceux : en prendre dès que possible(au sens où ça permet d'être optimal) mais pas monter trop vite.*)

(* let fusion l1 l2 = *)
(*   (\* l1 et l2 sont triées, on veut une sous-suite de longueur maximal comprenant des élements de l1 concat à des elts de l2 dans le même ordre qu'ils l'étaient dans la liste originelle.*\) *)
(*   (\* pas d'info sur la plus grande *\) *)
(*   Printf.printf "fusion de : " ; print_int_list l1 ; Printf.printf " et de " ; print_int_list l2 ; *)
(*   if List.length l1 < List.length l2 then *)
(*     (\* On ajoute tant d'élements  qu'on peut de l1 à l2. *\) *)
(*     let rec aux_fus l1 acc l2 = *)
(*       match l1 with *)
(*         [] -> (List.rev acc) @ l2 *)
(*       | t::q -> *)
(*         if t <= List.hd l2 then aux_fus q (t::acc) l2 *)
(*         else (List.rev acc) @ l2 (\* on aura pas moins pire car l1 est triée par ordre croissant *\) *)
(*     in *)
(*     let l = aux_fus l1 [] l2 in *)
(*     Printf.printf "\nOn obtient :" ; print_int_list l ; print_newline() ; *)
(*     l *)
(*   else *)
(*     (\* On ajoute tant d'élements qu'on peut de l2 à la fin de l1 *\) *)
(*     let rec aux_fus l1 l2 = *)
(*       (\* on renversera l1 lorsqu'on la mettra en paramètre, au cas initial. Elle devient donc décroissante.*\) *)
(*       match l2 with *)
(*         [] -> List.rev l1 (\* on renverse à nouveau pour qu'elle soit au final à l'endroit *\) *)
(*       | t::q -> *)
(*         if List.hd l1 <= t then aux_fus (t::l1) q *)
(*         else List.rev l1 *)
(*     in *)
(*     let l = aux_fus (List.rev l1) l2 in *)
(*     Printf.printf "\nOn obtient :" ; print_int_list l ; print_newline() ; *)
(*     l *)

(* let fusion2 l1 l2 = *)
(*   Printf.printf "\nfusion de : " ; print_int_list l1 ; Printf.printf " et de " ; print_int_list l2 ; *)
(*   let rec aux_fus l1 l2 = *)
(*     (\* l1 est décroissante, l2 est croissante *\) *)
(*     match l1 with *)
(*       [] -> l2 *)
(*     | t::q -> *)
(*       if t > List.hd l2 then aux_fus q l2 *)
(*       else aux_fus q (t::l2) *)
(*   in *)
(*   let l = aux_fus l1 l2 in *)
(*   Printf.printf "\nOn obtient :" ; print_int_list l ; print_newline() ; *)
(*   l *)

(* let fusion3 l1 l2 = *)
(*   (\* on a deux curseurs, i pour l1 et j pour l2 représentant les entiers gardés de l1 et de l2. Pour l1 c'est de 0 à i, pour l2 c'est de j à (List.length l2 -1) *\) *)
(*   (\* on part avec i = n et j = 0, on essaie de décaler  *\) *)

(* let cut_in_2 l = *)
(*   Printf.printf "\nOn coupe "; print_int_list l; Printf.printf "en deux\n"; *)
(*   let moitie = (List.length l) / 2 in *)
(*   let rec aux_cut i l1 l2 = *)
(*     if i < moitie then *)
(*       match l1 with *)
(*         [] -> failwith "cut_in_two cas impossible" *)
(*       | t::q -> aux_cut (i+1) q (t::l2) *)
(*     else *)
(*       let l1, l2 = List.rev l2,l1 in *)
(*       Printf.printf "On obtient :"; print_int_list l1 ; print_int_list l2 ; print_newline(); *)
(*       l1,l2 *)

(*   in *)
(*   aux_cut 0 l [] *)

(* let rec sss_croissante l = *)
(*   (\* sous-suite croissante *\) *)
(*   (\* on va couper en deux jusqu'à obtenir des sous-suites croissantes de longueur 1, puis on fusionne en conservant cette propriété avec les concaténés de sous-suites construites. *\) *)
(*   match l with *)
(*     [] | [_] -> l *)
(*   | _ -> *)
(*     let l1, l2 = cut_in_2 l in *)
(*     fusion2 (sss_croissante l1) (sss_croissante l2) *)

let print_int_list l = Printf.printf "[ "; List.iteri (fun i x -> Printf.printf "%i" x; if i < List.length l then Printf.printf ", ") l; Printf.printf "] "

let len_sss_croissante l =
  let len = List.length l in
  (* les tailles maximales des sss (sous-suites croisantes) où maxs.(i) = [i; ... ; len-1] *)
  let maxs = Array.make len (-1) in
  let best_length
  let rec dfs i (maximum:int) l =
    (* parcours en profondeur de l'arbres des possibilités mais en enregistrant les résultats (longueurs des sss-listes) déjà calculés. *)
    (* i c'est l'indice actuel dans la liste, *)
    (* maximum est le plus grand entier choisi dans une sous-liste *)
    (* l la liste restante à parcourir *)
    (* Printf.printf "maximum = %i | " maximum; print_int_list l; print_newline(); *)
    match l with
      [] -> 0
    | t::q ->
      (* test on a fini la liste *)
      if i = len then 0
      (* test on connaît déjà le résultat *)
      else if maxs.(i) <> -1 then maxs.(i)
      (* test on peut pas prendre t *)
      else if t < maximum then let leni = dfs (i+1) maximum q in (maxs.(i) <- leni; leni)
      (* on sélectionne la meilleure possibilité entre prendre t et ne pas le prendre. *)
      (* Attention à l'ordre d'appel de dfs. *)
      else let leni = max (dfs (i+1) maximum q) (1 + (dfs (i+1) t q))  in (maxs.(i) <- leni; leni)
  in
  dfs 0 0 l

let urgence_reseau n fibres =
  let l = List.map snd fibres in
  len_sss_croissante l

let _ =
  let n = read_int() in
  let fibres = List.init n (fun _ -> Scanf.scanf "%i %i " (fun x y -> (x,y))) in
  urgence_reseau n (List.sort (fun x y -> if fst x <> fst y then compare (fst x) (fst y) else compare (snd x) (snd y)) fibres)
  |> Printf.printf "%i"
