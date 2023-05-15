let enum_stabilisateurs p a  =
  (* p : stabilité parfaite
   * a : tableau des accroches *)

  (* renvoie la liste des stabilisateurs possibles, de la forme ((a1,a2,a3,a4), equilibre) *)
  (* en optimisation on pourrait supprimer les accroches-doublons pour cette étape *)
  let n = Array.length a in
  let solutions = ref [] in

  let enum_beetween (i_1:int) (i_4:int) (eq:int) : unit =
    (* ajoute toutes les possibilités d'accroches entre a1 et a2 à solutions *)
    for i_2 = i_1+1 to i_4-2 do
      for i_3 = i_2+1 to i_4-1 do
        (*Printf.printf "zob";*)
        solutions := ((i_1, i_2, i_3, i_4), eq) :: !solutions
      done;
    done
  in

  for i_1 = 0 to n-4 do
    let a1 = a.(i_1) and
    i_4 = ref (i_1+3) in
    let get_eq a1 i_4 =
      (*Printf.printf "dans update : i_4 = %i\n" i_4;*)
      (p - (a.(i_4)-a1)*(a.(i_4)-a1))
    in
    while !i_4 <= n-1 && (get_eq a1 !i_4) > 0 do
      (* on augmente a4 *)
      enum_beetween i_1 !i_4  (get_eq a1 !i_4);
      incr i_4;
    done;
  done;
  List.sort (fun (_,x) (_,y) -> compare y x) !solutions

let dfs a_restantes stabs nb_stabs =
  let rec aux (a:(bool*int) array) stabs (stabilite_max:int) nb_stabs =
    match stabs with
      [] -> stabilite_max
    | ((i1,i2,i3,i4),eq)::stabs2 ->
      (* on fait les deux cas possibles *)
      if nb_stabs > 0 && a.(i1) |> fst && fst a.(i2) && fst a.(i3) && fst a.(i4) then begin
        (* a' c'est les accroches encore dispo si on prend le stabilisateur *)
        let a' = Array.copy a in
        a'.(i1) <- false,snd a.(i1);
        a'.(i2) <- false,snd a.(i2);
        a'.(i3) <- false,snd a.(i3);
        a'.(i4) <- false,snd a.(i4);
        (* on prend le meilleur cas entre prendre ou non le stabilisateur et adaptons en conséquences le tableau des accroches restantes *)
        (*                 prendre                         |       pas prendre                 *)
        max (aux a' stabs2 (stabilite_max+eq) (nb_stabs-1)) (aux a stabs2 stabilite_max nb_stabs)
      end
      else
        (* on est obligé de pas prendre le stabilisateur *)
        aux a stabs2 stabilite_max nb_stabs
  in
  aux a_restantes stabs 0 nb_stabs

let stabiliteMaximale _ k p accroches =
  let a = Array.of_list accroches in
  Array.sort compare a;

  let stabs = enum_stabilisateurs p a in

  (*** operation douteuse sur la liste des stabilisateurs ***)
  let eq_max = if stabs <> [] then snd (List.hd stabs) else -1 in
  let stabs = List.fold_left (fun b x -> if snd x < eq_max/10 then b else x::b) [] stabs in
  let a_restantes = Array.map (fun h -> true,h) a in
  Printf.printf "%i" (dfs a_restantes stabs k)

let () =
  let n = read_int () in
  let k = read_int () in
  let p = read_int () in
  let accroches = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev in
  stabiliteMaximale n k p accroches
