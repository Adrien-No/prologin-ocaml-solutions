(* on réalise un parcours en largeur(bfs) sur les 4 possiblités de déplacements (H/B/G/D) pour avoir l'information du plus court nombre de chocs (une solution plus longue ne nous intéresse pas s'il en existe une plus longue.) *)
(* Pour éviter les éventuelles répétitions l'on stocke les cases sur lesquelles on s'est déjà arrêté dans une table de hachage (puisque parcours en largeur, la deuxième fois que l'on passe dessus sera moins bien que la première.) *)

type direction = Haut | Bas | Gauche | Droite

let print_carte carte =
  Printf.printf "[|";
  Array.iter (fun l -> Array.iter (fun c -> Printf.printf "%c " c) l; Printf.printf "\n") carte;
  Printf.printf "|]\n"

exception Trouve of int
let tete_baissee (n:int) carte =
  (* Inserez votre code ici *)
  let trouver_t () : int*int =
    let res = ref (-1,-1) in
    Array.iteri (fun x l -> Array.iteri (fun y case -> if case = 'T' then res := (x,y)) l) carte;
    !res
  in
  let avancer (x0,y0) (d:direction) : int*int =
    let rec aux_avancer (x,y) (next_x, next_y) =
      (* Printf.printf "(%i,%i)\n" x y; *)
      if next_x < 0 || next_x > n || next_y < 0 || next_y > n || carte.(next_x).(next_y) = 'X' || carte.(x).(y) = 'M' then
        (* stop *)
        if x = next_x && y = next_y then (-1, -1) else (x,y)
      else
        (* on peut avancer *)
        match d with
          Haut -> aux_avancer (next_x, next_y) (next_x, next_y-1)
        | Bas -> aux_avancer (next_x, next_y) (next_x, next_y+1)
        | Gauche -> aux_avancer (next_x, next_y) (next_x-1, next_y)
        | Droite -> aux_avancer (next_x, next_y) (next_x+1, next_y)
    in
    aux_avancer (x0,y0) (x0,y0)
  in
  let s = Queue.create() in
  Queue.push (0,trouver_t ()) s;
  let h = Hashtbl.create (n*n) in
  (* bfs *)
  let rec bfs_aux() =
    try
      if Queue.is_empty s then (Printf.printf "\npas trouvé\n";raise Not_found)
      else
        begin
          let chocs,(x,y) = Queue.pop s in
          (* Printf.printf "(%i, %i)\n" x y; *)
          if carte.(x).(y) = 'M' then raise (Trouve (chocs-1)) else (
            [Haut;Bas;Gauche;Droite]
            |> List.map (fun d ->
                avancer (x, y) d)
            (* let x',y' = avancer (x,y) d in Printf.printf "(%i, %i) " x' y'; x',y') *)
            |> List.iter (fun (x,y) -> if not (Hashtbl.mem h (x,y)) then (Hashtbl.add h (x,y) true; Queue.push ((chocs+1),(x,y)) s)) ;
            bfs_aux()
            (* Printf.printf "\n"; *)
          )
        end
    with Trouve x -> Printf.printf "%i" x
  in
  bfs_aux()

let () =
  let n = read_int() in
  let string_to_char_list s = let rec aux acc = if acc = (String.length s) then [] else s.[acc]::(aux (acc+1)) in aux 0 in
  (* ces deux versions marchent toutes les deux : il y a bien un "espace" à la fin de l'entrée ; l'on pouvait avoir un doute en copiant l'entrée sur le site dans notre presse-papier. *)
  (* let carte = Array.init n (fun _ -> read_line() |> string_to_char_list |> Array.of_list) in *)
  let carte = Array.init n (fun _ -> Scanf.scanf "%s " (fun s -> Array.init n (fun i -> s.[i]))) in
  (*print_carte carte;*)
  tete_baissee (n-1) carte
