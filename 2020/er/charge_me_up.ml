Printexc.record_backtrace true

let print_grille grille =
  Array.iter (fun l -> Array.iter (fun x -> Printf.printf "%c " x) l; Printf.printf "\n") grille

let dfs grille n (x_p,y_p) : int =
  (* x_p et y_p sont les coordonées x et y de la prise initiale *)
  (* parcours en profondeur de la grille suivant les cables et depuis la prise renvoyant le nombre de cables adjacents (H/B/G/D) à une prise *)

  let marque = Array.make_matrix n n false in

  let rec aux (s:(int*int) list) =
    (* s c'est la pile des positions à traiter *)
    match s with
      [] -> 0
    | (x,y)::q ->
      if 0 > x || x >= n || 0 > y || y >= n
         || (not (grille.(x).(y) = 'C') && marque.(x).(y)) then 0
      else begin marque.(x).(y) <- true;
      match grille.(x).(y) with
          'C' -> 1
        | '+' -> aux ((x+1,y)::q) + aux ((x-1,y)::q)
                 + aux ((x, y+1)::q) + aux ((x,y-1)::q)
        | _ -> 0
      end
   in
   (* il faut initialiser le dfs avec un cable adjacent à la prise initiale (sinon on sort tout de suite) et bien faire la somme car plusieurs chemins peuvent rapporter des "points d'énergie"*)
   aux [(x_p+1,y_p)] + aux [(x_p-1,y_p)] + aux [(x_p, y_p+1)] + aux [(x_p, y_p-1)]

let find_prises (grille:char array array) : (int*int) list =
  let res = ref [] in
  Array.iteri (fun x l ->
      Array.iteri (fun y p ->
          if p = 'P' then res := (x,y)::!res) l) grille;
  !res

let chargeMeUp n grille =
  grille
  |> find_prises
  |> List.map (fun p -> dfs grille n p)
  |> List.fold_left max 0
  |> (fun maxi -> Printf.printf "%i" (maxi*6))

let () =
  let n = read_int () in
  let panel = Array.init n (fun _ -> Array.init n (String.get (read_line ()))) in
  chargeMeUp n panel
