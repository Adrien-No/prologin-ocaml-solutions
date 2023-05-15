(* ladj est une liste d'adjacence qui représente un graphe orienté à n sommets*)
(* on les numérotes de 0 à n-1 et on suppose n pré-défini *)
(* le premier entier du couple est le sommet d'arrivé, le deuxième entier est le coût. *)
(* les requetes seront des couples (depart,arrivee) *)
type ladj = (int*int) list array

let print_graph ladj =
  Printf.printf "[|";
  Array.iteri (fun i l -> List.iter (fun (y,p) -> Printf.printf "(%i, %i) " y p) l; Printf.printf "\n") ladj;
  Printf.printf "|]"
let print_requetes l =
  List.iteri (fun x,y -> Printf.printf "(%i,%i) " x y) l; Printf.printf "\n"

let _ =
  let n = read_int() in (* n vertices *)
  let n_edges = read_int() in
  let n_requetes = read_int() in

  let ladj = Array.make n [] in
  (* read and add edges *)
  for i = 0 to n_edges-1 do
    let [x;y;p] =
    read_line()
    |> String.split_on_char ' '
    |> List.map int_of_string
    in
    ladj.(x-1) <- (y-1,p) :: ladj.(x-1)
  done;
  let requetes = List.init n_requetes (fun _ -> Scanf.scanf "%d %d " (fun depart,arrivee -> (depart-1,arrivee-1)) )
