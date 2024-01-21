(*
- On va d'abord trouver le plus grand sous-graphe :
    On va construire une collection (majorée par S) d'ensembles disjoints où les élements d'un ensemble sont exactement ceux reliés entre-eux.
- Puis trouver le nombre minimum de mouvements à effectuer pour le parcourir

*)

type graph = int list array

let get_graph nb_s a =
  let g = Array.make nb_s [] in
  List.iter (fun (a, b) -> g.(a) <- b::g.(a); g.(b) <- a::g.(b)) a;
  g

(* =========================== composantes ======================== *)

let dfs (f: int-> int-> unit) (f': unit-> unit) (g: graph) =
  (* calls f on each vertices of g, *)
  (* calls f' before each new dfs   *)
  let nvertices = Array.length g in
  let mark = Array.make nvertices false in
  let rec loop (old_x: int) (x: int) =
    if mark.(x) then ()
    else
      begin
        f old_x x;
        mark.(x) <- true;
        List.iter (loop x) g.(x)
      end
  in
  List.iter (fun x -> f'(); loop x x) (List.init nvertices Fun.id)

let get_composantes g =
  let comp = Stack.create() in
  let spanning_tree = Stack.create() in
  (** increase the actual composante *)
  let extend_comp (old_x: int) (x: int) =
    if x <> old_x then Stack.push (old_x, x) spanning_tree;
    Stack.push x comp
  in

  let comps = Stack.create() in
  (** save the actual composante (completly found), and prepare for searching others. *)
  let add_comp() =
    Stack.push (Stack.copy comp, Stack.copy spanning_tree) comps;
    Stack.clear comp;
    Stack.clear spanning_tree
  in

  dfs extend_comp add_comp g;
  add_comp();
  comps

(** Returns the vertex of biggest composantes with a max spanning tree for each of them. *)
let get_biggest_comps g : (int list * (int * int) list) list =
  let ss = get_composantes g in

  let choose_comp (lacc, size) s =
    let size' = Stack.length (fst s) in
    if size' > size then
      ([s], size')
    else if size' = size then
      (s::lacc, size)
    else
      (lacc, size)
  in
  let l = (Stack.fold choose_comp ([Stack.create(), Stack.create()], 0) ss) in
  let los s = s |> Stack.to_seq |> List.of_seq in
  List.map (fun (s, s') -> los s, los s') (fst l)

(* ================================================================ *)


(* ========================= minimal path ========================= *)
exception Trouve
let bfs (f: int -> bool) (f': int->unit) (g: graph) (x: int) =
  let mark = Array.make (Array.length g) false in
  let s = Queue.create() in
  Queue.push x s;
  try
    while not (Queue.is_empty s) do
      let y = Queue.pop s in
      if not mark.(y) then
        begin
          Printf.printf "sommet actuel: %i\n" y;
          if f y then raise Trouve;
          mark.(y) <- true;
          let suivs = List.filter (fun x -> not mark.(x)) g.(y) in
          List.iter (fun x -> Queue.push x s) suivs;
          f' (List.length suivs)
        end

    done
  with Trouve -> Printf.printf "\n"

let extremities g =
  List.init (Array.length g) Fun.id
  |> List.filter (fun i -> List.length g.(i) = 1)

let most_opposed_vertices g =
  (* i think that we don't need the best candidates, just the len of path, then we calculate with the next function *)
  let vertices = extremities g |> Array.of_list in
  let len = Array.length vertices in
  let max_dist = ref 0 in
  let best_candidates = ref (-1, -1) in

  for i = 0 to len-1 do
    for j= i+1 to len-1 do

      let compteur = ref 0 in
      let f' width =
        if width = 0 then decr compteur
        else compteur := !compteur + width
      in
      let dist = ref 0 in
      bfs ((=)vertices.(j)) f' g vertices.(i);
      Printf.printf "dist= %i\n" !dist;
      Printf.printf "compteur= %i\n" !compteur;

      if !dist > !max_dist then
        begin
          max_dist := !dist;
          best_candidates := (vertices.(i), vertices.(j))
        end
    done
  done;
  !max_dist
(* !best_candidates *)

let len_optimal_path g =
  let nvertices = Array.map List.length g |> Array.fold_left (+) 0 in
  Printf.printf "nvertices: %i\n" nvertices;
  let max_dist = most_opposed_vertices g in
  Printf.printf "max_dist: %i\n" max_dist;
  (* On an optimal global path, the edges on the max_dist_path will be used once, other edges two times. *)
  (nvertices / 2) * 2 - max_dist
(* ================================================================ *)


let solve nb_s nb_a a =
  let g = get_graph nb_s a in

  let comps = get_biggest_comps g in

  let comp_min_path (_, a) = (* len of optimal path for a given composante *)
    let g = get_graph nb_s a in
    len_optimal_path g
  in

  let salles_visitees = List.hd comps |> fst |> List.length in
  salles_visitees,
  List.map comp_min_path comps
  |> List.fold_left min max_int

let _ =
  let len_s, len_a = Scanf.scanf "%i %i " (fun x y -> x, y) in
  let a = List.init len_a (fun _ -> Scanf.scanf "%i %i " (fun x y -> x, y)) in
  solve len_s len_a a
  |> (fun (x, y) -> Printf.printf "%i %i" x y)
