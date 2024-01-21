(* NOTE:
- same code as the naive one, except for the calculation of the diameter.

- we don't had to calculate minimums spanning tree of composantes, since entry is already a tree ..
 *)

(*
- diameter calculation :
O(|A|) because only two consecutive dfs

proof of correctness in README.md
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
let dfs (f: int-> int-> unit) (g: graph) (x: int) =
  let nvertices = Array.length g in
  let mark = Array.make nvertices false in
  let rec loop (dist: int) (x: int) =
    if mark.(x) then ()
    else
      begin
        f dist x;
        mark.(x) <- true;
        List.iter (loop (dist+1)) g.(x)
      end
  in
  loop 0 x

let find_a_vertex g =
  (* quite bancale but works here (the point is that we build a graph where each vertex does not appear ie there is empty list in the ladj) *)
  try
    let rec loop i =
      match g.(i) with
        [] -> loop (i+1)
      | h::_ -> h
    in
    loop 0
  with Invalid_argument _ -> 0 (* deal with the case of a single vertex with no edges *)

let diameter g =
  let max_dist = ref 0 in
  let a = ref (find_a_vertex g) in
  let get_max_dist dist a' =
    if dist > !max_dist then
      begin
        a := a';
        max_dist := dist
      end
  in
  dfs get_max_dist g !a;
  dfs get_max_dist g !a; (* may seems ugly, this is CP *)
  !max_dist

let len_optimal_path g =
  let nvertices = Array.map List.length g |> Array.fold_left (+) 0 in
  let max_dist = diameter g in
  (* On an optimal global path, the edges on the max_dist_path will be used once, other edges two times. *)
  (nvertices / 2) * 2 - max_dist
(* ================================================================ *)


(* ============================ MAIN ============================== *)

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

(* ================================================================ *)

let _ =
  let len_s, len_a = Scanf.scanf "%i %i " (fun x y -> x, y) in
  let a = List.init len_a (fun _ -> Scanf.scanf "%i %i " (fun x y -> x, y)) in
  solve len_s len_a a
  |> (fun (x, y) -> Printf.printf "%i %i" x y)
