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

let dfs (f: int-> unit) (f': unit-> unit) (g: graph) =
  (* calls f on each vertices of g, *)
  (* calls f' before each new dfs   *)
  let nvertices = Array.length g in
  let mark = Array.make nvertices false in
  let rec loop (x: int) =
    if mark.(x) then ()
    else
      begin
        f x;
        mark.(x) <- true;
        List.iter loop g.(x)
      end
  in
  List.iter (fun x -> f'(); loop x) (List.init nvertices Fun.id)

let get_composantes g =
  let comp = Stack.create() in
  let extend_comp (x: int) =
    Stack.push x comp
  in

  let comps = Stack.create() in
  let add_comp() =
    Stack.push (Stack.copy comp) comps;
    Stack.clear comp
  in

  dfs extend_comp add_comp g;
  add_comp();
  comps

let get_biggest_comps g : int list list =
  let ss = get_composantes g in

  let choose_comp (lacc, size) s =
    let size' = Stack.length s in
    if size' > size then
      ([s], size')
    else if size' = size then
      (s::lacc, size)
    else
      (lacc, size)
  in
  let l = (Stack.fold choose_comp ([Stack.create()], 0) ss) in
  List.map (fun s -> s |> Stack.to_seq |> List.of_seq) (fst l)

(* ================================================================ *)


let solve nb_s nb_a a =
  let g = get_graph nb_s a in


let _ =
  let len_s, len_a = Scanf.scanf "%i %i " (fun x y -> x, y) in
  let a = List.init len_a (fun _ -> Scanf.scanf "%i %i " (fun x y -> x, y)) in
  solve len_s len_a a
