Printexc.record_backtrace true

(*
- We implement Dijkstra algorithm to compute the shortest path between two vertex
- We maintain a hashtbl that saves calculated queries
- Each time we search dist between two vertex, we check if we don't already have the result

 *)

type graph = (int * int) list array

let get_graph (nb_s: int) (a: (int * int * int) list) : graph =
  let g = Array.make nb_s [] in
  List.iter (fun (src, dst, w) -> g.(src) <- (dst, w)::g.(src)) a;
  g



let dijkstra (g: graph) (dists: int array array) (a: int) (b: int) : int =
  (* let dists = Array.make nb_s max_int in (\* we could use a type option but slower to write, and specification says there is always a path from a to b in queries *\) *)
  let path = ref [] in
  let rec loop (ppath: int list) (dist: int) (x: int) =
    if dist < dists.(a).(x) then
      begin
        (* Printf.printf "dist.(%i)= %i <- %i\n" x dists.(x) dist; *)
        dists.(a).(x) <- dist;
        if x = b then path := ppath;
        List.iter (fun (y, w) -> loop (y::ppath) (dist+w) y) g.(x)
      end
  in
  loop [0] 0 a;
  List.iter (fun a' -> dists.(a').(b) <- min dists.(a).(b) dists.(a').(b)) !path;
  dists.(a).(b)

(* ============================ MAIN ============================== *)

(* let querie g nb_s a b = *)
(*   dijkstra g nb_s a b *)
let solve nb_s nb_a nb_query a queries =
  let g = get_graph nb_s a in
  let dist_matrix = Array.make_matrix nb_s nb_s max_int in
  List.map (fun (a, b) -> dijkstra g dist_matrix a b) queries
  |> List.iteri (fun i res -> print_int res; if i <> nb_query-1 then print_newline())

(* ================================================================ *)

let _ =
  let nb_s, nb_a, nb_query = Scanf.scanf "%i %i %i " (fun x y z -> x, y, z) in
  let a = List.init nb_a (fun _ -> Scanf.scanf "%i %i %i " (fun x y z -> x-1, y-1, z)) in
  let queries = List.init nb_query (fun _ -> Scanf.scanf "%i %i " (fun x y -> x-1, y-1)) in
  solve nb_s nb_a nb_query a queries
