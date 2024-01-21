(* see README for recursive formula *)

Printexc.record_backtrace true

type dist = Infinity | Int of int
type graph = int array array


let get (d: dist) =
  match d with
    Infinity -> failwith "can't get infinity"
  | Int i    -> i

let dmin (d: dist) (d': dist) =
  match d, d' with
    Infinity , d' -> d'
  | d , Infinity  -> d
  | Int i, Int i' -> Int (min i i')

let (+++) (d: dist) (d': dist) : dist =
  match d, d' with
    Infinity, _ -> Infinity
  | _, Infinity -> Infinity
  | Int i, Int i' -> Int (i + i')

let get_graph nvertices edges =
  let g = Array.make_matrix nvertices nvertices max_int in
  List.iter (fun (src, dst, w) -> g.(src).(dst) <- min w g.(src).(dst)) edges;
  g

let floyd_warshall (nvertices: int) (g: graph) : graph =
  for k = 0 to nvertices-1 do
    for i = 0 to nvertices-1 do
      for j = 0 to nvertices-1 do
        let v' = g.(i).(k) + g.(k).(j) in (* works because of problem specifications *)
        if v' >= 0 then g.(i).(j) <- min g.(i).(j) v'
      done
    done
  done;
  g

(* ============================ MAIN ============================== *)

let solve nvertices nedges nqueries edges queries =
  let g = get_graph nvertices edges in
  let g = floyd_warshall nvertices g in

  List.map (fun (a, b) -> g.(a).(b)) queries
  |> List.iteri (fun i res -> print_int res; if i <> nqueries-1 then print_newline())

(* ================================================================ *)

let _ =
  let nb_s, nb_a, nb_query = Scanf.scanf "%i %i %i " (fun x y z -> x, y, z) in
  let a = List.init nb_a (fun _ -> Scanf.scanf "%i %i %i " (fun x y z -> x-1, y-1, z)) in
  let queries = List.init nb_query (fun _ -> Scanf.scanf "%i %i " (fun x y -> x-1, y-1)) in
  solve nb_s nb_a nb_query a queries
