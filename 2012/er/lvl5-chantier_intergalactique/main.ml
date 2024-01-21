type graph = (int * int ) list array (* ladj with (dst, weight) *)

let get_graph (nb_s: int) (a: (int*int*int) list) : graph =
  let g = Array.make nb_s [] in
  List.iter (fun (x, y, w) -> g.(x) <- (y, w)::g.(x); g.(y) <- (x, w)::g.(y)) a;
  g

let prim_alg nb_s g =
  let mark = Array.make nb_s false in

  let find_candidate (s: int list) : int* int =
    let candidates = List.map (fun x -> g.(x)) s |> List.concat |> List.filter (fun (s, _) -> not mark.(s)) in

    List.fold_left (fun acc cand -> if snd cand < snd acc then cand else acc) (List.hd candidates) candidates
  in

  let rec loop s marked sum_road =
    if marked = nb_s then sum_road
    else
      begin
        let s', road' = find_candidate s in
        mark.(s') <- true;
        loop (s'::s) (marked+1) (road'+sum_road)
      end
  in
  mark.(0) <- true;
  loop [0] 1 0

let chantier_intergalactique p r routes =
    let g = get_graph p routes in
    prim_alg p g

let _ =
    let p = read_int () in
    let r = read_int () in
    let routes = List.init r (fun _ -> Scanf.scanf "%i %i %i " (fun x y z -> (x, y, z))) in

    chantier_intergalactique p r routes
    |>  Printf.printf "%i"
