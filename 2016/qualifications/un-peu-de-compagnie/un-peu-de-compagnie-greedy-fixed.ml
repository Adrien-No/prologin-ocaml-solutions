
let print_couple_list l =
  Printf.printf "["; List.iteri (fun i (a,b) -> Printf.printf "(%i,%i)" a b; if i < List.length l then Printf.printf " ") l; Printf.printf "]\n"

type graphe =
  {
    n : int;
    s : int array;
    ladj : int list array;
  }

let create_graphe n edges =
  (* O (n + |edges|) *)
  let ladj = Array.make n [] in
  List.iter (fun (x, y) -> ladj.(x) <- y::ladj.(x); ladj.(y) <- x::ladj.(y)) edges;
  {
    n = n;
    s = Array.init n Fun.id;
    ladj = ladj;
  }

let aretes_complementaires n m l =
  (*  *)
  let cache = Hashtbl.create m in
  l |> List.iter (fun x -> Hashtbl.add cache x true);
  let res = ref [] in
  for i = 0 to n-1 do
    for j = i+1 to n-1 do
      if not (Hashtbl.mem cache (i,j)) then
        res := (i,j)::!res
    done;
  done;
  !res

let init_degrees g : int array =
  (* O(n + |A|) *)
  Array.map List.length g.ladj

let sorted_list (mark:bool array) (degrees:int array) : int list =
  (* renvoie la liste des numéros de sommets triés par degré croissant *)
  let n = Array.length mark in
  let rec get_couples i acc =
    if i = n then acc
    else get_couples (i+1) (if not mark.(i) then (i,degrees.(i))::acc else acc)
  in
  let couples = get_couples 0 [] in

  (* fst x le numéro, snd x le degré *)
  let comparec x y = compare (snd x) (snd y) in

  couples
  |> List.sort comparec
  |> List.map (fun (x,_) -> x)

let index_of_min l =
  let rec aux i i_min x l =
    match l with
      [] -> i_min
    | t::q -> if t < x then aux (i+1) i t q else aux (i+1) i_min x q
  in
  aux 0 (0) (List.hd l) l

(******************************************************************)
let is_free_vertex (x:int) (c:(int*int) list) =
  List.for_all (fun (a,b) -> a <> x && b <> x) c

let is_in_c (x,y) (c:(int*int) list) =
  (* [ AMELIORATION ] table de hachage*)
  List.mem (x,y) c

let is_augment_path (c: (int*int) list) (x,y) (path:(int*int) list) =
  is_free_vertex x c
  && List.for_all (fun (i,(x,y)) -> if i mod 2 = 0 then is_in_c (x,y) c else is_in_c (x,y) c |> not) (List.mapi (fun i x -> i,x) path)
  && is_free_vertex y c

let aretes_precedente (g:graphe) y =
  (* renvoie la liste des arêtes précedentes (de x -> y où y est fixé) *)
  (* [ TABLEAU DES PRECEDENTS ] *)

let augment_path (couplage:(int*int,bool) Hashtbl.t) (x,y) (path:(int*int) list) (mark:bool array): (int*int) list option =
  (* on suppose |path| impair *)

  (* test d'arête augmentante *)



(******************************************************************)

let main n m rivalites =
  let g = create_graphe n (aretes_complementaires n m rivalites)
  and mark = Array.make n false in
  let degrees = init_degrees g in

  let rec get_composante acc =
    (* actual_vertex are the vertex that aren't yet in acc (C) *)
    match sorted_list mark degrees with
      [] -> acc
    | x::q ->
      let y = List.fold_left (fun b x -> if degrees.(x) < degrees.(b) then x else b) (List.hd g.ladj.(x)) g.ladj.(x) in
      mark.(x) <- true ; mark.(y) <- true;
      degrees.(x) <- max_int; degrees.(y) <- max_int;
      get_composante ((x,y)::acc)
  in
  let v = get_composante [] in
  (*print_couple_list v;*)
  v |> List.length

let _ =
  let n = Scanf.scanf "%d " Fun.id in
  let m = Scanf.scanf "%d " Fun.id in
  let rivalites = List.init m (fun _i ->
    let ra, rb = Scanf.scanf "%d %d " (fun v_0 v_1 -> v_0, v_1) in
    (ra,rb)) in
  Printf.printf "%i" (main n m rivalites)
