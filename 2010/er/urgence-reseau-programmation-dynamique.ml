(* - affichages chelous (10, 62) et bug dans le terminal *)

let print_int_list l = Printf.printf "[";List.iteri(fun i x -> print_int x; if i < List.length l -1 then Printf.printf " ") l ; Printf.printf "]\n"
let print_int_array t = Printf.printf "[|";Array.iteri(fun i x -> print_int x; if i < Array.length t -1 then Printf.printf ", ") t; Printf.printf "|]\n"

let suivi = fun l -> print_int_list l; l

let lis l =
  let t = Array.of_list l in
  let len = Array.length t in
  let lis = Array.make len (-1) in
  lis.(len-1) <- 1;
  for i = len-2 downto 0 do
    for j = i+1 to len-1 do
      if t.(j) >= t.(i) then
        lis.(i) <- max (1+lis.(j)) lis.(i);
    done;
  done;
  Array.fold_left max 0 lis

let rec lis' l =
  let t = Array.of_list l in
  let len = Array.length t in
  let cache = Array.make len (-1) in
  let rec aux_lis i : int =
    if i = len then 1
    else if cache.(i) <> -1 then cache.(i)
    else
      begin
      List.init (len) Fun.id
      (* |> suivi *)
      |> List.filter (fun j -> j > i && t.(j) >= t.(i))
      (* |> suivi *)
      |> List.map (fun x -> 1 + aux_lis x)
      (* |> suivi *)
      |> List.fold_left max 1
      |> (fun x -> cache.(i) <- max x cache.(i); x)
      (* |> Int.add 1 *)
      end
  in
  List.init (len) Fun.id
  |> List.map aux_lis
  |> List.fold_left max 1

let lis''' l =
  let t = Array.of_list l in
  let len = Array.length t in
  let lis = Array.make len (1) in
  for i = len-2 downto 0 do
    for j = i+1 to len-1 do
      if t.(i) <= t.(j) then
        lis.(i) <- max lis.(i) (1+lis.(j))
    done;
  done;
  Array.fold_left max 0 lis

  
  let urgence_reseau n fibres =
    let l = List.map snd fibres in
    lis' l

let _ =
  let n = read_int() in
  let fibres = List.init n (fun _ -> Scanf.scanf "%i %i " (fun x y -> (x,y))) in
  urgence_reseau n (List.sort (fun x y -> if fst x <> fst y then compare (fst x) (fst y) else compare (snd x) (snd y)) fibres)
  |> Printf.printf "%i"
