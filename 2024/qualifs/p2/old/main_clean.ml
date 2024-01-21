let compare_fst x y = compare (fst x) (fst y)

let build_pos's (l: int list) : 'a array =
  let len = List.length l in
  List.combine l (List.init len Fun.id)
  |> List.stable_sort compare_fst
  |> List.map snd
  |> Array.of_list

let sortable n i i' k : bool = (* i = i' || *)
  let foi = float_of_int in
  let n, i, i', k = foi n, foi i, foi i', foi k in
  let p   = (i'-.i) /. k
  and binf= -.i /. k
  and bsup= (n-.1.-.i) /. k in
  binf <= p && p <= bsup
  && (p |> int_of_float |> foi) = p

let solve (k: int) (n: int) (l: int list) : string =
  let pos's = build_pos's l in
  if List.for_all (fun i -> sortable n i pos's.(i) k) (List.init n Fun.id)
  then "OUI"
  else "NON"

let _ =
  let k = read_int()
  and n = read_int() in
  let l =
    read_line()
    |> String.split_on_char ' '
    |> List.map int_of_string
  in
  Printf.printf "%s" (solve k n l);
