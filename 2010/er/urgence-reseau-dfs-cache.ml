let lis l =
  (* longest increasing subsequence of l*)
  let t = Array.of_list l in
  let len = Array.length t in
  let lis = Array.make len (-1) in
  let rec dfs i =
    if i = len then 0
    else if lis.(i) <> -1 then lis.(i)
    else let lisi =

let urgence_reseau n fibres =
  let l = List.map snd fibres in
  lis l


let _ =
  let n = read_int() in
  let fibres = List.init n (fun _ -> Scanf.scanf "%i %i " (fun x y -> (x,y))) in
  urgence_reseau n (List.sort (fun x y -> if fst x <> fst y then compare (fst x) (fst y) else compare (snd x) (snd y)) fibres)
  |> Printf.printf "%i"
