let lis l =
  let t = Array.of_list l in
  let len = Array.length t in
  let lis = Array.make len (-1) in (* lis de [i;len-1] *)

  let rec dfs is_starter i maxi acc =
    (* le booléen "is_starter" permet de traquer la branche d'exploration du dfs comportant les cas où l'on choisi de ne prendre à chaque étape aucun élement de la liste fibres.*)
    if i = len then acc
    else if lis.(i) <> -1 then lis.(i)
    else
      let lisi =
        if maxi > t.(i) then
          dfs is_starter (i+1) maxi acc
        (* attention à l'ordre d'appel des dfs dans max *)
        else max (dfs is_starter (i+1) maxi acc) (dfs false (i+1) t.(i) (acc+1))
      in
      (* Printf.printf "lis%i = %i\n" i lisi; *)
      if is_starter then lis.(i) <- max lisi lis.(i);
      lisi
  in
  dfs true 0 0 0

let urgence_reseau n fibres =
  let l = List.map snd fibres in
  lis l


let _ =
  let n = read_int() in
  let fibres = List.init n (fun _ -> Scanf.scanf "%i %i " (fun x y -> (x,y))) in
  urgence_reseau n (List.sort (fun x y -> if fst x <> fst y then compare (fst x) (fst y) else compare (snd x) (snd y)) fibres)
  |> Printf.printf "%i"
