let can_order_family k n sizes =
  let sorted_sizes = Array.copy sizes in
  Array.sort compare sorted_sizes;
  let rec helper i =
    if i = n then true
    else if sizes = sorted_sizes then true
    else if i + k >= n then false
    else if sizes.(i) = sorted_sizes.(i) && sizes.(i + k) = sorted_sizes.(i + k) then
      helper (i + 1)
    else
      let temp = sizes.(i) in
      sizes.(i) <- sizes.(i + k);
      sizes.(i + k) <- temp;
      helper (i + 1)
  in
  helper 0

let solve k n l = if can_order_family k n (Array.of_list l) then "OUI" else "NON"
(* let () = *)
(*   let k = read_int () in *)
(*   let n = read_int () in *)
(*   let sizes = Array.init n (fun _ -> read_int ()) in *)
(*   if can_order_family k n sizes then *)
(*     print_endline "OUI" *)
(*   else *)
(*     print_endline "NON" *)
