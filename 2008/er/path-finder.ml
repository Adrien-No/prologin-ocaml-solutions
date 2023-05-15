let print_int_array a = Printf.printf "[|"; Array.iteri (fun i x -> Printf.printf "%i" x; if i+1 < Array.length a then Printf.printf ", ") a; Printf.printf "|]\n"

let pathfinder size tab =
  (* print_int_array tab.(0); *)

  let f = Array.init size (fun i -> Array.copy tab.(i)) in

  for x = 0 to size-1 do
    for y = 0 to size-1 do
      match x,y with
        (0,0) -> ()
      | (x,0) -> f.(x).(0) <- f.(x).(0) + f.(x-1).(0)
      | (0,y) -> f.(0).(y) <- f.(0).(y) + f.(0).(y-1)
      | (x,y) -> f.(x).(y) <- max f.(x-1).(y) f.(x).(y-1) + f.(x).(y)
    done;
  done;
  f.(size-1).(size-1)

let _ =
  let in_int() = Scanf.scanf " %d" (fun x->x) in
  let n = in_int() in
  let tab = Array.init n (fun _ ->
      Array.init n (fun _ -> in_int()))
  in
  Printf.printf "%i" (pathfinder n tab
)
