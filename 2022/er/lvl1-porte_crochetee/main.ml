let n = read_int ()
let serrures = (read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev) |> Array.of_list
let cles = (read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev) |> Array.of_list

let rab = ref 0
let manque = ref 0

let _ =
for i = 0 to n-1 do
  let delta = cles.(i) - serrures.(i) in

  if delta > 0 then (rab := !rab + (delta/2))
  else manque := !manque - delta;
  (* Printf.printf "delta= %i, rab= %i, manque= %i\n" delta !rab !manque; *)
done;

Printf.printf "%i" (max (!manque - !rab) 0)
