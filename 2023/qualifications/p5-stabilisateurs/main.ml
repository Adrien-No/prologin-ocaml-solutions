(* let cached f x y = *)
(*   let cache = Hashtbl.create 64 in *)
(*   let rec f' x y = *)
(*     match Hashtbl.find_opt cache (x, y) with *)
(*     | Some v -> v *)
(*     | None -> *)
(*       let v = f' x y in *)
(*       Hashtbl.replace cache (x, y) (f v); *)
(*       v *)
(*   in *)
(*   f' x y *)

let stabiliteMaximale n0 k0 p accroches =
  let cache = Hashtbl.create 64 in
  let rec loop k n =
    match Hashtbl.find_opt cache (k, n) with
    | Some v -> v
    | None ->
      let v =
        if k <= 0 || n >= n0-3 then 0
        else
          max (loop k (n+1)) (loop (k-1) (n+4) + (accroches.(n+3)-accroches.(n) |> fun x -> p - x*x))
      in
      Hashtbl.add cache (k, n) v;
      v
  in
  loop k0 0

(* let stabiliteMaximale n0 k0 p accroches = *)
(*   let rec loop i k = *)
(*     if k <= 0 || i >= n0-3 then 0 else *)
(*       max (loop (i+1) k) (loop (i+4) (k-1) + (accroches.(i+3)-accroches.(i) |> fun x -> p - x*x)) *)
(*   in *)
(*   loop 0 k0 *)

let () =
  let n = read_int () in
  let k = read_int () in
  let p = read_int () in
  let accroches = read_line () |> fun x -> (if x = "" then [] else String.split_on_char ' ' x) |> List.map int_of_string |> List.sort compare |> Array.of_list in
  stabiliteMaximale n k p accroches |> Printf.printf "%i"
