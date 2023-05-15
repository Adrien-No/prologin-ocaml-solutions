let () =
  let films = Array.init 6 (fun _ -> read_line())
  and isnt_detested = Array.make 6 true in
  for i = 0 to 5 do
    let det = read_line() in
    films |> Array.iteri (fun i f -> if f = det then isnt_detested.(i) <- false);
  done;
  Printf.printf "%i" (Array.fold_left (fun b v -> if v then b+1 else b) 0 isnt_detested)
