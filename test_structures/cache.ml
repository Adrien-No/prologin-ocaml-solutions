let cached f x =
  let cache = Hashtbl.create 64 in
  let rec f' x =
    match Hashtbl.find_opt cache x with
    | Some v -> v
    | None ->
      let v = f x in
      Hashtbl.replace cache x (f v);
      v
  in
  f' x y

let fibo n =
