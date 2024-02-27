let lol' letters =
  let res = ref [] in
  List.iter (fun b -> List.iter (fun a -> res := (b@a@b)::!res) letters) letters;
  !res

let _ =
  let l = List.init 100 (fun i -> string_of_int i |> String.to_seq |> List.of_seq) in
  lol' l
