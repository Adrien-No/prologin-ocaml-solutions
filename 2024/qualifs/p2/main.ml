
(** returns the k distincs classes of l*)
let get_classes k l =
  assert(k<>0);
  let classes = Array.make k [] in
  let rec aux i = function
    | []   -> ()
    | h::t -> classes.(i) <- h::classes.(i);
      aux (if i+1 = k then 0 else i+1) t
  in
  aux 0 l;
  Array.map List.rev classes

(** sort classes*)
let sort_classes t =
  Array.map (List.sort compare) t

let solve k l : bool =
  if k = 1 then true else
  let classes = get_classes k l |> sort_classes in
  let rec aux i i' = match classes.(i), classes.(i') with
    | [], [] -> true
    | [], _ -> failwith "problème de logique" (* shoulnd not happend *)
    | _, [] -> true (* classe i should have a len of 1 *)
    | h1::t1, h2::t2 ->
      h1 <= h2 &&
      begin
        classes.(i) <- t1;
        aux i' (if i'+1 = k then 0 else i'+1)
      end
  in
  aux 0 1

let _ =
  let k = read_int()
  and _ = read_int() in
  let l =
    read_line()
    |> String.split_on_char ' '
    |> List.map int_of_string
  in
  (if (solve k l) then "OUI" else "NON") |> Printf.printf "%s"
