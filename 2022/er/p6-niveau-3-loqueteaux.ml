let estOuvrable n d loqueteaux =
  (* TODO Indiquer s'il est possible d'ouvrir la porte. *)

  let echange t i1 i2 =
    let v = t.(i1) in
    t.(i1) <- t.(i2);
    t.(i2) <- v
  in

  let move l =
    let res = Array.copy l in
    let f i x =
      if i < n/2 then (
        if x = 1 then echange res i (i+d)
      )
      else if x = 0 then echange res i (i-d)
    in
    Array.iteri f res;
    res
  in

  let rec moves l =
    let next_l = move l in
    if l = next_l then l else moves (next_l)
  in

  let l = moves loqueteaux in
  let res = ref true in
  let f2 i x =
    if i < n/2 then (
      if x = 1 then res := false
    )
    else if x = 0 then res := false
        in

  Array.iteri f2 l;
  !res

let () =
  let n = read_int () in
  let d = read_int () in
  let loqueteaux = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev in
  let l = loqueteaux |> Array.of_list in
  if estOuvrable n d l then Printf.printf "OUI" else Printf.printf "NON"
