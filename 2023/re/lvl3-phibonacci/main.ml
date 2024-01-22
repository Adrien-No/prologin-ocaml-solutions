
(**
   @param n la taille initiale de la machine
   @param x la machine à compacter
*)
let phibonacci n l =
  (** TODO Afficher la version la plus courte de la machine qui ne la casse
  pas. *)
  let rec compute l =
    match l with
      [] -> []
    | 0::q -> compute q
    | n::x::y::q -> (* Printf.printf "n= %i, x= %i, y= %i\n" n x y; *) compute (((x+n) mod 1000000007)::((y+n) mod 1000000007)::q)
    | _ -> List.rev l

  in
  let l = compute (List.rev l) in
  let len = List.length l in
  l
  |> List.map (fun x -> x mod 1000000007)
  |> List.iteri (fun i x -> print_int x; if i < len-1 then print_char ' ')

let () =
  let n = read_int () in
  let x = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev in
  phibonacci n x
