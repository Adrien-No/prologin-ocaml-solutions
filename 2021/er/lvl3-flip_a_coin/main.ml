let nombreDePieces = read_int ()
let rangeeDePieces = List.init nombreDePieces (String.get (read_line ())) |> Array.of_list

(* cas particulier : toutes les pieces sont faces *)

let longest_from_i t i =
  let len = ref 0 in
  let chip = ref false in (* une fois, on peut continuer même si la pièce est à Pile *)
  while i + !len < nombreDePieces && (t.(i+ !len) = 'F' || (let res = not !chip in  chip := true; res)) do
    incr len
  done;
  !len

let longuest () =
  List.init nombreDePieces Fun.id
  |> List.map (longest_from_i rangeeDePieces)
  |> List.fold_left max 0

let _ =
  Printf.printf "%i" (longuest())
