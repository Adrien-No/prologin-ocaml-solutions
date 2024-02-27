let n = read_int ()
let parenthesesOuvrantes = List.init n (fun _ -> read_line ())
let parenthesesFermantes = List.init n (fun _ -> read_line ())
let q = read_int ()
let programmes = List.init q (fun _ -> read_line ())

let lpars = Hashtbl.create 128
let _ = List.iteri (fun i par -> Hashtbl.add lpars par i) parenthesesOuvrantes
let rpars = Array.of_list parenthesesFermantes

let debug s  = if false then Printf.printf "%s" s
let debug2 s = if false then Printf.printf "%s" s

let print_int_array t = Array.iter (Printf.printf "%i ") t; print_newline()
let decalage n = String.make n ' '

let prog_valide (prog: string) : bool =
  let len = String.length prog in
  (* on accède aux élements du tableau avec ss_progs.(i).(j) avec i et j exclus *)
  let ss_progs = Array.make_matrix (len+1) (len+1) "" in

  let cache_ss = Array.make_matrix (len+1) (len+1) None in
  let sub_string s i j =
    match cache_ss.(i).(j) with
    | Some v -> v
    | None ->
      let v = String.init (j-i) (fun c -> s.[i+c])
      in
      cache_ss.(i).(j) <- Some v;
      v
  in
  (* updates ss_progs in O(|len|^3) *)
  for i = 0 to len do
    for j = i to len do
      ss_progs.(i).(j) <- sub_string prog i j
    done
  done;


let affiche l =
  let len = List.length l in
  List.iteri (fun i b -> print_string (if b then "VALIDE" else "INVALIDE"); if i < len-1 then print_newline() ) l

let _ =
  programmes
  |> List.map prog_valide
  |> affiche
