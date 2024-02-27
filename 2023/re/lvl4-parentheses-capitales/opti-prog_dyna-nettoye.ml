let n = read_int ()
let parenthesesOuvrantes = List.init n (fun _ -> read_line ())
let parenthesesFermantes = List.init n (fun _ -> read_line ())
let q = read_int ()
let programmes = List.init q (fun _ -> read_line ())

let lpars = Hashtbl.create 128
let _ = List.iteri (fun i par -> Hashtbl.add lpars par i) parenthesesOuvrantes
let rpars = Array.of_list parenthesesFermantes

let sub_string s i j =
  String.init (j-i) (fun c -> s.[i+c])

let prog_valide (prog: string) : bool =
  let len = String.length prog in
  (* on accède aux élements du tableau avec ss_progs.(i).(j) avec i et j exclus *)
  let ss_progs = Array.make_matrix (len+1) (len+1) "" in

  (* updates ss_progs in O(|len|^3) *)
  for i = 0 to len do
    for j = i to len do
      ss_progs.(i).(j) <- sub_string prog i j
    done
  done;

  let cache = Hashtbl.create 256 in
  let rec dp a b =
    match Hashtbl.find_opt cache (a, b) with
    | Some v -> v
    | None ->
    let add_to_cache =
      if a = b then true
      else begin
        (* is_concat *)
        let rec loop i =
          if i = b then false
          else
            (dp a i && dp i b)
            || loop (i+1)
        in
        loop (a+1)
      end
           ||
           (* is_prog_parenthésé *)
           let rec loop i =
             if i > b then false else
               let rec loop' j =
                 if j > b then false
                 else
                   match Hashtbl.find_opt lpars ss_progs.(a).(i) with
                   | None -> false
                   | Some i_rpar ->
                     (rpars.(i_rpar) = ss_progs.(j).(b) && dp i j)
                     ||
                     loop' (j+1)
               in
               loop' i
               || loop (i+1)
           in
           loop a
      in
      Hashtbl.add cache (a, b) add_to_cache;
      add_to_cache
    in
    dp 0 len

let affiche l =
  let len = List.length l in
  List.iteri (fun i b -> print_string (if b then "VALIDE" else "INVALIDE"); if i < len-1 then print_newline() ) l

let _ =
  programmes
  |> List.map prog_valide
  |> affiche