let n = read_int ()
let parenthesesOuvrantes = List.init n (fun _ -> read_line ())
let parenthesesFermantes = List.init n (fun _ -> read_line ())
let q = read_int ()
let programmes = List.init q (fun _ -> read_line ())

let lpars = Hashtbl.create 128
let _ = List.iteri (fun i par -> Hashtbl.add lpars par i) parenthesesOuvrantes
let rpars = Array.of_list parenthesesFermantes

let prog_valide (prog: string) : bool =
  let len = String.length prog in

  let cache_sub_string = Array.make_matrix (len+1) (len+1) None in
  let sub_string a b =
    match cache_sub_string.(a).(b) with
    | Some v -> v
    | None ->
      let add_to_cache =
        String.init (b-a) (fun c -> prog.[a+c])
        (* String.sub prog a (b-a) *)
      in
      cache_sub_string.(a).(b) <- Some add_to_cache;
      add_to_cache
  in

  let cache = Array.make_matrix (len+1) (len+1) None in
  let rec dp a b =
    match cache.(a).(b) with
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
                   match Hashtbl.find_opt lpars (sub_string a i) with
                   | None -> false
                   | Some i_rpar ->
                    loop' (j+1) || (rpars.(i_rpar) = sub_string j b && dp i j)

               in
               loop (i+1) || loop' i
           in
           loop a
      in
      cache.(a).(b) <- Some add_to_cache;
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
