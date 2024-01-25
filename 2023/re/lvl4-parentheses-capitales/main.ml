let affiche l =
  let len = List.length l in
  List.iteri (fun i b -> print_string (if b then "VALIDE" else "INVALIDE"); if i < len-1 then print_newline() ) l

let phrasesValides n parOuvs parFers q programmes =

  let rec parse_prog (prog: char list) : bool =
    (* lit  *)
    match prog with
    | [] -> true
    | _  ->
      let suites = parse_ouvrantes prog in (* on veut tester si une de ces suites permet d'obtenir un parenthésage valide *)
      List.exists parse_fermante suites

  and parse_ouvrantes (prog: char list) : (int * char list) list =
    (* Renvoie les (i, q) tq prog = pᵢq avec pᵢ la i-ème ParOuv *)
    let rec check_par prog (i_par: int) par : (int * char list) option =
      match (prog, par) with
      | _, [] -> Some (i_par, prog)
      | h::q, h'::q' when h = h' -> check_par q i_par q'
      | _ -> None
    in
    let rec apply i parOuvs acc =
      match parOuvs with
      | [] -> acc
      | parOuv::q ->
        match check_par prog i parOuv with
        | None -> apply (i+1) q acc
        | Some (i, par) -> apply (i+1) q ((i, par)::acc)
    in
    apply 0 parOuvs []

  and parse_fermante (i_par, prog) : bool =
    let parF = parFers.(i_par) in (* la ParFer qu'on cherche à reconnaître *)
    let rec check_par prog par =
      match (prog, par) with
      | _, [] -> Some prog
      | h::q, h'::q' when h = h' -> check_par q q'
      | _ -> None
    in
    let rec iter_fermantes (avant_prog: char list) (prog: char list) (acc : (char list * char list) list)  =
      (* les élements de acc sont les (avant, apres) tq prog = avant^ParFer^apres *)
      match prog with
      | [] -> acc
      | c::q ->
        match check_par prog parF with
        | None -> iter_fermantes (c::avant_prog) q acc
        | Some apres -> iter_fermantes (c::avant_prog) q ((List.rev avant_prog, apres)::acc)
    in
    let fermantes = iter_fermantes [] prog [] in (* les fermantes avec leurs contexte *)
    List.exists (fun (avant, apres) -> parse_prog avant && parse_prog apres) fermantes
  in
  List.map parse_prog programmes
  |> affiche

let () =
  let n = read_int () in
  let parenthesesOuvrantes = List.init n (fun _ -> read_line () |> String.to_seq |> List.of_seq) in
  let parenthesesFermantes = Array.init n (fun _ -> read_line () |> String.to_seq |> List.of_seq) in
  let q = read_int () in
  let programmes = List.init q (fun _ -> read_line () |> String.to_seq |> List.of_seq) in
  phrasesValides n parenthesesOuvrantes parenthesesFermantes q programmes
