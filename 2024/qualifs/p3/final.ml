type maillon = {
  v : string;
  mutable prec : chaine;
  mutable suiv : chaine;
}

and chaine = Vide | Liste of maillon

let sens = ref true

let list_of_chaine n s =
  let cache = Hashtbl.create n in
  let rec aux = function
    | Vide -> []
    | Liste s ->
      if Hashtbl.mem cache s.v
      then []
      else begin
        Hashtbl.add cache s.v true;
        s.v :: (aux (if !sens then s.suiv else s.prec))
      end
  in
  aux s

let get_maillon = function
  | Vide    -> failwith "get_maillon: chaine vide"
  | Liste m -> m

let init_chaine l =
  let c = ref Vide in
  let premier = ref Vide in
  List.iter (fun x ->
      let suiv = Liste {v= x; prec= !c; suiv= Vide} in
      (match !c with
       | Vide    -> premier := suiv;
       | Liste s -> s.suiv  <- suiv);
      c := suiv
    ) l;
  (get_maillon !premier).prec <- !c      ;
  (get_maillon !c      ).suiv <- !premier;
  !premier

let avance = function
  (* on renvoie le suivant *)
  | Vide    -> failwith "avance: chaine vide"
  | Liste s ->
    if !sens then
      s.suiv
    else
      s.prec

let estomac = ref []

let manger = function
  | Vide -> failwith "manger: chaine vide"
  | Liste s ->
    if !sens then begin
      (get_maillon s.suiv).prec <- s.prec;
      (get_maillon s.prec).suiv <- s.suiv;
      estomac := s.v :: !estomac;
      s.suiv
      end
    else
      begin
      (get_maillon s.suiv).prec <- s.prec;
      (get_maillon s.prec).suiv <- s.suiv;
      estomac := s.v :: !estomac;
      s.prec
    end

let retourner s =
  if !sens then sens := false
  else sens := true;
  avance s

let crache = function
  (* on se dit qu'on veut recracher en tete de liste *)
  | Vide -> failwith "crache: chaine vide"
  | Liste s ->
    let crachat =
      match !estomac with
      | [] -> failwith "crachat: estomac vide :-( "
      | h::t -> estomac := t; h
    in
    if !sens then begin
      (* vérifié ! *)
      let new_m = Liste {v= crachat; prec= s.prec; suiv= Liste s} in
      (get_maillon s.prec).suiv <- new_m;
      s.prec <- new_m;
      new_m
    end
    else begin
      (* corrigé - TODO *)
      let new_m = Liste {v= crachat; prec= Liste s; suiv= s.suiv} in
      (get_maillon s.suiv).prec <- new_m;
      s.suiv <- new_m;
      new_m
      end

let act s =
  function
  | 'A' -> s := avance !s
  | 'M' -> s := manger !s
  | 'R' -> s := retourner !s
  | 'C' -> s := crache !s (* crache !s*)
  | _   -> failwith "act: unrecognized action"

let solve n villes m actions =
  let serpent = ref (init_chaine villes) in
  List.iter (act serpent)  actions;
  list_of_chaine n !serpent

let print l = (String.concat "\n" l) |> Printf.printf "%s"

let _ =
    let n = read_int()
    and m = read_int() in
    let villes = List.init n (fun _ -> read_line())
    and actions= String.fold_left (fun l x -> x::l) [] (read_line()) |> List.rev in
    solve n villes m actions
    |> print
