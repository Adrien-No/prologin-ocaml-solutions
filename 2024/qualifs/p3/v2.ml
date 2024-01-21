let print_string_list (l:string list) : unit =
  print_string "[";
  let rec aux (l:string list) =
    match l with
    | [] -> print_string "]"
    | t::[] -> (Printf.printf "%s]\n" t)
    | t::q -> (Printf.printf "%s; " t; aux q)
  in aux l

type maillon = {
  v : string;
  mutable prec : chaine;
  mutable suiv : chaine;
}

and chaine = Vide | Liste of maillon

let list_of_chaine n s =
  let cache = Hashtbl.create n in
  let rec aux = function
  | Vide -> []
  | Liste s ->
    if Hashtbl.mem cache s.v
    then []
    else begin
      Hashtbl.add cache s.v true;
      s.v :: (aux s.suiv)
    end
  in
  aux s

let get_maillon = function
  | Vide    -> failwith "get_maillon: chaine vide"
  | Liste m -> m

let init_chaine0 (l: string list) : chaine =
  (* on suppose la liste non vide *)
  let c = ref (Liste {v= (List.hd l); prec= Vide; suiv= Vide}) in
  let premier = !c in
  List.iter (fun x ->
      let suiv= Liste {v= x; prec= Vide; suiv= Vide} in
      (get_maillon suiv).prec <- !c;
      (get_maillon !c).suiv <- suiv;
      c := suiv;
    ) l;
  (get_maillon premier).prec <- !c;
  (get_maillon !c).suiv <- premier;
  premier

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

let sens = ref true

let avance = function
  (* on renvoie le suivant *)
  | Vide    -> failwith "avance: chaine vide"
  | Liste s ->
    if !sens then
      s.suiv
    else
      s.prec

let estomac = ref []

let mange = function
  (* on renvoie le même maillon qui pointe maintenant sur le suivant du suivant *)
  | Vide    -> failwith "mange: chaine vide"
  | Liste s ->
    if !sens then begin
      estomac := (get_maillon s.suiv).v :: !estomac;
      s.suiv <- (get_maillon s.suiv).suiv;
      (get_maillon (get_maillon s.suiv).suiv).prec <- Liste s;
      Liste s
    end
    else begin
      estomac := (get_maillon s.prec).v :: !estomac;
      s.prec <- (get_maillon s.prec).prec;
      (get_maillon (get_maillon s.prec).prec).suiv <- Liste s;
      Liste s
    end
(* (get_maillon s.prec).suiv <- s.suiv; *)
(* (get_maillon s.suiv).prec <- s.prec; *)
(* estomac := s.v :: !estomac; *)
(* s.prec *)

let retourner s =
  if !sens then sens := false
  else sens := true;
  s

let crache = function
  | Vide    -> failwith "crache: chaine vide"
  | Liste s ->
    let crachat =
    match !estomac with
    | [] -> failwith "crachat: estomac vide :-( "
    | h::t -> estomac := t; h
    in
    let new_m = Liste {v= crachat; prec= Liste s; suiv= s.suiv} in
    s.suiv <- new_m;
    (get_maillon s.suiv).prec <- new_m;
    Liste s

let act s = fun l -> print_string_list (list_of_chaine 10 s); l |> function
  | 'A' -> avance    s
  | 'M' -> mange     s
  | 'R' -> retourner s
  | 'C' -> crache    s
  | _   -> failwith "act: unrecognized action"

let solve n villes m actions =
  Printf.printf "actions: prem: %c, suite: %s\n" (List.hd actions) (List.fold_left (fun s c -> s ^ (Char.escaped c) ) "" actions);
  List.fold_left act (init_chaine villes) actions
  |> list_of_chaine n


let print l = (String.concat "\n" l) |> Printf.printf "%s"

let _ =
  let n = read_int()
  and m = read_int() in
  let villes = List.init n (fun _ -> read_line())
  and actions= String.fold_left (fun l x -> x::l) [] (read_line()) |> List.rev in
  solve n villes m actions
  |> print
