(* TODO: corriger cracher (un seul sens qui marche mal ?) *)
(* TODO: question : est-ce que quand on mange à l'envers ça s'ordonne différemment ? *)
(* TODO: finir la 2e version (!sens = false) de manger, celle de cracher, continuer les tests et tester sur la vraie entrée *)
 (* RECAP des erreures : *)

(* - pour une liste chainée, attention à l'ordre d'attribution des pointeurs *)
(* (e.g si on ajoute un maillon, le suivant du prec devient ce maillon donc on a plus la donnée de l'ancien suiv du prec.) *)

(* - si se retourner c'est juste changer la variable de sens, il faut aussi avancer d'un ensuite. *)

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

let avance = function
  (* on renvoie le suivant *)
  | Vide    -> failwith "avance: chaine vide"
  | Liste s ->
    if !sens then
      s.suiv
    else
      s.prec

let estomac = ref []

let manger0 : chaine -> chaine = function
  (* on renvoie le même maillon qui pointe maintenant sur le suivant du suivant *)
  | Vide    -> failwith "mange: chaine vide"
  | Liste s ->
    if !sens then begin
      (get_maillon s.prec).suiv <- s.suiv;
      (get_maillon s.suiv).prec <- s.prec;
      estomac := s.v :: !estomac;
      s.suiv
    end
    else begin
      (get_maillon s.suiv).prec <- s.prec;
      (get_maillon s.prec).suiv <- s.suiv;
      estomac := s.v :: !estomac;
      s.prec
    end

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
        (* TODO *)
      let tmp = get_maillon s.prec in
      (get_maillon tmp.prec).suiv <- Liste s;
      s.prec <- tmp.prec;
      estomac := tmp.v :: !estomac;
      Liste tmp
      end
(* ====== version où on opère sur le maillon suivant ===== *)
(* if !sens then begin *)
(*   estomac := (get_maillon s.suiv).v :: !estomac; *)
(*   s.suiv <- (get_maillon s.suiv).suiv; *)
(*   (get_maillon (get_maillon s.suiv).suiv).prec <- Liste s *)
(*   (\* Liste s *\) *)
(* end *)
(* else begin *)
(*   estomac := (get_maillon s.prec).v :: !estomac; *)
(*   s.prec <- (get_maillon s.prec).prec; *)
(*   (get_maillon (get_maillon s.prec).prec).suiv <- Liste s *)
(*   (\* Liste s *\) *)
(* end *)


let retourner s =
  if !sens then sens := false
  else sens := true;
  avance s
(* s *)

let crache0 : chaine -> unit = function
  | Vide    -> failwith "crache: chaine vide"
  | Liste s ->
    let crachat =
      match !estomac with
      | [] -> failwith "crachat: estomac vide :-( "
      | h::t -> estomac := t; h
    in
    if !sens then begin
      let new_m = Liste {v= crachat; prec= Liste s; suiv= s.suiv} in
      s.suiv <- new_m;
      (get_maillon s.suiv).prec <- new_m
    end
    else begin
      let new_m = Liste {v= crachat; prec= s.prec; suiv= Liste s} in
      s.prec <- new_m;
      (get_maillon s.prec).suiv <- new_m
    end

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
      (* TODO *)
      let new_m = Liste {v= crachat; prec= Liste s; suiv= s.suiv} in
      s.suiv <- new_m;
      new_m
      end

let act s =
   (*fun l -> print_string_list (let v = list_of_chaine 10 !s in if !sens then v else (List.rev v)); l |>*)
  function
  | 'A' -> s := avance !s
  | 'M' -> s := manger !s
  | 'R' -> s := retourner !s
  | 'C' -> s := crache !s (* crache !s*)
  | _   -> failwith "act: unrecognized action"

let test() =

  let s_equal_l s l =
    assert(list_of_chaine (List.length l) !s = l)
  in
  (* s est le serpent *)
  (* ==== init ==== *)
  let s = ref (init_chaine ["0"; "1"; "2"]) in
  s_equal_l s ["0"; "1"; "2"];

  (* ==== avance ==== *)
  act s 'A'; s_equal_l s ["1"; "2"; "0"];
  act s 'A'; s_equal_l s ["2"; "0"; "1"];
  act s 'A'; s_equal_l s ["0"; "1"; "2"];

  (* ==== manger ==== *)
  act s 'M'; s_equal_l s ["1"; "2"]; assert(!estomac = ["0"]);
  act s 'A'; s_equal_l s ["2"; "1"];
  act s 'M'; s_equal_l s ["1"];      assert(!estomac = ["2"; "0"]);

  (* ==== crache ==== *)
  act s 'C'; s_equal_l s ["2"; "1"]; assert(!estomac = ["0"]); (* sens direct *)
  act s 'A'; s_equal_l s ["1"; "2"];
  act s 'C'; s_equal_l s ["0"; "1"; "2"]; assert(!estomac = []);


   (* ==== retourner ==== *)
  act s 'R'; s_equal_l s ["2"; "1"; "0"];
  (*   == avance == *)
  act s 'A'; s_equal_l s ["1"; "0"; "2"];
  act s 'A'; s_equal_l s ["0"; "2"; "1"];
  act s 'A'; s_equal_l s ["2"; "1"; "0"];

  act s 'R'; act s 'R'; s_equal_l s ["2"; "1"; "0"];

  (*   == manger == *)
  act s 'M'

let solve n villes m actions =
  Printf.printf "actions: prem: %c, suite: %s\n" (List.hd actions) (List.fold_left (fun s c -> s ^ (Char.escaped c) ) "" actions);
  let serpent = ref (init_chaine villes) in
  List.iter (act serpent)  actions;
  list_of_chaine n !serpent


let print l = (String.concat "\n" l) |> Printf.printf "%s"

let _ =
  if false then
    test();
  if true then (
    let n = read_int()
    and m = read_int() in
    let villes = List.init n (fun _ -> read_line())
    and actions= String.fold_left (fun l x -> x::l) [] (read_line()) |> List.rev in
    solve n villes m actions
    |> print
  )
