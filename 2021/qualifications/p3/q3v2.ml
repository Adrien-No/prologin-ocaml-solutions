(* Copy String.split_on_char from OCaml 4.04 *)
module String = struct
  include String

  let split_on_char sep s =
    let r = ref [] in
    let j = ref (String.length s) in
    for i = String.length s - 1 downto 0 do
      if String.unsafe_get s i = sep then begin
        r := String.sub s (i + 1) (!j - i - 1) :: !r;
        j := i
      end
    done;
    String.sub s 0 !j :: !r
end

(**
   @param x le nombre magique
   @param n la longueur du code la Matriks
   @param l le code de la Matriks
*)

let print_int_list (l:int list) =
  (** pour comprendre, avec crochets délimiteurs *)
  print_string "[";
  let rec aux (l:int list) =
    match l with
    | [] -> print_string "]"
    | t::q -> print_int t; if not (q = []) then print_string " "; aux q
  in aux l

let rec print_int_list_no_crochets (l:int list) =
  (** sans les crochets *)
  match l with
    | [] -> ()
    | t::q -> print_int t; if not (q = []) then print_string " "; print_int_list_no_crochets q

let rec blited_list (l:'a list) (pos1:int) (pos2:int) =
  (** revoie la liste des élements contenus dans la liste l entre les indices pos1 et pos2 inclus *)
  match l with
  | [] -> []
  | t::q when pos1 > 0 -> blited_list q (pos1-1) pos2
  | t::q when pos2 > 0 -> t::blited_list q pos1 (pos2-1)
  | _ -> []

let rec sum_elt (l:int list) =
  (** revoie la somme des élements contenus dans la liste l*)
  match l with
  | [] -> 0
  | t::q -> t+sum_elt q

let test_cle (poss: 'a list) (x:int) (i1:int) (i2:int) =
  (** test la clé pour les possibilites d'indices i1 et i2 dans la liste poss *)
  x = (sum_elt (List.nth poss i1) * sum_elt (List.nth poss i2))

let rec possibilities (n:int) (src: 'a list) (pos1:int) (pos2:int) =
  (* renvoie une liste des sous-listes respectant les conditions de l'énoncé
   * complexité : O(n^2) ---- plus précisément : n*(n+1) /2 *)

  match pos1 with
  | pos1 when pos1 = n-> []
  | pos1 when pos2 <= n -> (blited_list src pos1 pos2) :: (possibilities n src pos1 (pos2+1))
  | _ -> possibilities n src (pos1+1) pos1

let resoudre x n l =
  (** TODO Les deux clés (chacune sur une ligne) ou le message "IMPOSSIBLE". *)
  (** on écrit les possibilités *)
  let poss = possibilities n l 0 0 in
  (**print_newline();
  print_int (List.length poss);
  for i=0 to (List.length poss)-1 do
    print_int_list (List.nth poss i)
     done;*)

  (** on les essaies*)
  let best_i1 = ref 0
  and best_i2 = ref 0 in
  for i1 = 0 to List.length (poss) -1 do
    for i2 = i1 to List.length (poss) -1 do
      if test_cle poss x i1 i2 then
        if List.length (List.nth poss i1) + List.length (List.nth poss i2) > List.length (List.nth poss !best_i1) + List.length (List.nth poss !best_i2) then
          begin
            best_i1 := i1;
            best_i2 := i2
          end
    done;
  done;

  (**print_newline();
  print_string "----- best_i1 and best_i2 -----";
  print_int !best_i1;
  print_newline();
     print_int (!best_i2);*)

  (** On interprète le résultat *)
  if !best_i1 = 0 && !best_i2 = 0 && not (x = 0) then
    (** impossible *)
    print_string "IMPOSSIBLE"
  else
    let l1 = List.nth poss !best_i1
    and l2 = List.nth poss !best_i2 in

    (** on affiche conformement les deux listes solution selon les priorités de l'énoncé*)
    match List.length l1, List.length l2 with
    | long1, long2 when long1 > long2 || sum_elt l1 > sum_elt l2 ->
      print_int_list_no_crochets l1;
      print_newline();
      print_int_list_no_crochets l2
    | long1, long2 when long1 < long2 || sum_elt l1 <= sum_elt l2 ->
        print_int_list_no_crochets l2;
        print_newline();
        print_int_list_no_crochets l1
    | _ -> ()
  ;;

let () =
  let x = read_int () in
  let n = read_int () in
  let l = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev in
  resoudre x n l
