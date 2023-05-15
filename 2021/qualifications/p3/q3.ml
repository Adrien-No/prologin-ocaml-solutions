(* X = ΣelemL1 * Σelem2
 *  len L1 + len L2 is max *)

(** idée pour accélerer (ne réduit pas la complexité)
 ** - enlever le "0"
 ** - une fois qu'on a une première solution, ne faire que des tests avec longeur des listes >
 ** - etudier le signe / primalité de la clé *)
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

let rec print_int_list (l:int list) =
  match l with
  | [] -> () (*print_string "]"*)
  | t::q -> print_int t; if not (q = []) then print_string " "; print_int_list q

let rec blited_list (l:'a list) (pos1:int) (pos2:int) =
  (** revoie la liste des élements contenus dans la liste l entre les indices pos1 et pos2 inclus *)
  match l with
  | [] -> []
  | t::q ->
    if pos1 > 0 then
      blited_list q (pos1-1) pos2
    else
    if pos2 > 0 then
      t::blited_list q pos1 (pos2-1)
    else
      []

let rec sum_elt (l:int list) =
  (** revoie la somme des élements contenus dans la liste l*)
  match l with
  | [] -> 0
  | t::q -> t+sum_elt q

let test_cle l x i1 i2 len1 len2 =
  (***)
  sum_elt (blited_list l i1 (len1-i1)) * sum_elt (blited_list l i2 (len2-i2)) = x

let resoudre x n l =
  (** TODO Les deux clés (chacune sur une ligne) ou le message "IMPOSSIBLE". *)
  (* On essaie une méthode brute ... *)

  let best_i1 = ref 0
  and best_i2 = ref 0
  and best_len1 = ref 0
  and best_len2 = ref 0 in
  for i1 = 0 to n do
    for i2 = 0 to n do
      for len1 = i1+1 to n do
        for len2 = i2+1 to n do
          (* if test_cle l x i1 i2 len1 len2 then *)
          begin
            print_string "[";
            print_int_list (blited_list l i1 (len1- i1));
            print_string " ; ";
              print_int_list (blited_list l i2 (len2- i2));
            print_string "]";
            print_newline()
            end

          (* if test_cle l x i1 i2 len1 len2 then
           *   if len1+len2 > !best_len1 + !best_len2 then
           *     begin
           *     best_len1 := len1;
           *     best_len2 := len2;
           *     best_i1 := i1;
           *     best_i2 := i2;
           *     end *)
        done;
      done;
    done;
  done;
  if !best_i1 = 0 && !best_i2 = 0 && !best_len1 = 0 && !best_len2 = 0 then
    print_string "IMPOSSIBLE"
  else
    let l1 = blited_list l !best_i1 (!best_len1- !best_i1)
    and l2 = blited_list l !best_i2 (!best_len2- !best_i2) in
    (** l1 > l2*)
    if List.length l1 > List.length l2 then
      begin
        print_int_list l1;
        print_newline();
        print_int_list l2;
      end
    (** l2 > l1*)
    else if List.length l2 > List.length l1 then
      begin
        print_int_list l2;
        print_newline();
        print_int_list l1;
      end
    else
      (** l1 = l2*)
    (** sl1 > sl2*)
    if sum_elt l1 > sum_elt l2 then
      begin
        print_int_list l1;
        print_newline();
        print_int_list l2;
      end
    else
      begin
        print_int_list l2;
        print_newline();
        print_int_list l1;
      end


let () =
  let x = read_int () in
  let n = read_int () in
  let l = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev in
  resoudre x n l
