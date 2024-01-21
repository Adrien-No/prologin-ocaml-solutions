(* on arrête de déconner avec les if *)

Printexc.record_backtrace true
open Printf

let print_int_array (t:int array) : unit =
  Printf.printf "[|";
  for i = 0 to Array.length t-1 do
    Printf.printf "%i" t.(i);
    if i <> Array.length t-1 then Printf.printf ";"
  done;
  Printf.printf "|]\n"

let print_iaa t =
  Array.iteri (fun i t ->
      Array.iteri (fun j x ->
          Printf.printf "%i" x;
          if j <> Array.length t -1 then
            Printf.printf " "
        ) t;
      if i  <> Array.length t -1 then
            Printf.printf "\n"
    ) t

(* ================================ SPARSE TABLE ================================ *)
let print_int_list l =
  let len = List.length l in
  List.iteri (fun i x -> Printf.printf "%i" x; if i <> len-1 then Printf.printf " ") l

let print_hashtbl h =
  printf "=== table de hachage de taille %i ===\n" (Hashtbl.length h);
  Hashtbl.iter (fun (a,b) v -> printf "(%i, %i) -> %i\n" a b v) h

let max_pow_2 n =
  let rec aux acc n =
    if acc*2 <= n then aux (acc*2) n
    else acc
  in
  aux 1 n

let max_bet_naif t a b =
  let res = ref t.(a) in
  for i = a to b do
    res := max !res t.(i)
  done;
  !res

let log2 x = int_of_float (log (float_of_int x) /. log 2.0)

let precompute t n =
  let n = Array.length t in
  let m = log2 n + 1 in
  let t' = Array.make_matrix n m 0 in
  (* printf "n= %i, m= %i\n" n m; *)
  assert (Array.length t = n);
  for i = 0 to n-1 do
    t'.(i).(0) <- t.(i)
  done;
  for j = 1 to m-1 do
    for i = 0 to n - (1 lsl j)  (* -2 *) do
      (* printf "i= %i, j= %i\n" i j; *)
      t'.(i).(j) <- max t'.(i).(j-1) t'.(i+(1 lsl (j-1))).(j-1)
      (* t'.(i).(j) <- max *)
      (*     t'.(i).(max (i+w-1) 0) *)
      (*     t'.(i+w).(j) *)
    done
  done;
  t'

let max_bet_opti0 t a b =
  (* a <= b *)
  (* printf "max_bet_opti: (%i, %i)\n%!" a b; *)
  let k = max_pow_2 (b-a+1) in
  let max1 = t.(a).(a+k-1)
  and max2 = t.(b-k+1).(b) in
  let res = max max1 max2 in
  (* printf "res: %i\n%!" res; *)
  res

let max_bet_opti t a b =
  (* let k = max_pow_2 (b-a+1) in *)
  (* let max1 = t.(a).(log2 (a+k-1)) *)
  (* and max2 = t.(b-k+1).(log2 b) in *)
  (* max max1 max2 *)
  (* printf "a= %i, b= %i\n" a b; *)
  let k = log2 (b - a + 1) in
  max t.(a).(k) t.(b- (1 lsl k) +1).(k)

let get_max h n a b =
  (* printf "a= %i, b= %i\n" a b; *)
  if a > b then
    begin
      (* on divise la requête en deux plages *)
      (* entre b et n-1 *)
      (* printf "divise\n"; *)
      let m1 = max_bet_opti h a (n-1) in
      (* entre 0 et (k+a-(n-1)) *)
      let m2 = max_bet_opti h 0 b in
      max m1 m2
    end
  else
    begin
      (* printf "divise pas\n"; *)
      max_bet_opti h a b
    end

(* ================================================================ *)

let passages n r =
  (* renvoie le tableau du nombre de passages du serpent dans chaque ville *)
  (* on fait attention à ne pas faire un algo en O(r) car r est grand (comme Quentin) *)
  let tours_complets = r / n in
  let nb_max_last_turn = r - n * tours_complets in
  Array.init n (fun i -> if i < nb_max_last_turn then tours_complets + 1 else tours_complets)

let solve n r k villes =
  let t = precompute villes n in

  (* print_iaa t; *)
  let passages = passages n r in

  (* printf "passages :\n"; *)
  (* print_int_array passages; *)
  (* test *)
  (* for i = n-1 downto n-k do *)
  (*   passages.(i) <- passages.(i) + 1 *)
  (* done; *)
  let batiments i =
    (* print_newline(); *)
    let k' = ((k-1) * (passages.(i))) in (* k > 0, k' >= 0 *)
    (* printf "k'= %i\n" k'; *)
    if k' >= n-1 then
      max_bet_opti t 0 (n-1)
    else if i + k' >= n then
      let k' = k' + (k-1) in (* le dépassement du cycle des villes nous fait gagner de la distance *)
      if k' >= n-1 then max_bet_opti t 0 (n-1) else
        begin
          assert(k'<n); (* on pense que si k'>= n alors on aura été dans la branche précédente du if *)
          let m1 = max_bet_opti t i (n-1) in
          max m1 (max_bet_opti t 0 (k'-(n-i)))
        end
    else
      max_bet_opti t i (i+k')
      (* if (i + k') >= n then *)
      (*   begin *)
      (*     let d = n-1-i in *)
      (*     i+k'+(k-1) - d *)
      (*   (\* (((i+k') mod n) + (k-1)) *\) *)
      (*   end *)
      (* else *)
      (*   i+k' *)
  in

  List.init n batiments

let tests() =
  let res = solve 5 2 3 [|2; 4; 3; 6; 8|] in
  print_int_list res; print_newline();
  assert(solve 5 2 3 [|2; 4; 3; 6; 8|] = [4; 6; 3; 6; 8]);
  let res = solve 5 2 4 [|2; 4; 3; 6; 8|] in
  print_int_list res;print_newline();
  assert(solve 5 2 4 [|2; 4; 3; 6; 8|] = [6; 8; 3; 6; 8]);
  assert(solve 5 6 3 [|5; 4; 3; 2; 1|] = [5; 4; 3; 5; 5]);
  assert(solve 10 6 3 (Array.init 10 ((+)1)) = [3; 4; 5; 6; 7; 8; 7; 8; 9; 10]);
  solve 20 42 3 (Array.init 20 (function x when x < 10 -> x+1 | x -> 20-x)) |> print_int_list;
  let res = solve 20 42 3 (Array.init 20 (function x when x < 10 -> x+1 | x -> 20-x)) in
  print_int_list res; print_newline();
  assert(res = [7; 8; 7; 8; 9; 10; 10; 10; 10; 10; 10; 9; 8; 7; 6; 5; 4; 4; 5; 6]);
  let res = solve  3  3 3 [|5; 1; 1|] in
  print_int_list res;
  assert(res = [5; 5; 5]);
  let res = solve 4 7 7 [|6; 1; 5; 1|] in
  print_int_list res;
  assert(res = [6; 6; 6; 6]);
  let res = solve 2 5 9 [|3; 4|] in
  print_int_list res;
  assert(res = [4; 4]);
  let res = solve 3 8 2 [|1; 4; 0|] in
  print_int_list res;
  assert(res = [4; 4; 4]);
  let res = solve 3 5 2 [|2; 9; 2|] in
  print_int_list res;
  assert(res = [9; 9; 9]);
  let res = solve 4 4 3 [|9; 6; 9; 2|] in
  print_int_list res;
  assert(res = [9; 9; 9; 9])


let () =
  (* tests() *)
  (* printf "salut"; *)
  (* ignore (solve 100000 100000 100000 (Array.init 100000 Fun.id)) *)

  let n = read_int () in
  let r = read_int () in
  let k = read_int () in
  let villes = (read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.map int_of_string) |> Array.of_list in
  solve n r k villes
  |> print_int_list
