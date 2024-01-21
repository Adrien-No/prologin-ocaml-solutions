Printexc.record_backtrace true
open Printf

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

let precompute t =
  let len = Array.length t in
  let h= Hashtbl.create len in

  let delta = ref 1 in
  while !delta <= len do
    (* printf "delta= %i\n" !delta; *)
    for i = 0 to len - !delta do
      let a, b = i, i + !delta -1 in
      let w = (b-a+1)/2 in
      Hashtbl.add h (a, b) (max (max_bet_naif t a (a+w-1)) (max_bet_naif t (a+w) b))
    done;
    delta := !delta * 2
  done;
  h

let max_bet_opti h a b =
  (* a <= b *)
  printf "max_bet_opti: (%i, %i)\n%!" a b;
  let k = max_pow_2 (b-a+1) in
  let max1 = Hashtbl.find h (a, a+k-1)
  and max2 = Hashtbl.find h (b-k+1, b) in
  let res = max max1 max2 in
  printf "res: %i\n%!" res;
  res

let get_max0 h n i plage =
  if i + plage > n - 1 then
    begin
      (* on divise la requête en deux plages *)
      (* entre b et n-1 *)
      printf "divise\n";
      let m1 = max_bet_opti h i (n-1) in
      (* entre 0 et (k+a-(n-1)) *)
      let m2 = max_bet_opti h 0 (abs (plage+i-n-1)) in
      max m1 m2
    end
  else
    begin
      printf "divise pas\n";
      max_bet_opti h i (i + plage -1)
    end

let get_max h n a b =
  if a > b then
    begin
      (* on divise la requête en deux plages *)
      (* entre b et n-1 *)
      printf "divise\n";
      let m1 = max_bet_opti h a (n-1) in
      (* entre 0 et (k+a-(n-1)) *)
      let m2 = max_bet_opti h 0 b in
      max m1 m2
    end
  else
    begin
      printf "divise pas\n";
      max_bet_opti h a b
    end

(* ================================================================ *)


(* let solve maxi t queries = *)
(*   let t_computed = precompute maxi t in *)
(*   (\* print_hashtbl t_computed; *\) *)
(*   List.map (max_bet_opti t_computed) queries *)

let passages n r =
  (* renvoie le tableau du nombre de passages du serpent dans chaque ville *)
  (* on fait attention à ne pas faire un algo en O(r) car r est grand (comme Quentin) *)
  let tours_complets = r / n in
  let nb_max_last_turn = r - n * tours_complets in
  Array.init n (fun i -> if i < nb_max_last_turn then tours_complets + 1 else tours_complets)

let solve n r k villes =
  let t_computed = precompute villes in
  let passages = passages n r in

  (* correction de passage car  *)
  print_hashtbl t_computed;
  printf "passages:\n";
  print_int_list (Array.to_list passages);
  let batiments i =
    (* let k' = k * passages.(i) in (\* TODO min pour éviter les dépassements *\) *)

    (* if k' <= 0 then *)
    (*   (\* on a même pas fait un tour complet *\) *)
    (*   villes.(i) *)
    (* else *)
    (*   begin *)
    (*     let k' = (k' + i - 1) mod n in *)
    (*     printf "k'= %i\n" k'; *)
    (*     get_max t_computed n i k' *)
    (*   end *)
    let k' = (k-1) * (passages.(i)) in (* k > 0, k' >= 0 *)
    printf "k'= %i\n" k';
    get_max t_computed n i (
      if i+k' < n then i+k'
      else ((i+k') mod n + (k-1)) mod n (* TODO besoin d'adapter encore avec des + k ? je pense pas car dans tout les cas on aura fait tout le tour .. *)
      )
  in

  List.init n batiments

let tests() =
  assert(solve 5 2 3 [|2; 4; 3; 6; 8|] = [4; 6; 3; 6; 8]);
  assert(solve 5 2 4 [|2; 4; 3; 6; 8|] = [6; 8; 3; 6; 8]);
  assert(solve 5 6 3 [|5; 4; 3; 2; 1|] = [5; 4; 3; 5; 5]);
  (* assert(solve 10 6 3 (Array.init 10 ((+)1)) = [3; 4; 5; 6; 7; 8; 7; 8; 9; 10]); *)
  (* solve 20 42 3 (Array.init 20 (function x when x < 10 -> x+1 | x -> 20-x)) |> print_int_list *)
  assert(solve 20 42 3 (Array.init 20 (function x when x < 10 -> x+1 | x -> 20-x)) = [7; 8; 7; 8; 9; 10; 10; 10; 10; 10; 10; 9; 8; 7; 6; 5; 4; 4; 5; 6])


let () =
  (* tests() *)
  (* (solve 10000 1000000 10000 (Array.init 10000 Fun.id)) |> print_int_list *)
  solve 4 4 3 [|9; 6; 9; 2|] |> print_int_list
  (* let n = read_int () in *)
  (* let r = read_int () in *)
  (* let k = read_int () in *)
  (* let villes = (read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.map int_of_string) |> Array.of_list in *)
  (* solve n r k villes *)
  (* |> print_int_list *)

(*
0  1 2 3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19
1  2 3 4  5  6  7  8  9 10 10  9  8  7  6  5  4  3  2  1 // input
7  8 7 8  9 10 10 10 10 10 10  9  8  7  6  5  4  4  5  6 // expected output

9 10 8 9 10 10 10 10 10 10 10  9  8  7  6 10 10 10 10 10 // obtained output
*)
