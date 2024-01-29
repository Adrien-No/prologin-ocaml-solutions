(* cette fois on modifie en temps réel le tableau, et on cherche le max dessus *)
(* ça permet d'éviter d'ajouter les + k quand y'a module *)
(* -> sauf que c'est chiant ça veut dire faut un truc mutable *)

Printexc.record_backtrace true
open Printf

let print_int_array (t:int array) : unit =
  Printf.printf "[|";
  for i = 0 to Array.length t-1 do
    Printf.printf "%i" t.(i);
    if i <> Array.length t-1 then Printf.printf ";"
  done;
  Printf.printf "|]\n"

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

let get_max h n a b =
  printf "a= %i, b= %i\n" a b;
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

let passages n r =
  (* renvoie le tableau du nombre de passages du serpent dans chaque ville *)
  (* on fait attention à ne pas faire un algo en O(r) car r est grand (comme Quentin) *)
  let tours_complets = r / n in
  let nb_max_last_turn = r - n * tours_complets in
  Array.init n (fun i -> if i < nb_max_last_turn then tours_complets + 1 else tours_complets)

let solve n r k villes =
  let t_computed = precompute villes in
  let passages = passages n r in

  printf "passages :\n";
  print_int_array passages;
  (* test *)
  (* for i = n-1 downto n-k do *)
  (*   passages.(i) <- passages.(i) + 1 *)
  (* done; *)
  let batiments i =
    print_newline();
    let k' = ((k-1) * (passages.(i))) in (* k > 0, k' >= 0 *)
    (* printf "k'= %i\n" k'; *)
    get_max t_computed n i (
      let v = (i + k') in
      printf "i= %i, v= %i\n" i v;
      if v >= n then
        if v - n (*- 1*) >= i - 2 then
          (* toute la plage *)
          if i = 0 then
            n-1
          else
            i-1
        else
          (* deux cas, si passages.(i) -1 = 0 alors on prend les valeurs de bases plutot que prolonger la portée *)
          if passages.(i) = 1 then
            v - n + (k-1)
          else
            v - n + (k-1) * (passages.(i)-1) (* TODO sensé être < i *)
      else
        v
    )
  in

  List.init n batiments

let tests() =
  assert(solve 5 2 3 [|2; 4; 3; 6; 8|] = [4; 6; 3; 6; 8]);
  assert(solve 5 2 4 [|2; 4; 3; 6; 8|] = [6; 8; 3; 6; 8]);
  assert(solve 5 6 3 [|5; 4; 3; 2; 1|] = [5; 4; 3; 5; 5]);
  (* assert(solve 10 6 3 (Array.init 10 ((+)1)) = [3; 4; 5; 6; 7; 8; 7; 8; 9; 10]); *)
  (* solve 20 42 3 (Array.init 20 (function x when x < 10 -> x+1 | x -> 20-x)) |> print_int_list *)
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
  tests()
  (* (solve 10000 1000000 10000 (Array.init 10000 Fun.id)) |> print_int_list *)

  (* let n = read_int () in *)
  (* let r = read_int () in *)
  (* let k = read_int () in *)
  (* let villes = (read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.map int_of_string) |> Array.of_list in *)
  (* solve n r k villes *)
  (* |> print_int_list *)
