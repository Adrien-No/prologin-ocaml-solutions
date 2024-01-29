(* avec les sparses table *)
(*
D'après les spécifications du problème, R peut-être très grand. Deux possibilités :
- Réduire au maximum les facteurs autres que R (c'est ce qu'on a essayé de faire avec les arbres segmentés) : on peut espérer obtenir une complexité en O(R * N)
- Faire un algorithme indépendant de R, c'est ce qu'on va faire ici.

On précalcule une "sparse table" immuable (à l'inverse de l'arbre segmenté dynamique) pour une complexité O(NlogN)
Puis on construit des requêtes intelligentes sur le principe d'une fenêtre glissante (avec un peu d'arithmétique),
donnant à chaque ville son nombre maximum de batiment cassé.
finalement, complexité en O(NlogN + N) = O(NlogN)
 *)

open Printf

(* ================================ SPARSE TABLE ================================ *)
let print_int_list l =
  let len = List.length l in
  List.iteri (fun i x -> Printf.printf "%i" x; if i <> len-1 then Printf.printf " ") l

let max_pow_2 n =
  let rec aux acc n =
    if acc*2 <= n then aux (acc*2) n
    else acc
  in
  aux 1 n

let log2 x = int_of_float (log (float_of_int x) /. log 2.0)

let precompute t n =
  let n = Array.length t in
  let m = log2 n + 1 in
  let t' = Array.make_matrix n m 0 in
  assert (Array.length t = n);
  for i = 0 to n-1 do
    t'.(i).(0) <- t.(i)
  done;
  for j = 1 to m-1 do
    for i = 0 to n - (1 lsl j) do
      t'.(i).(j) <- max t'.(i).(j-1) t'.(i+(1 lsl (j-1))).(j-1)
    done
  done;
  t'

let max_bet_opti t a b =
  let k = log2 (b - a + 1) in
  max t.(a).(k) t.(b- (1 lsl k) +1).(k)

(* ============================================================================ *)

let passages n r =
  (* renvoie le tableau du nombre de passages du serpent dans chaque ville *)
  (* on fait attention à ne pas faire un algo en O(r) *)
  let tours_complets = r / n in
  let nb_max_last_turn = r - n * tours_complets in
  Array.init n (fun i -> if i < nb_max_last_turn then tours_complets + 1 else tours_complets)

let solve n r k villes =
  let t = precompute villes n in

  let passages = passages n r in

  let batiments i =
    let k' = ((k-1) * (passages.(i))) in (* k > 0, k' >= 0 *)
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
  in

  List.init n batiments

let () =
  let n = read_int () in
  let r = read_int () in
  let k = read_int () in
  let villes = (read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.map int_of_string) |> Array.of_list in
  solve n r k villes
  |> print_int_list
