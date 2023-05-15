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

let cumulative_tab (t:'a array) n =
  (* Crée un tableau cumulatatif en O(n) /!\ le tableau doit être non vide*)
  (* Cela permet d'acceder en O(1) à la somme des élements d'une sous-liste*)
  let cum = Array.make n t.(0)
  and sum = ref 0 in
  for i = 0 to n-1 do
    sum := !sum + t.(i);
    cum.(i) <- !sum
  done;
  cum

let tab_sommes_elts_souslistes (tab_0:'a array) (tab_cumulatif:'a array) (n:int) =
  (* tab_0 : le tableau initialement donné en entrée comprennant n élements
   * tab_cumulatif : tableau de n élements dont chaque élement est la somme de tous les élements de tab_0 précédant *)
  let sum = Array.make (n*(n+1)/2) 0
  and i_sum = ref 0 in
  for pos1 = 0 to n-1 do
    for pos2 = pos1 to n-1 do
        sum.(!i_sum) <- tab_cumulatif.(pos2) - (if pos1 = 0 then 0 else tab_cumulatif.(pos1-1));
          i_sum := !i_sum +1
    done;
  done;
  sum

let find_sousliste (t:'a array) (cum_t:'a array) (s:int) (n:int) =
  (* On cherche la sous-liste la plus longue possible dont la sommes des élements donne s*)
  let best_pos = [|0;0|] in
  for pos1 = 0 to n-1 do
    for pos2 = pos1 to n-1 do
      if cum_t.(pos2) - (if pos1 = 0 then 0 else cum_t.(pos1-1)) = s then
        if pos2 - pos1 > best_pos.(1) - best_pos.(0)  || (best_pos.(1) = 0 && best_pos.(0) = 0) then
          begin best_pos.(0) <- pos1;
            best_pos.(1) <- pos2
          end
    done;
  done;
  (**print_endline "meilleurs positions : ";
  print_int best_pos.(0);
  print_newline();
     print_int best_pos.(1);*)
  (*print_endline "gouzi";*)
  (* Maintenant on veut renvoyer la liste constituée des élements entre best_pos.(0) et best_pos.(1) compris *)
  let sousliste = Array.make (best_pos.(1)-best_pos.(0)+1) 0 in
  for i = 0 to n-1 do
    if i >= best_pos.(0) && i <= best_pos.(1) then
      sousliste.(i-best_pos.(0)) <- t.(i)
  done;
  sousliste

let print_int_tab (tab:'a array) (n:int) =
  for i = 0 to n-1 do
    if not (i = 0) then
      print_string " ";
    print_int tab.(i)
  done;
  (*print_newline();*)()

let resoudre x n l =
  (* -> première étape, on cherche les sommes d'élements de sous-listes dont leur produit donne x *)
  (* On crée un tableau à partir de la liste pour accèder à chaque élements en O(n)*)
  let tab = Array.of_list l in
  (**print_endline "tab :";
     print_int_tab tab (n-1);*)

  (* tableau cumulatif permettant d'obtenir la somme des élements d'une sous-liste en O(1)*)
  let cum_tab = cumulative_tab tab n in
  (**print_endline "cumulative tab :";
     print_int_tab cum_tab (n-1);*)

   (* on met toutes les sommes possibles dans tab_sum, optimisable en n*(n+1)/2 mais ça reste O(n^2)*)
  let tab_sum = tab_sommes_elts_souslistes tab cum_tab n in
  (**print_endline "tableau des sommes de sous-listes :";
     print_int_tab tab_sum (n*(n+1)/2);*)
  (* on trie ce tab_sum pour s'arrêter de tester une combinaison si on dépasse x quand on fait le produit des deux sommes*)
  Array.sort compare tab_sum;
  (**print_endline "après tri :";
     print_int_tab tab_sum (n*(n+1)/2)*)
  (* On sépare tab_sum en deux, les eléments < sqrt(x) et eléments > sqrt(x)*)
  (* pour cela on trouve i_sep, l'indice où il faut séparer les deux sous-listes *)
  let i_sep = ref (n*(n+1)/2) in (* longueur de tab_sum*)
  for i = 0 to n*(n+1)/2-1 do
    if tab_sum.(i) > int_of_float (sqrt (float_of_int x)) && i < !i_sep  then
      i_sep := i
  done;
  let s0 = ref 0
  and s1 = ref 0 in
  (* ainsi on va faire n tests *)
  for i_s1 = 0 to !i_sep-1 do
    for i_s0 = !i_sep to n*(n+1)/2-1 do
      (**print_int tab_sum.(i_s0);
      print_string " ";
      print_int tab_sum.(i_s1);
         print_newline();*)
      if  tab_sum.(i_s0) * tab_sum.(i_s1) = x then
        begin  (*print_endline "new val of s0 & s1:";*) s0 := tab_sum.(i_s0);
        s1 := tab_sum.(i_s1);
          (**print_int !s0; print_newline(); print_int !s1;print_newline()*)
      end
    done;
  done;
  (**print_endline "somme délements ok !";*)
  (* -> deuxième étape, on cherche les sous-listes les plus longues possibles et correspondant aux sommes *)
  (** On interprète le résultat *)
  if !s0 = 0 && !s1 = 0 && not (x = 0) then
    (** impossible *)
    print_string "IMPOSSIBLE"
  else
    (* on a t1 et t2 les deux sous-listes (par commodité ce sont des tableaux)*)
    let t1 = find_sousliste tab cum_tab !s0 n
    and t2 = find_sousliste tab cum_tab !s1 n in
    let len_t1 = Array.length t1
    and len_t2 = Array.length t2 in
    (**print_endline "find_sous liste ok !";*)
    (* enfin, on renvoit les listes selon l'ordre défini par l'énoncé *)
    if len_t1 > len_t2 then
      begin print_int_tab t1 len_t1;
      print_newline();
      print_int_tab t2 len_t2
      end
    else
    if len_t1 < len_t2 then
      begin print_int_tab t2 len_t2 ; print_newline() ; print_int_tab t1 len_t1 end
    else
      (*même longueur*)
    if !s0 > !s1 then
      begin print_int_tab t1 len_t1; print_newline(); print_int_tab t2 len_t2 end
    else
      begin print_int_tab t2 len_t2 ; print_newline() ; print_int_tab t1 len_t1 end

let () =
  let x = read_int () in
  let n = read_int () in
  let l = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev in
  resoudre x n l

(**Tu peux essayer ça pour tester ton programme avec un x négatif :
   x=-25  n=18  l=[-42 8 5 42 -1 20 -16 0 -49 -12 29 -25 -12 38 -22 -21 21 -24]*)
