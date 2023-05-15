()
let find_sousliste (t:'a array) (cum_t:'a array) (s:int) (n:int) =
  (* On cherche la sous-liste la plus longue possible dont la sommes des élements donne s*)
  let best_pos = [|0;0|] in
  for pos1 = 0 to n-1 do
    for pos2 = pos1 to n-1 do
      (**print_int pos1; print_string " ";print_int pos2;print_newline();*)

      if cum_t.(pos2) - (if pos1 = 0 then 0 else cum_t.(pos1-1)) = s then
        if pos2 - pos1 > best_pos.(1) - best_pos.(0)|| (best_pos.(1) = 0 && best_pos.(0) = 0) then
          begin best_pos.(0) <- pos1;
            best_pos.(1) <- pos2
          end
    done;
  done;
  print_int best_pos.(0);
  print_newline();
  print_int best_pos.(1);
  (*print_endline "gouzi";*)
  (* Maintenant on veut renvoyer la liste constituée des élements entre best_pos.(0) et best_pos.(1) compris *)
  let sousliste = Array.make (best_pos.(1)-best_pos.(0)+1) 0 in
  for i = 0 to n-1 do
    if i >= best_pos.(0) && i <= best_pos.(1) then
      sousliste.(i-best_pos.(0)) <- t.(i)
  done;
  sousliste;;

let print_int_tab (tab:'a array) (n:int) =
  for i = 0 to n-1 do
    if not (i = 0) then
      print_string " ";
    print_int tab.(i)
  done;
  print_newline();();;

(**let t1 = find_sousliste [|1;2;3;4;5|] [|1;3;6;10;15|] 7 5 in*)
let t1 = find_sousliste [|2;3|] [|2;5|] 3 2 in
print_int_tab t1 (Array.length t1)
