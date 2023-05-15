()
let print_int_tab (tab:'a array) (n:int) =
  for i = 0 to n-1 do
    if not (i = 0) then
      print_string " ";
    print_int tab.(i)
  done;
  print_newline();()

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
  sum;;
print_int_tab (tab_sommes_elts_souslistes [|1;2;3;4;5|] [|1;3;6;10;15|] 5) 15
