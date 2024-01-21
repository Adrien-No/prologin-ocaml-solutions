open Main
open V2
open Random

let print_int_array (t:int array) : unit =
  Printf.printf "[|";
  for i = 0 to Array.length t-1 do
    Printf.printf "%i" t.(i);
    if i <> Array.length t-1 then Printf.printf ";"
  done;
  Printf.printf "|]\n"

let test() : unit =
  let k = (Random.int 5) + 1
  and n = (Random.int 5) + 1 in
  let l = List.init n (fun _ -> Random.int 10 + 1) in
  (* Printf.printf "debut test\n"; *)
  let res_main = Main.solve k n l
  and res_v2   = V2.solve k n l in
  if  res_main <> res_v2 then
    begin
      Printf.printf ("main : %s | v2 : %s -> k = %i, n = %i, l = ") res_main res_v2 k n;
      print_int_array (Array.of_list l);
    end
  (* Printf.printf "fin test\n" *)

let _ =
  Random.self_init();
  for _ = 0 to 10 do
    test()
  done
