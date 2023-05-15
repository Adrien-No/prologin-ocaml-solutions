(* let rec get_opti freqs max = *)
(*   if freqs = 0 then *)
(*     max *)
(*   else *)
(*     let temp = read_int() in *)
(*     if temp < max && (temp mod 3) = 0 then *)
(*       get_opti (freqs-1) temp *)
(*     else *)
(*       get_opti (freqs-1) max in *)

(* print_int (get_opti (read_int()) 1000); *)

let rec get_opti (freqs : 'a list) (max : int) =
  match freqs with
  | [] -> max
  | t::q -> if t < max && t mod 3 = 0 then
              get_opti q t
            else
              get_opti q max in
let tab =
print_int (get_opti [138;13;25;333;28] 1000)
