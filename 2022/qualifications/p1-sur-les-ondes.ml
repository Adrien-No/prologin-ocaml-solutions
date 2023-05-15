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
   @param n nombre de fréquences données
   @param freqs la liste des fréquences à vérifier
*)
let rec get_opti (freqs : 'a list) (max : int) =
  match freqs with
  | [] -> max
  | t::q -> if t < max && t mod 3 = 0 then
              get_opti q t
            else
              get_opti q max

let surLesOndes n freqs =
  (** TODO Afficher la fréquence optimale. *)
  print_int (get_opti freqs 1000)

let () =
  let n = read_int () in
  let freqs = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev in
  surLesOndes n freqs
