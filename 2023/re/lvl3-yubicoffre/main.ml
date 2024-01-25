(* NOTE: même problème que le "Baguenaudier" sur france-ioi : https://www.france-ioi.org/algo/task.php?idChapter=671&idTask=1504 *)

let affichage = ref true

let ft s =
  if !affichage then
    Printf.printf "%s" s
  else
    Printf.printf "\n%s" s;
  affichage := false

let rec vider = function
  (* on suppose qu'au début de l'appel de cette fonction, le tableau est rempli jusqu'à n *)
    1 -> ft "1 0"
  | 2 -> ft "2 0\n1 0"
  | n -> vider (n-2) ; Printf.printf "\n%i 0" n; remplir (n-2); vider (n-1)

and remplir = function
  (* on suppose qu'au début de l'appel le tableau est vide jusqu'à n *)
    0 -> ()
  | 1 -> ft "1 1"
  | 2 -> remplir 1; ft "2 1"
  | n -> remplir (n-1) ; vider (n-2) ; Printf.printf "\n%i 1" n ; remplir (n-2)

let _ =
  remplir (read_int()) (* différence : on vidait dans le pb de france-ioi *)
