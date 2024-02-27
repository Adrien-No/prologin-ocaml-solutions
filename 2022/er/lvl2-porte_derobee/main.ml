let h = read_int ()
let l = read_int ()
let lettres = Array.init h (fun _ -> Array.init h (String.get (read_line ())))
let x, y = (read_line () |> String.split_on_char ' ' |> function [a;b] -> (int_of_string a, int_of_string b) | _ -> failwith "error")
let chemin = List.init l (String.get (read_line ()))

let new_pos_of_c (x, y) c =
  match c with
  | 'N' -> (x-1, y)
  | 'S' -> (x+1, y)
  | 'O' -> (x, y-1)
  | 'E' -> (x, y+1)
  | _ -> failwith "invalid move"

let rec follow_path (x, y) path acc =
  match path with
  | [] -> acc^(Char.escaped (lettres.(x).(y)))
  | h::t ->
    let new_pos = new_pos_of_c (x, y) h in
    follow_path new_pos t (acc^(Char.escaped (lettres.(x).(y))))

let _ =
  follow_path (y, x) chemin ""
  |> print_string
