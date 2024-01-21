
read_line ()
|> String.split_on_char ' '
|> List.map int_of_string
|> List.sort compare
|> (List.nth |> Fun.flip) 10 (* |> (fun l -> List.nth l 10)*)
|> Printf.printf "%i"
