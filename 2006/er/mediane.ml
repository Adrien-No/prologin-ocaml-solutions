
read_line ()
|> String.split_on_char ' '
|> List.map int_of_string
|> List.sort compare
|> (fun l -> List.nth l 10)
|> Printf.printf "%i"
