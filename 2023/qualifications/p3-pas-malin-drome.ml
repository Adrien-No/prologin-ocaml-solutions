(*Printexc.record_backtrace true*)

let palindrome (l:'a array) : bool =
  let n = Array.length l in
  let res = ref true in
  for i = 0 to n/2-1 do
    if l.(i) <> l.(n-i-1) then res := false;
  done;
  !res

let list_of_string (s:string) : char list = (* attention renversé *)
  String.fold_left (fun b char -> char::b) [] s

let rec discriminate i mot min maj num : (char array) * (char array) * (char array) =
    match mot with
      [] -> Array.of_list min, Array.of_list maj, Array.of_list num (* on va tester si c'est un palindrome donc pas besoin de renverser *)
    | char::q -> let code_n = Char.code char in
      if code_n > 96 && code_n < 123 then discriminate (i+1) q (char::min) maj num
      else if code_n > 64 && code_n < 91 then discriminate (i+1) q min (char::maj) num
      else if code_n > 47 && code_n < 58 then discriminate (i+1) q min maj (char::num)
      else discriminate (i+1) q min maj num (* caractere non reconnu ex espace *)

let nbPasMalinDrome _ (mots:string list) =
  let nb_pasmalindrome = ref 0 in
  mots |> List.iter (fun mot -> let min, maj, num = discriminate 0 (mot |> list_of_string) [] [] [] in if palindrome min && palindrome maj && palindrome num then incr nb_pasmalindrome);
  Printf.printf "%i" !nb_pasmalindrome

let () =
  let n = read_int () in
  let mots = List.init n (fun _ -> read_line ()) in
  nbPasMalinDrome n mots
