let miseEnBoite (n:int) (restes:int list) (boites:int list) : int =
  let boites = Array.of_list boites
  and restes = Array.of_list restes in
  Array.sort (fun x y -> if x>y then 1 else if x<y then -1 else 0) boites;
  Array.sort (fun x y -> if x>y then 1 else if x<y then -1 else 0) restes;

  let final = ref 0 in
  let accu_dos = ref ((Array.length boites)-1) in
  for i=0 to (n-1) do
    let accu = ref 0 in
    while !accu <= !accu_dos && restes.(i) > boites.(!accu) do
      accu := !accu + 1
        done;
    if !accu <= !accu_dos then begin
      (*final := !final + 1*)
        incr final; (* pareil que : final := !finale + 1; *)
        (*boites = pop boites;*)
        accu_dos := !accu_dos - 1
    end
        done;
        !final(*;;*)

let pop (array:int array) : int array =
    let newl = (Array.length array) - 1 in
    let new_array = Array.make newl 0 in
    Array.blit array 1 new_array 0 newl;
    new_array(*;;*)

let _ =
  let n = read_int () in
  let restes = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev in
  let boites = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev in
  miseEnBoite n restes boites
