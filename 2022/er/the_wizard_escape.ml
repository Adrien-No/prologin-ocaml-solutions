exception Trouve
let findpath (t:int) (s:char array array) =

  let fire_zone = Array.make_matrix t t false in
  let draw x y =
    for i = max 0 (x-1) to min (t-1) (x+1) do
      for j = max 0 (y-1) to min (t-1) (y+1) do
        let v = s.(i).(j) in
        if v <> '@' && v <> 'P' && (not (v = 'T' && (i,j) <> (x,y))) then (* dernier cas pour gérer les tours superposées*)
          fire_zone.(i).(j) <- true
      done;
    done
  in

  Array.iteri (fun i l -> Array.iteri (fun j v -> if v = 'T' then draw i j) l) s;

  let depls t x y =
    let l = [(x-1,y);(x+1,y);(x,y-1);(x,y+1)] in
    List.fold_left (fun b (x,y) -> if 0 <= x && x < t && 0 <= y && y < t then (x,y)::b else b) [] l
  in

  let s_pots = Array.make_matrix t t (-1) in

let rec bfs life x y : unit =
  (*Printf.printf "fire_zone:\n";
    print_b fire_zone; Printf.printf "\n";*)
  let life = max life 0 in
  let d = depls t x y in

  match s.(x).(y) with
    '@' -> raise Trouve
  | _ when fire_zone.(x).(y) && life = 0 -> ()
  | _ when s_pots.(x).(y) >= life -> ()
  | 'P' ->
    let life = 3 in
    s_pots.(x).(y) <- life;
    List.iter (fun (x,y) -> bfs (life-1) x y) d


  | _ ->
    s_pots.(x).(y) <- life; List.iter (fun (x,y) -> bfs (life-1) x y) d

  in
  try
  bfs 0 (t-1) 0;
  false
  with Trouve -> true

let () =
  let t = read_int () in
  let s = Array.init t (fun _ -> Array.init t (String.get (read_line ()))) in
  if findpath t s then Printf.printf "Oui\n" else Printf.printf "Non\n"
