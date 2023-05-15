type arbre = Nil | Noeud of int * arbre list

(* let f a = *)
(*   match a with *)
(*     Nil -> Printf.printf "Nil\n" *)
(*   | Noeud(x,_) -> Printf.printf "%i\n" x *)

(* let bfs f a = *)
(*   let s = Queue.create() in *)
(*   Queue.push a s; *)
(*   while not (Queue.is_empty s) do *)
(*     match Queue.pop s with *)
(*       Nil -> () *)
(*     | Noeud(x, l) -> f(Noeud(x,l)); List.iter (fun x -> Queue.push x s) l *)
(*   done *)

let f x = Printf.printf "%i" x

let bfs a =
  let s = Queue.create() in
  Queue.push a s;
  let rec aux_bfs () =
    let a = Queue.pop s in
    begin
    match a with
      Nil -> ()(* if Queue.is_empty s then () else aux_bfs () *)
    | Noeud(x, l) -> f x; List.iter (fun x -> Queue.push x s) l
    end;
    if not (Queue.is_empty s) then
      aux_bfs ()
  in
  aux_bfs()

(* ################ dfs (change "Queue" by "Stack") ################ *)

let dfs a =
  let s = Stack.create() in
  Stack.push a s;
  let rec aux_dfs () =
    let a = Stack.pop s in
    begin
    match a with
      Nil -> ()(* if Stack.is_empty s then () else aux_dfs () *)
    | Noeud(x, l) -> f x; List.iter (fun x -> Stack.push x s) l
    end;
    if not (Stack.is_empty s) then
      aux_dfs ()
  in
  aux_dfs()


(* ################ example ################ *)
let a =
Noeud(0,
[
Noeud(1, [Noeud(4,[Nil]);Noeud(5,[Nil])]);
Noeud(2, [Nil]);
Noeud(3, [Noeud(6,
[Noeud(7,[Nil])]

)])
]
)
