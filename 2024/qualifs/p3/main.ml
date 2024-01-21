let print_int_list (l:int list) : unit =
  print_string "[";
  let rec aux (l:int list) =
    match l with
    | [] -> print_string "]"
    | t::[] -> (Printf.printf "%d]\n" t)
    | t::q -> (Printf.printf "%d; " t; aux q)
  in aux l

type action = A | M | R | C

let tokenize = function
  | 'A' -> A
  | 'M' -> M
  | 'R' -> R
  | 'C' -> C
  | _   -> failwith "error while tokenize"

(* on implémente une file permettant aussi les ajouts FIFO à l'aide de deux piles. *)
type 'a serpent = {
  mutable debut : 'a Stack.t;
  mutable fin   : 'a Stack.t;

  mutable estomac : 'a Stack.t;
}

(* ================ functions ================ *)
let serpent_create s s' = {debut= Stack.copy s; fin= Stack.copy s'; estomac= Stack.create()}

let stack_rev (s: 'a Stack.t) : 'a Stack.t =
  let s' = Stack.create() in
  Stack.iter (fun x -> Stack.push x s') s;
  s'

let s_pop (s: 'a serpent) : 'a =
  if Stack.is_empty s.debut then
    begin
      s.debut <- stack_rev s.fin;
      s.fin   <- Stack.create();
      Stack.pop s.debut
    end
  else
    Stack.pop s.debut

let avancer (s: 'a serpent) : unit =
  let x = s_pop s in
  Stack.push x s.fin

let manger (s: 'a serpent) : unit =
  let x = s_pop s in
  Stack.push x s.estomac

let cracher (s: 'a serpent) : unit =
  let x = Stack.pop s.estomac in
  Stack.push x s.fin (* s.debut *)

let retourner (s: 'a serpent) : unit =
  let deb = (*stack_rev*) s.fin
  and fin = (*stack_rev*) s.debut in
  s.debut <- deb;
  s.fin   <- fin

let act (s: 'a serpent) : action -> unit = function
  | A -> avancer   s
  | M -> manger    s
  | C -> cracher   s
  | R -> retourner s

let acts (s: 'a serpent) : action list -> unit = List.iter (act s)

let solve (n: int) (l: string list) (m: int) (actions: action list) : string list =

  (* init serpent *)
  let serpent = serpent_create (Stack.create()) (Stack.create()) in
  List.iter (fun x -> Stack.push x serpent.debut) l;

  acts serpent actions;

  (* obtenir la sortie *)
  let output = ref [] in
  while not (Stack.is_empty serpent.debut) do
    output := s_pop serpent :: !output
  done;
  !output

(* ========================================= *)


(* ================ asserts ================ *)
let stack_of_list l = let s = Stack.create() in
  List.iter (fun x -> Stack.push x s) (List.rev l);
  s

let list_of_stack s = Stack.fold (fun l x -> x::l) [] s |> List.rev

let _ =
  let s = stack_of_list [0; 1; 2; 3; 4] in
  let serpent = serpent_create s (Stack.create()) in
  let serpent2= serpent_create s (Stack.create()) in
  (* stack_rev *)
  let s' = stack_rev s in Stack.push 5 s';
  (* print_int_list (list_of_stack s'); *)
  assert(s' = stack_of_list [5; 4; 3; 2; 1; 0]);
  assert(s = stack_of_list [0; 1; 2; 3; 4]);
  assert(stack_rev (Stack.create()) = Stack.create());

  (* s_pop *)
  assert( s_pop serpent = 0);
  ignore (s_pop serpent); ignore (s_pop serpent); ignore (s_pop serpent);
  (* print_int_list (list_of_stack serpent.debut); *)
  (* print_int_list (list_of_stack serpent2.debut); *)
  assert (s_pop serpent = 4); assert (s_pop serpent2= 0);
  try ignore (s_pop serpent); assert(false) with Stack.Empty -> assert(true);

  (* avancer *)
  avancer serpent2;
  assert(list_of_stack serpent2.debut = [2; 3; 4]
         && list_of_stack serpent2.fin = [1]);
  avancer serpent2; avancer serpent2; avancer serpent2;
  assert(list_of_stack serpent2.debut = []
         && list_of_stack serpent2.fin = [4; 3; 2; 1]);
  avancer serpent2;
  (* print_int_list (list_of_stack serpent2.debut); *)
  (* print_int_list (list_of_stack serpent2.fin); *)
  assert(list_of_stack serpent2.debut = [2; 3; 4]
         && list_of_stack serpent2.fin = [1]);

  (* manger et cracher *)
  manger serpent2;
  manger serpent2;
  assert(list_of_stack serpent2.estomac = [3; 2]);

  let serpent2'' = serpent_create serpent2.debut serpent2.fin in
  serpent2''.estomac <- Stack.copy serpent.estomac;
  manger serpent2''; cracher serpent2''; assert(serpent2'' = serpent2);

  cracher serpent2;
  assert(list_of_stack serpent2.debut = [3; 4]
        && list_of_stack serpent2.fin = [1]
        && list_of_stack serpent2.estomac = [2]);

  (* retourner *)
  cracher serpent2;
  let serpent2' = serpent_create serpent2.debut serpent2.fin in
  assert(serpent2' = serpent2);
  retourner serpent2';
  assert(list_of_stack serpent2'.debut= [1]
         && list_of_stack serpent2'.fin= [2; 3; 4]);
  retourner serpent2';
  assert(serpent2' = serpent2);
  avancer serpent2';
  retourner serpent2';
  assert(list_of_stack serpent2'.debut= [2; 1]
         && list_of_stack serpent2'.fin= [3; 4]);
  avancer serpent2';
  avancer serpent2';
  assert(list_of_stack serpent2'.debut= []
         && list_of_stack serpent2'.fin= [1; 2; 3; 4])

(* ========================================= *)
let print l = (String.concat "\n" l) |> Printf.printf "%s"

let _ =
  let n = read_int()
  and m = read_int() in
  let villes = List.init n (fun _ -> read_line())
  and actions= String.fold_left (fun acc x -> tokenize x::acc) [] (read_line()) |> List.rev in
  solve n villes m actions
  |> print
