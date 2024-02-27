(* pas fini *)

Printexc.record_backtrace true

(* version naive *)

let n = read_int ()
let clocks = (read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev) |> Array.of_list

let moves = ref []

let conf = Array.make n 0

let confs = ref []

let string_conf () = "[|" ^ Array.fold_left (^) "" (Array.map (fun i -> Printf.sprintf "%i ;" i) conf) ^ "|]"

let rec loop i_clock =
  if i_clock < 0 then failwith "caca";
  if i_clock = n then ()
  else
    begin
      for i = 0 to clocks.(i_clock) -1 do
        loop (i_clock+1);
        moves := (Printf.sprintf "%i H" i_clock)::!moves;
        loop ()
    end

let _ =



  loop 0;
  (* loop 0; *)
  !confs |> List.sort compare |> String.concat "\n" |> print_string;
  !moves |> String.concat "\n" |> Printf.printf "%s"



(* *)
(*       for i = 0 to clocks.(i_clock) -2 do
        loop (i_clock+1);
        moves := (Printf.sprintf "%i H" i_clock) :: !moves;
        conf.(i_clock) <- (1 + conf.(i_clock)) mod clocks.(i_clock);
        confs := (string_conf()) :: !confs;
      done;
      for i = 0 to clocks.(i_clock) -2 do
        loop (i_clock+1);
        moves := (Printf.sprintf "%i A" i_clock) :: !moves;
        conf.(i_clock) <- (let x = conf.(i_clock) - 1 in if x < 0 then x+clocks.(i_clock) else x) mod clocks.(i_clock);
        confs := (string_conf()) :: !confs;
      done;
      (* for i = clocks.(i_clock) -1 downto 1 do *)
      (*   conf.(i_clock) <- (let x = conf.(i_clock) - 1 in if x < 0 then x+clocks.(i_clock) else x) mod clocks.(i_clock); *)
      (*   confs := (string_conf()) :: !confs; *)
      (*   moves := (Printf.sprintf "%i A" i_clock) :: !moves; *)
      (*   loop (i_clock+1); *)
 *)
