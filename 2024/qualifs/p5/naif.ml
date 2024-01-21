(* on suppose que deux dieux n'ont pas les mêmes nom ET prénoms *)

type graph = int list array (* les sommets sont indexés de 0 à n-1 selon dieu_of_int *)

(* ================================ MISC  ================================ *)
let dieu_of_string s =
  match String.split_on_char ' ' s with
  | [prenom; nom] -> prenom, nom
  | _             -> failwith "erreur lecture prenom/nom du dieu"

let couple_dieu_of_string s =
  match String.split_on_char ' ' s with
  | [x1; y1; x2; y2] -> (x1, y1), (x2, y2)
  | _             -> failwith "erreur lecture couple prenom/nom de dieux"

let is_connected ((x1, y1), (x2, y2)) = x1=x2 || y1=y2 || x1=y2 || y1=x2

let string_of_dieu (nom, prenom) = nom ^ " " ^ prenom

(* ======================================================================= *)

(* ================================ ENTRÉE =============================== *)
let n = read_int ()
let dieux = List.init n (fun _ -> read_line ()) |> List.map dieu_of_string
let dieux_t = Array.of_list dieux
let m = read_int ()

let int_of_dieu_h = Hashtbl.create 64
let dieu_of_int_h = Hashtbl.create 64

let _ = List.iteri (fun i dieu ->
    Hashtbl.add int_of_dieu_h dieu i;
    Hashtbl.add dieu_of_int_h i dieu
  ) dieux

let int_of_dieu = Hashtbl.find int_of_dieu_h
let dieu_of_int = Hashtbl.find dieu_of_int_h
let passations = List.init m (fun _ -> read_line ()) |> List.map couple_dieu_of_string |> List.filter is_connected |> List.map (fun (d1, d2) -> int_of_dieu d1, int_of_dieu d2) (* arrêtes autorisées *)

(* ======================================================================= *)

(* ================================ GRAPH ================================ *)
let graph =
  Array.map (fun (x1,y1) -> List.filter (fun (x2, y2) -> x1=x2 || y1=y2 || x1=y2 || y1=x2) dieux |> List.map int_of_dieu) dieux_t

let graph_allowed = (* on a peut-être pas besoin du graphe entier *)
  let res = Array.make n [] in
  List.iter (fun (src, dst) -> res.(src) <- dst::res.(src)) passations;
  res
(* On a vérifié lors de la construction de passations que d1 et d2 ont un nom en commun pour toutes arêtes. *)

exception Est_complet
let dfs_is_complet g x0 =
  let mark = Hashtbl.create n in
  let nb_mark = ref 0 in
  try
    let rec loop x =
      let f y =
        if Hashtbl.mem mark y then ()
        else
          begin
            incr nb_mark;
            if !nb_mark = n then raise Est_complet;
            Hashtbl.add mark y true;
            loop y
          end
      in
      List.iter f g.(x)
    in
    loop x0;
    false
  with Est_complet -> true
(* ======================================================================= *)

let cheminValide n dieux m passations =
  (** TODO Si le message n'a pas été passé en respectant le protocole, afficher
  sur une ligne le message `NON`. Sinon, afficher `OUI` sur une ligne, puis, en
  affichant un nom par ligne, le nom de tous les dieux ayant pu être dieu
  initial.  *)
  List.init n Fun.id
  |> List.filter (dfs_is_complet graph_allowed)
  |> function
  | [] -> "NON"
  | l  ->

let _ =
  cheminValide n dieux m passations
