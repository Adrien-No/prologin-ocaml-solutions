type 'a uf = {
  parent : 'a array;
  rang : 'a array;
  nelements : int;
}

let makeset uf x : 'a uf =
  uf.parent.(uf.nelements) <- x;
  uf.rang.(uf.nelements) <- 0;
  {
    parent = uf.parent;
    rang = uf.rang;
    nelements = uf.nelements+1;
  }


let find (uf:'a uf) x : 'a =
  let rec aux maillon =
    if uf.parent.(maillon) = maillon then maillon
    else aux (uf.parent.(maillon))
  in
  aux x

let union uf x y : unit =
  (* sous réserve que x <> y *)
  let rx = find uf x
  and ry = find uf y in

  if uf.rang.(rx) > uf.rang.(ry) then
    uf.parent.(ry) <- rx
  else
    uf.parent.(rx) <- ry;
  if uf.rang.(rx) = uf.rang.(uf.rang.(ry))
  then uf.rang.(ry) <- uf.rang.(ry) + 1
