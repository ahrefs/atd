type t = {
  row: int;
  column: int;
}

let equal a b = a.row = b.row && a.column = b.column

let compare a b =
  let c = Int.compare a.row b.row in
  if c <> 0 then c
  else Int.compare a.column b.column
