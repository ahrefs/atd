type t = {
  start: Pos.t;
  end_: Pos.t;
  path: string option;
}

let equal a b =
  Pos.equal a.start b.start
  && Pos.equal a.end_ b.end_
  && Option.equal String.equal a.path b.path

let compare a b =
  let c = Pos.compare a.start b.start in
  if c <> 0 then c
  else
    let c = Pos.compare a.end_ b.end_ in
    if c <> 0 then c
    else Option.compare String.compare a.path b.path
