(** A JSON-like AST type meant to accommodate various configuration
    file formats *)

type t =
  | Null of Loc.t
  | Bool of Loc.t * bool
  | Number of Loc.t * Number.t
  | String of Loc.t * string
  | Array of Loc.t * t list
  | Object of Loc.t * (Loc.t * string * t) list

(** Report the location associated with a node in a way that is suitable
    for preceding an error message.

    Sample output:
{v
File "config.yaml", lines 3-6, characters 7-5:
v}
*)
val loc_msg : t -> string

