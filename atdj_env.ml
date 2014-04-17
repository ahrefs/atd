(* Translation environment *)

type id = string
type ty_name = string

(* Java types *)
type ty =
  [ `Class of ty_name * (id * ty_name) list
      (* Class name and constructor parameters *)
  | `Interface of ty_name
      (* Interface name *)
  ]

module Names = Map.Make(
  struct
    type t = string
    let compare = Pervasives.compare
  end
)

type env_t = {
  module_items : (string * Atd_ast.type_expr) list;
  package      : string;
  package_dir  : string;
  input_file   : string option;
  graph        : bool;
  types        : ty list;
  sub_types    : (ty_name * ty_name) list;
  names        : int Names.t;
}

let default_env = {
  module_items = [];
  package      = "out";
  package_dir  = "out";
  input_file   = None;
  graph        = false;              (* Whether to output a dot graph *)
  types        = [];                 (* Generated Java types *)
  sub_types    = [];                 (* Sub-typing relation amongst types *)
  names        = Names.empty;        (* Current used names, for freshening *)
}
