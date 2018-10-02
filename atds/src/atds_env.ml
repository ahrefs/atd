(* Translation environment *)

type id = string
type ty_name = string

type env_t = {
  module_items : (string * Atd.Ast.type_expr) list;
  package      : string;
  package_dir  : string;
  input_file   : string option;
}

let default_env = {
  module_items = [];
  package      = "out";
  package_dir  = "out";
  input_file   = None;
}
