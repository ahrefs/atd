(* Translation environment *)

type id = string
type ty_name = string

type env_t = {
  module_items : (string * Atd.Ast.type_expr) list;
  tags         : string list;
  package      : string;
  input_file   : string option;
  output       : out_channel;
}

let default_env = {
  module_items = [];
  tags         = [];
  package      = "out";
  input_file   = None;
  output       = stdout;
}
