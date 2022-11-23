(* Translation environment *)

type id = string
type ty_name = string

type env_t = {
  type_defs    : (Atd.Ast.type_name * Atd.Ast.type_expr) list;
  package      : string;
  input_file   : string option;
  output       : out_channel;
}

let default_env = {
  type_defs    = [];
  package      = "out";
  input_file   = None;
  output       = stdout;
}
