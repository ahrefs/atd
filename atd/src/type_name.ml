(*
   Utilities for manipulating type names, since they're not plain strings.
*)

type t = TN of string list

let compare = Stdlib.compare
let equal = Stdlib.(=)

let to_string (TN path) = String.concat "." path

let basename name =
  match name with
  | TN [base_name]
  | TN [_; base_name] -> base_name
  | TN [] -> invalid_arg "Type_name.basename: empty name"
  | TN (_ :: _ as path) -> List.rev path |> List.hd

let split name =
  match name with
  | TN [base_name] -> None, base_name
  | TN [local_module_name; base_name] -> Some local_module_name, base_name
  | TN [] -> invalid_arg "Type_name.split: empty name"
  | TN _ -> invalid_arg ("Type_name.split: more than two components in "
                         ^ to_string name)
