(*
   Type definitions used to build comparison results
*)

type direction = Forward | Backward | Both

type incompatibility_kind =
  | Missing_field of { def_name: string; field_name: string }
  | Default_required of { def_name: string }
  | Incompatible_type of { def_name: string }
  | Deleted_root_type of { def_name: string }
  | Added_root_type of { def_name: string }

type incompatibility = {
  direction: direction;
  kind: incompatibility_kind;
  location_old: Atd.Ast.loc option;
  location_new: Atd.Ast.loc option;
  description: string;
}

type result = incompatibility list
