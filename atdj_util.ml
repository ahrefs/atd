(* Utilities *)

open Printf
open Atdj_env

(* Get rid of `wrap' constructors that we don't support on the Java side yet.
   They could be useful for timestamps, though. *)
let rec unwrap atd_ty =
  match atd_ty with
  | `Wrap (_, x, _) -> unwrap x
  | x -> x

(* Normalise an ATD type by expanding `top-level' type aliases *)
let rec norm_ty env atd_ty =
  let atd_ty = unwrap atd_ty in
  match atd_ty with
    | `Name (_, (_, name, _), _) ->
        (match name with
           | "bool" | "int" | "float" | "string" | "abstract" -> atd_ty
           | _ ->
               (try
                  let x = List.assoc name env.module_items in
                  norm_ty env x
                with Not_found ->
                  eprintf "Warning: unknown type %s\n%!" name;
                  atd_ty
               )
        )
    | _ -> atd_ty

let not_supported x =
  let loc = Atd_ast.loc_of_type_expr x in
  Atd_ast.error_at loc "Construct not yet supported by atdj."
