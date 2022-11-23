(* Utilities *)

open Atd.Stdlib_extra
open Atdj_env

let type_not_supported x =
  let loc = Atd.Ast.loc_of_type_expr x in
  Atd.Ast.error_at loc "Type not supported by atdj."

(* Get rid of `wrap' constructors that we don't support on the Java side yet.
   They could be useful for timestamps, though. *)
let rec unwrap atd_ty =
  match atd_ty with
  | Atd.Ast.Wrap (_, x, _) -> unwrap x
  | x -> x

let rec unwrap_option env atd_ty =
  match atd_ty with
  | Atd.Ast.Wrap (_, x, _) -> unwrap_option env x
  | Option (_, x, _) -> unwrap_option env x
  | x -> x

(* Normalise an ATD type by expanding `top-level' type aliases *)
let rec norm_ty ?(unwrap_option = false) env atd_ty =
  let atd_ty = unwrap atd_ty in
  match atd_ty with
  | Atd.Ast.Name (_, (loc, name, _), _) ->
      (match name with
       | TN ["bool" | "int" | "float" | "string" | "abstract"] -> atd_ty
       | TN [_] ->
           (match List.assoc name env.type_defs with
            | Some x -> norm_ty env x
            | None ->
                eprintf "Warning: unknown type %s\n%!"
                  (Atd.Print.tn name);
                atd_ty)
       | TN _ ->
          Atd.Ast.error_at loc
            (sprintf "Imports aren't supported: %s"
               (Atd.Print.tn name))
      )
  | Option (_, atd_ty, _) when unwrap_option ->
      norm_ty env atd_ty
  | _ ->
      atd_ty

let warning loc msg =
  let loc_s = Atd.Ast.string_of_loc loc in
  eprintf "\
Warning:
%s
%s
%!"
    loc_s
    msg

(*
   Insert given string ind_S at the beginning of each line from string s.
*)
let indent_block_s =
  let rex = Re.Str.regexp "^" in
  fun ins s ->
    Re.Str.global_replace rex ins s

(*
   Insert n spaces at the beginning of each line from string s.
*)
let indent_block n s =
  let ins = String.make n ' ' in
  indent_block_s ins s
