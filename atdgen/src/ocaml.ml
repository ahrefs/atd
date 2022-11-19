(*
  Translation from ATD types into OCaml types and pretty-printing.

  This is derived from the ATD pretty-printer (atd_print.ml).
*)

open Atd.Stdlib_extra

open Atd.Ast
open Mapping
module Json = Atd.Json
module R = Ocaml_repr

let unwrap_option = function
  | Option (_, x, _, _)
  | Nullable (_, x, _, _) -> x
  | Name (loc, s, _, _, _) ->
      Error.error loc ("Not an option type: " ^ s)
  | x ->
      Error.error (loc_of_mapping x) "Not an option type"

let get_implicit_ocaml_default (x : (R.t, _) Mapping.t) =
  match x with
  | Unit (_, Unit, _) -> Some "()"
  | Bool (_, Bool, _) -> Some "false"
  | Int (_, Int o, _) ->
      Some (match o with
          Int -> "0"
        | Char -> "'\000'"
        | Int32 -> "0l"
        | Int64 -> "0L"
        | Float -> "0.")
  | Float (_, Float, _) -> Some "0.0"
  | String (_, String, _) -> Some "\"\""
  | List (_, _, List List, _) -> Some "[]"
  | List (_, _, List Array, _) -> Some "[||]"
  | Option (_, _, Option, _) -> Some "None"
  | Nullable (_, _, Nullable, _) -> Some "None"
  | _ -> None

type create_fields =
  { intf_params: string
  ; impl_params: string
  ; impl_fields: string
  }

let map_record_creator_field deref x =
  let o =
    match (x.f_arepr : R.t) with
    | Field o -> o
    | _ -> assert false
  in
  let fname = o.ocaml_fname in
  let impl2 = sprintf "\n    %s = %s;" fname fname in
  match x.f_kind with
      Required ->
        let t = ocaml_of_expr (ocaml_of_expr_mapping x.f_value) in
        let intf = sprintf "\n  %s: %s ->" fname t in
        let impl1 = sprintf "\n  ~%s" fname in
        { intf_params = intf
        ; impl_params = impl1
        ; impl_fields = impl2
        }

    | Optional ->
        let x = unwrap_option (deref x.f_value) in
        let t = ocaml_of_expr (ocaml_of_expr_mapping x) in
        let intf = sprintf "\n  ?%s: %s ->" fname t in
        let impl1 = sprintf "\n  ?%s" fname in
        { intf_params = intf
        ; impl_params = impl1
        ; impl_fields = impl2
        }

    | With_default ->
        let t = ocaml_of_expr (ocaml_of_expr_mapping x.f_value) in
        let intf = sprintf "\n  ?%s: %s ->" fname t in
        let impl1 =
          let default =
            match o.ocaml_default with
                None ->
                  (match get_implicit_ocaml_default (deref x.f_value) with
                       None ->
                         Error.error x.f_loc "Missing default field value"
                     | Some s -> s
                  )
              | Some s -> s
          in
          sprintf "\n  ?(%s = %s)" fname default
        in
        { intf_params = intf
        ; impl_params = impl1
        ; impl_fields = impl2
        }

let obj_unimplemented loc = function
   | Record -> ()
   | Object -> Error.error loc "Sorry, OCaml objects are not supported"
