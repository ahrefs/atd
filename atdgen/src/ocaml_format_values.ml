(*
   Helper functions for translating ATD types into OCaml expressions.
   For conversions to OCaml types, see Ocaml_format_types.
*)

module R = Ocaml_repr

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
