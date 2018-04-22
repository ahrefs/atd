(*
   Code used by the tests for the json adapter feature.
*)

(*
   Convert between concrete json `{"type":"foo", "x":123}`
   and abstract json `["Foo", {"X":123}]` which is compatible with atd/json
   conventions.
*)
module Type_field = struct
  open Yojson.Safe

  (* Find field `"type"` and make it the variant constructor. *)
  let normalize (x : json) : json =
    let open Yojson.Safe.Util in
    let cons = member "type" x |> to_string |> String.capitalize_ascii in
    (* The `"type"` field is left in place in `x` even though we don't
       need it, but it's simpler (and slightly faster). *)
    `List [ `String cons; x ]

  (* Add a `"type"` field. *)
  let restore (x : json) : json =
    let cons, assoc =
      match x with
      | `List [ `String cons; `Assoc l ] -> cons, l
      | _ -> assert false
    in
    `Assoc (("type", `String (String.lowercase_ascii cons)) :: assoc)
end
