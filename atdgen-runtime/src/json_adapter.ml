(* Json adapters. See .mli. *)

module type S = sig
  val normalize : Yojson.Safe.json -> Yojson.Safe.json
  val restore : Yojson.Safe.json -> Yojson.Safe.json
end

module Type_field = struct
  module type Param = sig
    val type_field_name : string
  end

  module Make (Param : Param) : S = struct
    open Yojson.Safe

    open Param

    let normalize (x : json) : json =
      match x with
      | `Assoc fields ->
          (match
             try Some (List.assoc type_field_name fields)
             with Not_found -> None
           with
           | Some (`String type_) -> `List [ `String type_; x ]
           | _ -> x (* malformed *)
          )
      | `String type_ as x -> x
      | malformed -> malformed

    let restore (x : json) : json =
      match x with
      | `List [ `String type_; `Assoc fields ] ->
          let fields =
            (type_field_name, `String type_) ::
            List.filter (fun (k, v) -> k <> type_field_name) fields
          in
          `Assoc fields
      | `String type_ as x -> x
      | malformed -> malformed
  end

  module Default_param : Param = struct
    let type_field_name = "type"
  end

  include Make (Default_param)
end

module One_field = struct
  open Yojson.Safe

  let normalize (x : json) : json =
    match x with
    | `Assoc [name, value] -> `List [`String name; value]
    | `String _ as x -> x
    | malformed -> malformed

  let restore (x : json) : json =
    match x with
    | `List [`String name; value] -> `Assoc [name, value]
    | `String _ as x -> x
    | malformed -> malformed
end

module Type_and_value_fields = struct
  module type Param = sig
    val type_field_name : string
    val value_field_name : string
  end

  module Make (Param : Param) : S = struct
    open Yojson.Safe
    open Param

    let normalize (x : json) : json =
      let open Yojson.Safe.Util in
      match x with
      | `Assoc fields ->
          let type_ = member type_field_name x in
          let found = ref false in
          let fields =
            List.map (fun ((k, v) as field) ->
              if k = value_field_name then (
                found := true;
                (k, `List [type_; v])
              )
              else
                field
            ) fields
          in
          let fields =
            if !found then
              fields
            else
              (value_field_name, type_) :: fields
          in
          `Assoc fields
      | malformed -> malformed

    let unwrap_value (x : json) =
      match x with
      | `String tag -> (tag, None)
      | `List [`String tag; v] -> (tag, Some v)
      | malformed -> failwith ("Malformed json field " ^ value_field_name)

    let restore (x : json) : json =
      match x with
      | `Assoc fields ->
          let type_ = ref None in
          let fields =
            List.fold_right (fun ((k, tagged) as field) acc ->
              if k = value_field_name then (
                let tag, opt_value = unwrap_value tagged in
                type_ := Some tag;
                match opt_value with
                | None -> acc
                | Some v -> (value_field_name, v) :: acc
              )
              else if k = type_field_name then
                acc
              else
                field :: acc
              ) fields []
          in
          let fields =
            match !type_ with
            | None -> fields
            | Some tag -> (type_field_name, `String tag) :: fields
          in
          `Assoc fields
      | malformed -> malformed
  end
end
