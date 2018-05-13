(** Json adapters and tools for the user to make their own json adapters
    for common situations.

    Json adapters are used to rewrite json node into a form compatible
    with atdgen's conventions.
*)

(** Module signature required of any json adapter.
    For example, an ATD annotation
    [<json
       adapter.ocaml="Atdgen_runtime.Json_adapter.Type_field"]
    refers to the OCaml module
    [Atdgen_runtime.Json_adapter.Type_field].
*)
module type S = sig
  (** Convert a real json tree into an atd-compliant form. *)
  val normalize : Yojson.Safe.json -> Yojson.Safe.json

  (** Convert an atd-compliant json tree into a real json tree. *)
  val restore : Yojson.Safe.json -> Yojson.Safe.json
end

(** Support for json objects that contain a field that indicates
    the type of that object.
    The following
{[
{
  "type": "User",
  "id": "abc123",
  "age": 52
}
]}
        gets converted into a pair
{[
[
  "User",
  {
    "type": "User",
    "id": "abc123",
    "age": 52
  }
]
]}
        A corresponding ATD type definition is
{[
type obj = [
  | User of user
  | ...
] <json adapter.ocaml="Atdgen_runtime.Json_adapter.Type_field">

type user = {
  id: string;
  age: int;

  (* The following field definition is supported, albeit useless. *)
  type_ <json name="type">: string;
}
]}
*)
module Type_field : sig
  module type Param = sig
    val type_field_name : string
  end

  (** Default parameters, using [type_field_name = "type"]. *)
  module Default_param : Param

  (** Default adapter assuming a ["type"] field. *)
  include S

  (** Functor, allowing the use of a custom parameter:
{[
module Kind = Type_field.Make (struct type_field_name = "kind" end)
]}
  *)
  module Make (Param : Param) : S
end

(** Support for objects with a single field whose name indicates the type
    of the value.
    For instance,
{[
{
  "User": {
    "id": "abc123",
    "age": 52
  }
}
]}
    gets converted into
{[
[
  "User",
  {
    "id": "abc123",
    "age": 52
  }
]
]}
    An ATD type definition for this is
{[
type obj = [
  | User of user
  | ...
] <json adapter.ocaml="Atdgen_runtime.Json_adapter.One_field">

type user = {
  id: string;
  age: int;
}
]}
*)
module One_field : S

(** Support for the retired tag_field feature.
    This converts the following
{[
{
  "id": "abc123",
  "type": "User",
  "payload": 17
}
]}
    into
{[
{
  "id": "abc123",
  "type": "User",
  "payload": [ "User", 17 ]
}
]}
    As illustrated, two parameters need to be specified: the name
    of the field that holds the tag of the variant
    (["type"] in the example)
    and the name of the field that holds the value associated with
    that tag (["payload"] in the example).

    If the value is missing, we'll use the representation of an enum,
    i.e. a json string:
{[
{
  "id": "abc124",
  "type": "Event",
}
]}
    becomes
{[
{
  "id": "abc124",
  "type": "Event",
  "payload": "Event"
}
]}
*)
module Type_and_value_fields : sig
  module type Param = sig
    (** The name of the json field containing the type of the value
        as a json string. *)
    val type_field_name : string

    (** The name of the json field containing the value. *)
    val value_field_name : string
  end

  (** Functor needed to produce a module using the correct parameters. *)
  module Make (Param : Param) : S
end

