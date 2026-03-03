Type aliases.

  $ cat > aliases.atd << 'EOF'
  > type id = string
  > type score = float
  > type tag_list = string list
  > type opt_name = string option
  > EOF
  $ atdml aliases.atd
  $ cat aliases.mli
  (* Auto-generated from "aliases.atd". *)
  
  type id = string
  and score = float
  and tag_list = string list
  and opt_name = string option
  
  val id_of_yojson : Yojson.Safe.t -> id
  val yojson_of_id : id -> Yojson.Safe.t
  val id_of_string : string -> id
  val string_of_id : id -> string
  val id_of_channel : in_channel -> id
  val id_of_file : string -> id
  
  val score_of_yojson : Yojson.Safe.t -> score
  val yojson_of_score : score -> Yojson.Safe.t
  val score_of_string : string -> score
  val string_of_score : score -> string
  val score_of_channel : in_channel -> score
  val score_of_file : string -> score
  
  val tag_list_of_yojson : Yojson.Safe.t -> tag_list
  val yojson_of_tag_list : tag_list -> Yojson.Safe.t
  val tag_list_of_string : string -> tag_list
  val string_of_tag_list : tag_list -> string
  val tag_list_of_channel : in_channel -> tag_list
  val tag_list_of_file : string -> tag_list
  
  val opt_name_of_yojson : Yojson.Safe.t -> opt_name
  val yojson_of_opt_name : opt_name -> Yojson.Safe.t
  val opt_name_of_string : string -> opt_name
  val string_of_opt_name : opt_name -> string
  val opt_name_of_channel : in_channel -> opt_name
  val opt_name_of_file : string -> opt_name
  

Classic sum types: plain constructors, constructors with payloads, <json name>
and <ocaml name> renaming.

  $ cat > sums.atd << 'EOF'
  > type direction = [
  >   | North
  >   | South
  >   | East
  >   | West
  > ]
  > 
  > type shape = [
  >   | Circle of float
  >   | Rect <json name="rectangle"> of (float * float)
  >   | Dot <ocaml name="Point">
  >   | Arc <json name="arc"> <ocaml name="ArcShape"> of float
  > ]
  > EOF
  $ atdml sums.atd
  $ cat sums.mli
  (* Auto-generated from "sums.atd". *)
  
  type direction =
    | North
    | South
    | East
    | West
  and shape =
    | Circle of float
    | Rect of (float * float)
    | Point
    | ArcShape of float
  
  val direction_of_yojson : Yojson.Safe.t -> direction
  val yojson_of_direction : direction -> Yojson.Safe.t
  val direction_of_string : string -> direction
  val string_of_direction : direction -> string
  val direction_of_channel : in_channel -> direction
  val direction_of_file : string -> direction
  
  val shape_of_yojson : Yojson.Safe.t -> shape
  val yojson_of_shape : shape -> Yojson.Safe.t
  val shape_of_string : string -> shape
  val string_of_shape : shape -> string
  val shape_of_channel : in_channel -> shape
  val shape_of_file : string -> shape
  

Polymorphic variants via <ocaml repr="poly">.

  $ cat > polyvariants.atd << 'EOF'
  > type status = [
  >   | Active
  >   | Inactive
  >   | Pending of string
  > ] <ocaml repr="poly">
  > EOF
  $ atdml polyvariants.atd
  $ cat polyvariants.mli
  (* Auto-generated from "polyvariants.atd". *)
  
  type status = [
    | `Active
    | `Inactive
    | `Pending of string
  ]
  
  val status_of_yojson : Yojson.Safe.t -> status
  val yojson_of_status : status -> Yojson.Safe.t
  val status_of_string : string -> status
  val string_of_status : status -> string
  val status_of_channel : in_channel -> status
  val status_of_file : string -> status
  

Records: required, optional (?), and with-default (~) fields; implicit defaults
for common types; explicit default via <ml default="...">; <json name> on a
field.

  $ cat > records.atd << 'EOF'
  > type person = {
  >   name: string;
  >   age: int;
  >   ?email: string option;
  >   ~score: float;
  >   ~active: bool;
  >   ~tags: string list;
  >   ~level <ml default="1">: int;
  >   address <json name="addr">: string;
  > }
  > EOF
  $ atdml records.atd
  $ cat records.mli
  (* Auto-generated from "records.atd". *)
  
  type person = {
    name: string;
    age: int;
    email: string option;
    score: float;
    active: bool;
    tags: string list;
    level: int;
    address: string;
  }
  
  val make_person : name:string -> age:int -> ?email:string -> ?score:float -> ?active:bool -> ?tags:string list -> ?level:int -> address:string -> unit -> person
  val person_of_yojson : Yojson.Safe.t -> person
  val yojson_of_person : person -> Yojson.Safe.t
  val person_of_string : string -> person
  val string_of_person : person -> string
  val person_of_channel : in_channel -> person
  val person_of_file : string -> person
  
  $ cat records.ml
  (* Auto-generated from "records.atd". *)
  [@@@ocaml.warning "-27-32-33-35-39"]
  
  (* Inlined runtime — no external dependency needed. *)
  module Atdml_runtime = struct
    let bad_type expected_type x =
      Printf.ksprintf failwith "expected %s, got: %s"
        expected_type (Yojson.Safe.to_string x)
  
    let bad_sum type_name x =
      Printf.ksprintf failwith "invalid variant for type '%s': %s"
        type_name (Yojson.Safe.to_string x)
  
    let missing_field type_name field_name =
      Printf.ksprintf failwith "missing field '%s' in object of type '%s'"
        field_name type_name
  
    let bool_of_yojson = function
      | `Bool b -> b
      | x -> bad_type "bool" x
  
    let yojson_of_bool b = `Bool b
  
    let int_of_yojson = function
      | `Int n -> n
      | x -> bad_type "int" x
  
    let yojson_of_int n = `Int n
  
    let float_of_yojson = function
      | `Float f -> f
      | `Int n -> Float.of_int n
      | x -> bad_type "float" x
  
    let yojson_of_float f = `Float f
  
    let string_of_yojson = function
      | `String s -> s
      | x -> bad_type "string" x
  
    let yojson_of_string s = `String s
  
    let unit_of_yojson = function
      | `Null -> ()
      | x -> bad_type "null" x
  
    let yojson_of_unit () = `Null
  
    let list_of_yojson f = function
      | `List xs -> List.map f xs
      | x -> bad_type "array" x
  
    let yojson_of_list f xs = `List (List.map f xs)
  
    let option_of_yojson f = function
      | `String "None" -> None
      | `List [`String "Some"; x] -> Some (f x)
      | x -> bad_type "option" x
  
    let yojson_of_option f = function
      | None -> `String "None"
      | Some x -> `List [`String "Some"; f x]
  
    let nullable_of_yojson f = function
      | `Null -> None
      | x -> Some (f x)
  
    let yojson_of_nullable f = function
      | None -> `Null
      | Some x -> f x
  end
  
  type person = {
    name: string;
    age: int;
    email: string option;
    score: float;
    active: bool;
    tags: string list;
    level: int;
    address: string;
  }
  
  let make_person ~name ~age ?email ?(score = 0.) ?(active = false) ?(tags = []) ?(level = 1) ~address () : person =
    { name; age; email; score; active; tags; level; address }
  
  let rec person_of_yojson (x : Yojson.Safe.t) : person =
    match x with
    | `Assoc fields ->
      let name =
        match List.assoc_opt "name" fields with
        | Some v -> Atdml_runtime.string_of_yojson v
        | None -> Atdml_runtime.missing_field "person" "name"
      in
      let age =
        match List.assoc_opt "age" fields with
        | Some v -> Atdml_runtime.int_of_yojson v
        | None -> Atdml_runtime.missing_field "person" "age"
      in
      let email =
        match List.assoc_opt "email" fields with
        | None | Some `Null -> None
        | Some v -> Some (Atdml_runtime.string_of_yojson v)
      in
      let score =
        match List.assoc_opt "score" fields with
        | None -> 0.
        | Some v -> Atdml_runtime.float_of_yojson v
      in
      let active =
        match List.assoc_opt "active" fields with
        | None -> false
        | Some v -> Atdml_runtime.bool_of_yojson v
      in
      let tags =
        match List.assoc_opt "tags" fields with
        | None -> []
        | Some v -> (Atdml_runtime.list_of_yojson Atdml_runtime.string_of_yojson) v
      in
      let level =
        match List.assoc_opt "level" fields with
        | None -> 1
        | Some v -> Atdml_runtime.int_of_yojson v
      in
      let address =
        match List.assoc_opt "addr" fields with
        | Some v -> Atdml_runtime.string_of_yojson v
        | None -> Atdml_runtime.missing_field "person" "addr"
      in
      { name; age; email; score; active; tags; level; address }
    | _ -> Atdml_runtime.bad_type "person" x
  
  let rec yojson_of_person (x : person) : Yojson.Safe.t =
    `Assoc (List.concat [
      [("name", Atdml_runtime.yojson_of_string x.name)];
      [("age", Atdml_runtime.yojson_of_int x.age)];
      (match x.email with None -> [] | Some v -> [("email", Atdml_runtime.yojson_of_string v)]);
      [("score", Atdml_runtime.yojson_of_float x.score)];
      [("active", Atdml_runtime.yojson_of_bool x.active)];
      [("tags", (Atdml_runtime.yojson_of_list Atdml_runtime.yojson_of_string) x.tags)];
      [("level", Atdml_runtime.yojson_of_int x.level)];
      [("addr", Atdml_runtime.yojson_of_string x.address)];
    ])
  
  let person_of_string s =
    person_of_yojson (Yojson.Safe.from_string s)
  let string_of_person x =
    Yojson.Safe.to_string (yojson_of_person x)
  let person_of_channel ic =
    person_of_yojson (Yojson.Safe.from_channel ic)
  let person_of_file path =
    person_of_yojson (Yojson.Safe.from_file path)
  

All builtin types and composite type constructors (list, option, nullable,
abstract, tuple, nested).

  $ cat > builtins.atd << 'EOF'
  > type all_types = {
  >   a_unit: unit;
  >   a_bool: bool;
  >   a_int: int;
  >   a_float: float;
  >   a_string: string;
  >   a_list: int list;
  >   a_option: string option;
  >   a_nullable: bool nullable;
  >   a_abstract: abstract;
  >   a_tuple: (int * string * bool);
  >   a_nested: (float list) option;
  > }
  > EOF
  $ atdml builtins.atd
  $ cat builtins.mli
  (* Auto-generated from "builtins.atd". *)
  
  type all_types = {
    a_unit: unit;
    a_bool: bool;
    a_int: int;
    a_float: float;
    a_string: string;
    a_list: int list;
    a_option: string option;
    a_nullable: bool option;
    a_abstract: Yojson.Safe.t;
    a_tuple: (int * string * bool);
    a_nested: (float list) option;
  }
  
  val make_all_types : a_unit:unit -> a_bool:bool -> a_int:int -> a_float:float -> a_string:string -> a_list:int list -> a_option:string option -> a_nullable:bool option -> a_abstract:Yojson.Safe.t -> a_tuple:(int * string * bool) -> a_nested:(float list) option -> unit -> all_types
  val all_types_of_yojson : Yojson.Safe.t -> all_types
  val yojson_of_all_types : all_types -> Yojson.Safe.t
  val all_types_of_string : string -> all_types
  val string_of_all_types : all_types -> string
  val all_types_of_channel : in_channel -> all_types
  val all_types_of_file : string -> all_types
  

Parametric types with one and two type variables.

  $ cat > parametric.atd << 'EOF'
  > type 'a result = [
  >   | Ok of 'a
  >   | Error of string
  > ]
  > 
  > type ('a, 'b) either = [
  >   | Left of 'a
  >   | Right of 'b
  > ]
  > 
  > type 'a page = {
  >   items: 'a list;
  >   total: int;
  >   ?cursor: string option;
  > }
  > EOF
  $ atdml parametric.atd
  $ cat parametric.mli
  (* Auto-generated from "parametric.atd". *)
  
  type 'a result =
    | Ok of 'a
    | Error of string
  and ('a, 'b) either =
    | Left of 'a
    | Right of 'b
  and 'a page = {
    items: 'a list;
    total: int;
    cursor: string option;
  }
  
  val result_of_yojson :
    (Yojson.Safe.t -> 'a) ->
    Yojson.Safe.t ->
    'a result
  val yojson_of_result :
    ('a -> Yojson.Safe.t) ->
    'a result ->
    Yojson.Safe.t
  
  val either_of_yojson :
    (Yojson.Safe.t -> 'a) ->
    (Yojson.Safe.t -> 'b) ->
    Yojson.Safe.t ->
    ('a, 'b) either
  val yojson_of_either :
    ('a -> Yojson.Safe.t) ->
    ('b -> Yojson.Safe.t) ->
    ('a, 'b) either ->
    Yojson.Safe.t
  
  val make_page : items:'a list -> total:int -> ?cursor:string -> unit -> 'a page
  val page_of_yojson :
    (Yojson.Safe.t -> 'a) ->
    Yojson.Safe.t ->
    'a page
  val yojson_of_page :
    ('a -> Yojson.Safe.t) ->
    'a page ->
    Yojson.Safe.t
  

Mutually recursive types.

  $ cat > recursive.atd << 'EOF'
  > type tree = [
  >   | Leaf
  >   | Node of node
  > ]
  > type node = {
  >   value: int;
  >   children: tree list;
  > }
  > EOF
  $ atdml recursive.atd
  $ cat recursive.mli
  (* Auto-generated from "recursive.atd". *)
  
  type tree =
    | Leaf
    | Node of node
  and node = {
    value: int;
    children: tree list;
  }
  
  val tree_of_yojson : Yojson.Safe.t -> tree
  val yojson_of_tree : tree -> Yojson.Safe.t
  val tree_of_string : string -> tree
  val string_of_tree : tree -> string
  val tree_of_channel : in_channel -> tree
  val tree_of_file : string -> tree
  
  val make_node : value:int -> children:tree list -> unit -> node
  val node_of_yojson : Yojson.Safe.t -> node
  val yojson_of_node : node -> Yojson.Safe.t
  val node_of_string : string -> node
  val string_of_node : node -> string
  val node_of_channel : in_channel -> node
  val node_of_file : string -> node
  
