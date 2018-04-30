(** Various convenience types and functions *)

type 'a ocaml_array = 'a array
    (** An alias for OCaml's standard array type,
        used in generated code. *)

module Biniou :
sig
  type 'a reader = Bi_inbuf.t -> 'a
    (** Type of a [read_] function as produced by [atdgen -biniou]. *)

  type 'a writer = Bi_outbuf.t -> 'a -> unit
    (** Type of a [write_] function as produced by [atdgen -biniou]. *)

  val from_channel :
    ?len:int ->
    ?shrlen:int ->
    'a reader -> in_channel -> 'a
    (** Read a biniou value from a channel.
        @param len     input buffer length.
        @param shrlen  obsolete and ignored.
    *)

  val from_file :
    ?len:int ->
    ?shrlen:int ->
    'a reader -> string -> 'a
    (** Read a biniou value from a file.
        @param len     input buffer length.
        @param shrlen  obsolete and ignored.
    *)

  val to_channel :
    ?len:int ->
    ?shrlen:int ->
    'a writer -> out_channel -> 'a -> unit
    (** Write a biniou value to a channel.
        @param len     output buffer length.
        @param shrlen  obsolete and ignored.
    *)

  val to_file :
    ?len:int ->
    ?shrlen:int ->
    'a writer -> string -> 'a -> unit
    (** Write a biniou value to a file.
        @param len     output buffer length.
        @param shrlen  obsolete and ignored.
    *)
end

module Json :
sig
  type 'a reader = Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a
    (** Type of a [read_] function as produced by [atdgen -json].

        In versions of yojson greater than 1.0.1,
        type [Yojson.Safe.lexer_state] is equivalent to
        [Yojson.lexer_state], [Yojson.Basic.lexer_state] and
        [Yojson.Raw.lexer_state]. *)

  type 'a writer = Bi_outbuf.t -> 'a -> unit
    (** Type of a [write_] function as produced by [atdgen -json]. *)

  val from_lexbuf :
    ?stream:bool ->
    'a reader -> Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a
    (** Read a JSON value from a lexbuf.
        @param stream  if [true], the JSON parser will not try
                       to consume whitespace until the end of file.
                       Default is [false], which raises a [Yojson.Json_error]
                       exception if the valid JSON value is followed
                       by anything other than standard JSON whitespace.
    *)

  val from_string :
    ?buf:Bi_outbuf.t ->
    ?fname:string ->
    ?lnum:int ->
    'a reader -> string -> 'a
    (** Convert a JSON value from a string.
        @param buf     buffer used to accumulate string data
                       during the lexing phase.
        @param fname   input file name to be used in error messages.
                       It does not have to be the name of a real file,
                       it can be something like ["<stdin>"].
        @param lnum    line number to assign to the first line of input.
                       For example [lnum=10] means that an error on the first
                       line of input will be reported as an error on line 10.
                       Default: 1.
    *)

  val from_channel :
    ?buf:Bi_outbuf.t ->
    ?fname:string ->
    ?lnum:int ->
    'a reader -> in_channel -> 'a
    (** Read a JSON value from a channel.
        @param buf     buffer used to accumulate string data
                       during the lexing phase.
        @param fname   input file name to be used in error messages.
                       It does not have to be the name of a real file,
                       it can be something like ["<stdin>"].
        @param lnum    line number to assign to the first line of input.
                       For example [lnum=10] means that an error on the first
                       line of input will be reported as an error on line 10.
                       Default: 1.
    *)

  val from_file :
    ?buf:Bi_outbuf.t ->
    ?fname:string ->
    ?lnum:int ->
    'a reader -> string -> 'a
    (** Read a JSON value from a channel.
        @param buf     buffer used to accumulate string data
                       during the lexing phase.
        @param fname   input file name to be used in error messages.
                       It is intended to represent the source file
                       if it is different from the input file.
        @param lnum    line number to assign to the first line of input.
                       For example [lnum=10] means that an error on the first
                       line of input will be reported as an error on line 10.
                       Default: 1.
    *)

  val stream_from_lexbuf :
    ?fin:(unit -> unit) ->
    'a reader -> Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a Stream.t
    (** Read a stream of JSON values from a lexbuf.
        @param fin     finalization function executed once when the end of the
                       stream is reached either because there is no more
                       input or because of an exception. This is typically
                       used to close the input channel, e.g.
                       [fun () -> close_in_noerr ic].
    *)

  val stream_from_string :
    ?buf:Bi_outbuf.t ->
    ?fin:(unit -> unit) ->
    ?fname:string ->
    ?lnum:int ->
    'a reader -> string -> 'a Stream.t
    (** Read a stream of JSON values from a channel.
        Values do not have to be separated by newline characters.
        @param buf     buffer used to accumulate string data
                       during the lexing phase.
        @param fin     finalization function executed once when the end of the
                       stream is reached either because there is no more
                       input or because of an exception. This is typically
                       used to free the underlying resources, if any.
        @param fname   input file name to be used in error messages.
                       It does not have to be the name of a real file,
                       it can be something like ["<stdin>"].
        @param lnum    line number to assign to the first line of input.
                       For example [lnum=10] means that an error on the first
                       line of input will be reported as an error on line 10.
                       Default: 1.
    *)

  val stream_from_channel :
    ?buf:Bi_outbuf.t ->
    ?fin:(unit -> unit) ->
    ?fname:string ->
    ?lnum:int ->
    'a reader -> in_channel -> 'a Stream.t
    (** Read a stream of JSON values from a channel.
        Values do not have to be separated by newline characters.
        @param buf     buffer used to accumulate string data
                       during the lexing phase.
        @param fin     finalization function executed once when the end of the
                       stream is reached either because there is no more
                       input or because of an exception. This is typically
                       used to close the input channel, e.g.
                       [fun () -> close_in_noerr ic].
        @param fname   input file name to be used in error messages.
                       It does not have to be the name of a real file,
                       it can be something like ["<stdin>"].
        @param lnum    line number to assign to the first line of input.
                       For example [lnum=10] means that an error on the first
                       line of input will be reported as an error on line 10.
                       Default: 1.
    *)

  val stream_from_file :
    ?buf:Bi_outbuf.t ->
    ?fin:(unit -> unit) ->
    ?fname:string ->
    ?lnum:int ->
    'a reader -> string -> 'a Stream.t
    (** Read a stream of JSON values from a file.
        Values do not have to be separated by newline characters.
        @param buf     buffer used to accumulate string data
                       during the lexing phase.
        @param fin     finalization function executed once when the end of the
                       stream is reached either because there is no more
                       input or because of an exception. This can be used
                       to remove the input file if it was temporary, e.g.
                       [fun () -> Sys.remove fname].
        @param fname   input file name to be used in error messages.
                       It is intended to represent the source file
                       if it is different from the input file.
        @param lnum    line number to assign to the first line of input.
                       For example [lnum=10] means that an error on the first
                       line of input will be reported as an error on line 10.
                       Default: 1.
    *)

  val list_from_string :
    ?buf:Bi_outbuf.t ->
    ?fin:(unit -> unit) ->
    ?fname:string ->
    ?lnum:int ->
    'a reader -> string -> 'a list
    (** Read a list of JSON values from a channel.
        Values do not have to be separated by newline characters.
        @param buf     buffer used to accumulate string data
                       during the lexing phase.
        @param fin     finalization function executed once when the end of the
                       stream is reached either because there is no more
                       input or because of an exception. This is typically
                       used to free the underlying resources, if any.
        @param fname   input file name to be used in error messages.
                       It does not have to be the name of a real file,
                       it can be something like ["<stdin>"].
        @param lnum    line number to assign to the first line of input.
                       For example [lnum=10] means that an error on the first
                       line of input will be reported as an error on line 10.
                       Default: 1.
    *)

  val list_from_channel :
    ?buf:Bi_outbuf.t ->
    ?fin:(unit -> unit) ->
    ?fname:string ->
    ?lnum:int ->
    'a reader -> in_channel -> 'a list
    (** Read a list of JSON values from a channel.
        Values do not have to be separated by newline characters.
        @param buf     buffer used to accumulate string data
                       during the lexing phase.
        @param fin     finalization function executed once when the end of the
                       stream is reached either because there is no more
                       input or because of an exception. This is typically
                       used to close the input channel, e.g.
                       [fun () -> close_in_noerr ic].
        @param fname   input file name to be used in error messages.
                       It does not have to be the name of a real file,
                       it can be something like ["<stdin>"].
        @param lnum    line number to assign to the first line of input.
                       For example [lnum=10] means that an error on the first
                       line of input will be reported as an error on line 10.
                       Default: 1.
    *)

  val list_from_file :
    ?buf:Bi_outbuf.t ->
    ?fname:string ->
    ?lnum:int ->
    'a reader -> string -> 'a list
     (** Read a list of JSON values from a file.
        Values do not have to be separated by newline characters.
        @param buf     buffer used to accumulate string data
                       during the lexing phase.
        @param fname   input file name to be used in error messages.
                       It is intended to represent the source file
                       if it is different from the input file.
        @param lnum    line number to assign to the first line of input.
                       For example [lnum=10] means that an error on the first
                       line of input will be reported as an error on line 10.
                       Default: 1.
    *)

  val to_string :
    ?len:int ->
    'a writer -> 'a -> string
    (** Write a JSON value to a string.
        @param len     output buffer length.
    *)

  val to_channel :
    ?len:int ->
    'a writer -> out_channel -> 'a -> unit
    (** Write a JSON value to a channel.
        @param len     output buffer length.
    *)

  val to_file :
    ?len:int ->
    'a writer -> string -> 'a -> unit
    (** Write a JSON value to a file.
        @param len     output buffer length.
    *)

  val stream_to_string :
    ?len:int ->
    ?lf:string ->
    'a writer -> 'a Stream.t -> string
    (** Write a stream of values to a string.
        @param len     output buffer length.
        @param lf      additional element terminator. Default: ["\n"].
    *)

  val stream_to_channel :
    ?len:int ->
    ?lf:string ->
    'a writer -> out_channel -> 'a Stream.t -> unit
    (** Write a stream of values to a channel.
        @param len     output buffer length.
        @param lf      additional element terminator. Default: ["\n"].
    *)

  val stream_to_file :
    ?len:int ->
    ?lf:string ->
    'a writer -> string -> 'a Stream.t -> unit
    (** Write a stream of values to a file.
        @param len     output buffer length.
        @param lf      additional element terminator. Default: ["\n"].
    *)

  val list_to_string :
    ?len:int ->
    ?lf:string ->
    'a writer -> 'a list -> string
    (** Write a list of values to a string.
        @param len     output buffer length.
        @param lf      additional element terminator. Default: ["\n"].
    *)

  val list_to_channel :
    ?len:int ->
    ?lf:string ->
    'a writer -> out_channel -> 'a list -> unit
    (** Write a list of values to a channel.
        @param len     output buffer length.
        @param lf      additional element terminator. Default: ["\n"].
    *)

  val list_to_file :
    ?len:int ->
    ?lf:string ->
    'a writer -> string -> 'a list -> unit
    (** Write a list of values to a file.
        @param len     output buffer length.
        @param lf      additional element terminator. Default: ["\n"].
    *)


  val preset_unknown_field_handler : string -> string -> unit
    (**
        [preset_unknown_field_handler src_loc field_name]
        raises a [Failure] exception with a message containing
        the location of the type definition in the source ATD file
        ([src_loc]) and the name of the field ([field_name]).
    *)

  val unknown_field_handler : (string -> string -> unit) ref
    (** Function called when an unknown JSON field is encountered if
        the code was generated by atdgen -json-strict-fields.
        Its preset behavior is to call [preset_unknown_field_handler]
        which raises a [Failure] exception.

        Usage: [!Atdgen_runtime.Util.Json.unknown_field_handler src_loc field_name]
        where [src_loc] is the location of the type definition
        in the source ATD file and [field_name] is the unknown
        JSON field name.
    *)

  val preset_constr_mismatch_handler :
    string -> string -> string -> string -> unit
    (**
        [preset_constr_mismatch_handler
           constr_field constr_constr value_field value_constr]
        raises a [Oj_run.Error] exception with a message containing
        the constructor field and value ([constr_field] and [constr_constr])
        which don't match the value field's ([value_field])
        constructor ([value_constr]).
    *)

  val constr_mismatch_handler :
    (string -> string -> string -> string -> unit) ref
    (** Function called when an explicit constructor field value does not
        match the constructor used if the code was generated by atdgen
        -json-strict-constrs. Its preset behavior is to call
        [preset_unknown_field_handler] which raises a [Oj_run.Error]
        exception.

        Usage: [!Atdgen_runtime.Util.Json.constr_mismatch_handler
        constr_field constr_constr value_field value_constr]
        where [constr_field] is the name of the constructor field,
        [constr_constr] is the constructor field value, [value_field] is the
        name of the value field, and [value_constr] is the constructor used.
    *)

  module Adapter : sig

    (** Module signature required of any json adapter.
        For example, an ATD annotation
        [<json adapter="atdgen_runtime.util.json.adapter.type_field.default"]
        refers to the OCaml module
        [Atdgen_runtime.Util.Json.Adapter.Type_field.Default].
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
] <json adapter="atdgen_runtime.util.json.adapter.type_field.default">

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

      (** Adapter assuming a ["type"] field. *)
      module Default : S

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
] <json adapter="atdgen_runtime.util.json.adapter.one_field">

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
  end
end

module Validation :
sig
  type path_elem = [ `Field of string | `Index of int ]
  type path = path_elem list
    (** Path within a value, used to report validation errors. *)

  val string_of_path : path -> string
    (** Reverse and concatenate a path into a string
        such as [".settings.ports[0]"] *)

  type error = {
    error_path : path;
    error_msg : string option;
  }

  val error : ?msg: string -> path -> error
  val string_of_error : error -> string
end
