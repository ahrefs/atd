(** Bridge between YAMLx and the ATD jsonlike AST.

    This library translates a parsed YAML value — a [YAMLx.value] produced by
    the {{:https://github.com/mjambon/yamlx} yamlx} library — into an
    [Atd_jsonlike.AST.t] node.  The key property preserved by the translation
    is {b source location}: every node in the resulting tree carries the file
    path and line/column range from which it was parsed, enabling ATD-generated
    reader functions (e.g. [foo_of_jsonlike]) to report precise error messages.

    Typical usage:

    {[
      let path = "config.yaml" in
      let yaml_text = In_channel.input_all (open_in path) in
      match YAMLx.Values.one_of_yaml ~file:path yaml_text with
      | Error msg -> failwith msg
      | Ok yaml_val ->
          let jsonlike = Atd_yamlx.of_yamlx_value ~path yaml_val in
          (* Feed to an ATD-generated reader, e.g. *)
          let config = My_config_j.config_of_jsonlike jsonlike in
          ...
    ]}

    {1 YAML–JSON type correspondence}

    | YAML (yamlx)           | Jsonlike (atd-jsonlike)           |
    |-------------------------|-----------------------------------|
    | [Null]                  | [Null]                            |
    | [Bool]                  | [Bool]                            |
    | [Int] (int64)           | [Number] (via [Number.of_int] or  |
    |                         | [Number.of_string_opt])           |
    | [Float]                 | [Number] (via [Number.of_float])  |
    | [String]                | [String]                          |
    | [Seq] (sequence/array)  | [Array]                           |
    | [Map] (mapping/object)  | [Object]                          |

    Map keys must be scalar YAML values ([Null], [Bool], [Int], [Float], or
    [String]).  Sequence or mapping keys raise [Invalid_argument].
*)

(** Convert a [YAMLx.value] to [Atd_jsonlike.AST.t], preserving source
    locations.

    @param path
      The file path from which the YAML was read.  When provided it is
      attached to every [Atd_jsonlike.Loc.t] in the resulting tree so that
      error messages produced by ATD-generated readers include the file name.
      Matches the [?file] argument of [YAMLx.Values.of_yaml] /
      [YAMLx.Values.one_of_yaml_file].

    @raise Invalid_argument
      when a YAML map has a complex key (a sequence or a nested map) that
      cannot be represented as a plain string.  All scalar key types
      ([Null], [Bool], [Int], [Float], [String]) are accepted and
      converted to their natural string representation.
*)
val of_yamlx_value : ?path:string -> YAMLx.value -> Atd_jsonlike.AST.t
