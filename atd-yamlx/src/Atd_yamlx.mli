(** Bridge between YAMLx and the ATD jsonlike AST.

    This library translates a parsed YAML value — a [YAMLx.value] produced by
    the {{:https://github.com/mjambon/yamlx} yamlx} library — into an
    [Atd_jsonlike.AST.t] node.  The key property preserved by the translation
    is {b source location}: every node in the resulting tree carries the file
    path and line/column range from which it was parsed, enabling ATD-generated
    reader functions (e.g. [foo_of_jsonlike]) to report precise error messages.

    Typical usage:

    {[
      let file = "config.yaml" in
      let yaml_text = In_channel.input_all (open_in file) in
      match YAMLx.Values.one_of_yaml ~file yaml_text with
      | Error msg -> failwith msg
      | Ok yaml_val ->
          match Atd_yamlx.of_yamlx_value ~file yaml_val with
          | Error msg -> failwith msg
          | Ok jsonlike ->
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

    Map keys must be YAML strings.  Any other key type produces an error.
    If a non-string key is needed, pre-process the YAML document to convert
    it before calling this function.
*)

(** Convert a [YAMLx.value] to [Atd_jsonlike.AST.t], preserving source
    locations.  Returns [Error msg] if a YAML map has a non-string key.

    @param file
      The file path from which the YAML was read.  When provided it is
      attached to every [Atd_jsonlike.Loc.t] in the resulting tree so that
      error messages produced by ATD-generated readers include the file name.
      Matches the [?file] argument of [YAMLx.Values.of_yaml] /
      [YAMLx.Values.one_of_yaml_file].
*)
val of_yamlx_value :
  ?file:string -> YAMLx.value -> (Atd_jsonlike.AST.t, string) result

(** Like {!of_yamlx_value} but raises [Invalid_argument] instead of returning
    [Error] when a YAML map has a non-string key.  The error message includes
    the source location of the offending key. *)
val of_yamlx_value_exn : ?file:string -> YAMLx.value -> Atd_jsonlike.AST.t

(** Convert an [Atd_jsonlike.AST.t] to a [YAMLx.value].

    This is the reverse of {!of_yamlx_value_exn}.  Source locations in the
    input are ignored; the resulting [YAMLx.value] nodes all carry
    {!YAMLx.zero_loc}.

    {1 Jsonlike–YAML type correspondence}

    | Jsonlike (atd-jsonlike)   | YAML (yamlx)                      |
    |---------------------------|-----------------------------------|
    | [Null]                    | [Null]                            |
    | [Bool]                    | [Bool]                            |
    | [Number] (int available)  | [Int] (int64)                     |
    | [Number] (float only)     | [Float]                           |
    | [Number] (literal only)   | [String] (verbatim literal)       |
    | [String]                  | [String]                          |
    | [Array]                   | [Seq]                             |
    | [Object]                  | [Map] (string keys)               |
*)
val to_yamlx_value : Atd_jsonlike.AST.t -> YAMLx.value
