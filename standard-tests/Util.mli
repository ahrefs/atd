(**
   Utilities for running tests from OCaml
*)

(** Run the specified command and log useful info such as current working
    directory, the command being invoked, and its termination status.
    In case a non-successful exit, a [Failure] exception is raised.

    The implementation may or may not use a shell.
*)
val run_command : string list -> unit

(** Log a command like [run_command] does. *)
val log_command : string list -> unit

(** Sort the key/value pairs alphabetically by key, preserving
    the original order in case of duplicate keys (stable sort).

    Format the normalized tree with consistent indentation, one item
    per line to make diffs easy to read.

    This normalization is suggested before comparing two JSON trees
    when key order doesn't matter. If key order matters, use
    [format_json_naively] or [format_yojson_naively] instead.
*)
val normalize_json : string -> string
val normalize_yojson : Yojson.Safe.t -> string

(** Pretty-print JSON in a naive way that's not very compact but is stable
    from one version of Yojson to the next and makes diffs more readable
    by not putting more than one leaf node per line. *)
val format_json_naively : string -> string
val format_yojson_naively : Yojson.Safe.t -> string
