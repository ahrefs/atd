(**
   OCaml code generation for JSON support using the Yojson AST.

   For each ATD type [foo], the generated code contains:
   - A type definition [type foo = ...]
   - A creation function [val make_foo ...] (for record types only)
   - A deserialization function [val foo_of_yojson : Yojson.Safe.t -> foo]
   - A serialization function [val yojson_of_foo : foo -> Yojson.Safe.t]
   - Top-level I/O functions (for non-parametrized types only)

   Two files are produced: [foo.ml] and [foo.mli].
*)

val run_file : string -> unit
(** [run_file src_path] reads an ATD file at [src_path] and generates
    OCaml source files [<basename>.ml] and [<basename>.mli] in the current
    directory, where [<basename>] is derived from [src_path] by removing the
    [.atd] extension and lowercasing. *)
