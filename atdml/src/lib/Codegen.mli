(**
   OCaml code generation for JSON support using the Yojson AST.

   For each ATD type [foo], the generated code contains:
   - A type definition [type foo = ...]
   - A creation function [val make_foo ...] (for record types only)
   - A deserialization function [val foo_of_yojson : Yojson.Safe.t -> foo]
   - A serialization function [val yojson_of_foo : foo -> Yojson.Safe.t]
   - Top-level I/O functions
   - A submodule [module Foo]
*)

val run_file : yojson:bool -> jsonlike:bool -> string -> unit
(** [run_file ~yojson ~jsonlike src_path] reads an ATD file at [src_path]
    and generates OCaml source files [<basename>.ml] and [<basename>.mli] in
    the current directory, where [<basename>] is derived from [src_path] by
    removing the [.atd] extension and lowercasing.
    [~yojson] controls whether [Yojson.Safe.t]-based readers/writers are
    generated (default [true]).
    [~jsonlike] controls whether [Atd_jsonlike.AST.t]-based readers and writers are
    generated (default [true]). *)

val run_stdin : yojson:bool -> jsonlike:bool -> unit -> unit
(** [run_stdin ~yojson ~jsonlike ()] reads ATD source from stdin and writes
    a self-contained OCaml snippet to stdout of the form:
    {[
      module type Types = sig ... end
      module Types : Types = struct ... end
    ]}
    The snippet can be copy-pasted into utop or ocaml. *)
