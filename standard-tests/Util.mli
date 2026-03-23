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
