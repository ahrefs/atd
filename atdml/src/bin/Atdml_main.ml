(*
   Entry point to the atdml command.
*)

open Printf
open Cmdliner

type conf = {
  input_files: string list;
  version: bool;
}

let run conf =
  if conf.version then (
    print_endline Atdml.Version.version;
    exit 0
  )
  else
    match conf.input_files with
    | [] -> Atdml.Codegen.run_stdin ()
    | files -> List.iter Atdml.Codegen.run_file files

(***************************************************************************)
(* Command-line processing *)
(***************************************************************************)

let error msg =
  eprintf "Error: %s\n%!" msg;
  exit 1

let input_files_term =
  let info =
    Arg.info []  (* list must be empty for anonymous arguments *)
      ~docv:"PATH"
      ~doc:"Input file in the ATD format with the '.atd' extension"
  in
  let default = [] in
  Arg.value (Arg.pos_all Arg.file default info)

let version_term =
  let info =
    Arg.info ["version"]
      ~doc:"Print the version of atdml and exit"
  in
  Arg.value (Arg.flag info)

let doc =
  "Simplified OCaml JSON serializers using the Yojson AST"

let man = [
  `S Manpage.s_description;
  `P "atdml turns ATD type definitions into an OCaml module that serializes \
      and deserializes values to and from JSON via the Yojson.Safe.t \
      intermediate representation.";

  `P "Unlike atdgen, atdml does not use semi-secret Yojson streaming \
      functions. This makes the generated code easier to read and \
      understand at the cost of some performance.";

  `P "When given a file, atdml writes 'foo.ml' and 'foo.mli' next to it. \
      When reading from stdin, atdml writes a self-contained OCaml snippet \
      to stdout that can be copy-pasted into utop or ocaml:";
  `Pre "module type Types = sig ... end\nmodule Types : Types = struct ... end";

  `S Manpage.s_examples;
  `P "Generate 'foo.ml' and 'foo.mli' from 'foo.atd':";
  `Pre "atdml foo.atd";
  `P "Generate a self-contained snippet from stdin:";
  `Pre "atdml < foo.atd";
  `P "Sample ATD file 'foo.atd':";
  `Pre "\
type color = [
  | Red
  | Green
  | Blue
]

type point = {
  x: float;
  y: float;
  ?label: string option;
}
";
  `P "The generated code exposes functions and submodules such as:";
  `Pre "\
val color_of_yojson : Yojson.Safe.t -> color
val yojson_of_color : color -> Yojson.Safe.t
val color_of_json : string -> color
val json_of_color : color -> string

module Color : sig
  type nonrec t = color
  val of_yojson : Yojson.Safe.t -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_json : string -> t
  val to_json : t -> string
end

val make_point : x:float -> y:float -> ?label:string -> unit -> point
val point_of_yojson : Yojson.Safe.t -> point
val yojson_of_point : point -> Yojson.Safe.t
val point_of_json : string -> point
val json_of_point : point -> string

module Point : sig
  type nonrec t = point
  val make : x:float -> y:float -> ?label:string -> unit -> t
  val of_yojson : Yojson.Safe.t -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_json : string -> t
  val to_json : t -> string
end
";

  `S Manpage.s_authors;
  `P "See https://github.com/ahrefs/atd for the list of contributors.";

  `S Manpage.s_bugs;
  `P "Report issues at https://github.com/ahrefs/atd";

  `S Manpage.s_see_also;
  `P "atdgen, atdpy, atdts"
]

let cmdline_term run =
  let combine input_files version =
    run {
      input_files;
      version;
    }
  in
  Term.(const combine
        $ input_files_term
        $ version_term
       )

let parse_command_line_and_run run =
  let info =
    Cmd.info
      ~doc
      ~man
      "atdml"
  in
  Cmd.v info (cmdline_term run) |> Cmd.eval |> exit

let safe_run conf =
  try run conf
  with
  | Failure msg -> error msg
  | Atd.Ast.Atd_error msg -> error msg
  | e ->
      let trace = Printexc.get_backtrace () in
      eprintf "Error: exception %s\n%s%!"
        (Printexc.to_string e)
        trace

let main () =
  Printexc.record_backtrace true;
  let conf = parse_command_line_and_run safe_run in
  safe_run conf

let () = main ()
