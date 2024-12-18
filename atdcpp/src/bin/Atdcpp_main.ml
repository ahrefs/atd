(*
   Entry point to the Atdcpp command.
*)

open Printf
open Cmdliner

type conf = {
  input_files: string list;
  tags: string list;
  version: bool;
}

let run conf =
  if conf.version then (
    print_endline Atdcpp.Version.version;
    exit 0
  )
  else
    conf.input_files
    |> List.iter (fun atd_file ->
      Atdcpp.Codegen.run_file ~tags:conf.tags atd_file
    )

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

let tags_term =
  let info =
    Arg.info ["tag"]
      ~doc:"Only evaluate annotations which have either the provided TAG as a field value <... tag=TAG>, or have no tags specified.\nOption can be used multiple times to specify several tags"
  in
  Arg.value (Arg.opt_all Arg.string [] info)

let version_term =
  let info =
    Arg.info ["version"]
      ~doc:"Prints the version of Atdcpp and exits"
  in
  Arg.value (Arg.flag info)

let doc =
  "Type-safe JSON serializers for D"

(*
   The structure of the help page.
*)
let man = [
  (* 'NAME' and 'SYNOPSIS' sections are inserted here by cmdliner. *)

  `S Manpage.s_description;  (* standard 'DESCRIPTION' section *)
  `P "Atdcpp turns a file containing type definitions into D classes \
      that read, write, and validate JSON data. The generated code \
      can be type-checked statically upon compilation to ensure user code agrees \
      with the ATD interface.";

  (* 'ARGUMENTS' and 'OPTIONS' sections are inserted here by cmdliner. *)

  `S Manpage.s_examples; (* standard 'EXAMPLES' section *)
  `P "The following is a sample ATD file. 'sample.atd' becomes 'sample.d' \
      with the command 'Atdcpp sample.atd'.";
  `Pre "\
(* Sample ATD file sample.atd *)

type foo = {
  name: string;                 (* required field *)
  ?description: string option;  (* optional field *)
  ~tags: string list;           (* optional with implicit default *)
  ~price <cpp default=\"0.99\">: float;  (* explicit default *)
  items: bar list;
}

(* sum type *)
type bar = [
  | Thing <json name=\"thing\"> of int
  | Nothing <json name=\"nothing\">
]
";

  `S Manpage.s_authors;
  `P "Martin Jambon <martin@r2c.dev>";

  `S Manpage.s_bugs;
  `P "Report issues at https://github.com/ahrefs/atd";

  `S Manpage.s_see_also;
  `P "atdgen, atdj, atds, atdts"
]

let cmdline_term run =
  let combine input_files tags version =
    run {
      input_files;
      version;
      tags;
    }
  in
  Term.(const combine
        $ input_files_term
        $ tags_term
        $ version_term
       )

let parse_command_line_and_run run =
  let info =
    Cmd.info
      ~doc
      ~man
      "Atdcpp"
  in
  Cmd.v info (cmdline_term run) |> Cmd.eval |> exit

let safe_run conf =
  try run conf
  with
  (* for other exceptions, we show a backtrace *)
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
