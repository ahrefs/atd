(*
   Entry point to the atddiff command.
*)

open Printf
open Cmdliner

type conf = {
  old_file: string;
  new_file: string;
  out_file: string option;
  json_defaults_old: bool;
  json_defaults_new: bool;
  version: bool;
}

let run conf =
  if conf.version then (
    print_endline Atddiff.version;
    exit 0
  )
  else
    let out_data =
      Atddiff.compare_files
        ~json_defaults_old:conf.json_defaults_old
        ~json_defaults_new:conf.json_defaults_new
        conf.old_file conf.new_file in
    match conf.out_file with
    | None -> print_string out_data
    | Some out_file ->
       let oc = open_out_bin out_file in
       Fun.protect
         ~finally:(fun () -> close_out_noerr oc)
         (fun () -> output_string oc out_data)

(***************************************************************************)
(* Command-line processing *)
(***************************************************************************)

let error msg =
  eprintf "Error: %s\n%!" msg;
  exit 1

let old_file_term =
  let info =
    Arg.info []
      ~docv:"OLD_ATD_FILE"
      ~doc:"Path to the older version of the ATD file to compare"
  in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None info)

let new_file_term =
  let info =
    Arg.info []
      ~docv:"NEW_ATD_FILE"
      ~doc:"Path to the newer version of the ATD file to compare"
  in
  Arg.required (Arg.pos 1 (Arg.some Arg.file) None info)

let out_file_term =
  let info =
    Arg.info ["o"; "output-file"]
      ~docv:"OUTPUT_FILE"
      ~doc:"Path to the output file. The default is to print the result to \
            standard output."
  in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

let json_defaults_term =
  let info =
    Arg.info ["json-defaults"]
      ~doc:"Shorthand for '--json-defaults-old --json-defaults-new'."
  in
  Arg.value (Arg.flag info)

let json_defaults_old_term =
  let info =
    Arg.info ["json-defaults-old"]
      ~doc:"Assume that old implementations emitting JSON populate \
            optional fields with a value when a default exists. This applies \
            to all the fields marked with a '~'. For example, a field \
            declared as '~items: item list' defaults to the empty list. \
            This option makes atddiff assume that '~' fields behave like \
            required fields in JSON for old implementations. \
            For example, when using atdgen it is achieved with '-j-defaults'."
  in
  Arg.value (Arg.flag info)

let json_defaults_new_term =
  let info =
    Arg.info ["json-defaults-new"]
      ~doc:"Assume that new implementations emitting JSON populate \
            optional fields with a value when a default exists. This applies \
            to all the fields marked with a '~'. For example, a field \
            declared as '~items: item list' defaults to the empty list. \
            This option makes atddiff assume that '~' fields behave like \
            required fields in JSON for new implementations. \
            For example, when using atdgen it is achieved with '-j-defaults'."
  in
  Arg.value (Arg.flag info)

let version_term =
  let info =
    Arg.info ["version"]
      ~doc:"Prints the version of atddiff and exits"
  in
  Arg.value (Arg.flag info)

let doc =
  "Assess the compatibility of two versions of the same ATD interface"

(*
   The structure of the help page.
*)
let man = [
  (* 'NAME' and 'SYNOPSIS' sections are inserted here by cmdliner. *)

  `S Manpage.s_description;  (* standard 'DESCRIPTION' section *)
  `P "Atddiff compares two versions of the same ATD file and reports \
      changes that cause some incompatibilities. \
      Incompatibilities are of two kinds: forward and backward. \
      Backward compatibility refers to the ability to read older data \
      or data produced by an older implementation using a newer \
      implementation. Conversely, forward compatibility is the ability \
      to read data produced by a newer implementation. For example, if \
      a new field is removed from a record type and was it not \
      optional, it makes it impossible for an older implementation \
      to read data from a newer implementation that lacks the field in \
      question. Typically, data comes from \
      storage (databases, configuration files, ...), from client requests, \
      or from server responses. All these sources of data may suffer from \
      being 'too old' (backward-incompatible) or 'too new' \
      (forward-incompatible) for the reader. Atddiff helps developers \
      protect themselves against unintentional breaking changes \
      without being ATD experts.";

  `P "Git users will find it convenient to run 'atddiff' via the command \
      'git difftool -x atddiff', allowing them to select two versions of the \
      same file as they usually do with 'git diff'.";

  (* 'ARGUMENTS' and 'OPTIONS' sections are inserted here by cmdliner. *)
  `S Manpage.s_authors;
  `P "Martin Jambon <martin@semgrep.com>";

  `S Manpage.s_see_also;
  `P "atdcat"
]

let cmdline_term run =
  let combine
      old_file new_file out_file
      json_defaults json_defaults_old json_defaults_new
      version =
    let json_defaults_old = json_defaults_old || json_defaults in
    let json_defaults_new = json_defaults_new || json_defaults in
    run {
      old_file;
      new_file;
      out_file;
      json_defaults_old;
      json_defaults_new;
      version;
    }
  in
  Term.(const combine
        $ old_file_term
        $ new_file_term
        $ out_file_term
        $ json_defaults_term
        $ json_defaults_old_term
        $ json_defaults_new_term
        $ version_term
       )

let parse_command_line_and_run run =
  let info =
    Cmd.info
      ~doc
      ~man
      "atddiff"
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