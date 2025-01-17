(*
   Entry point to the atddiff command.
*)

open Printf
open Cmdliner

type conf = {
  old_file: string;
  new_file: string;
  out_file: string option;
  filter: Atddiff.filter;
  root_types_superset: string list option;
  json_defaults_old: bool;
  json_defaults_new: bool;
  with_locations: bool;
  output_format: Atddiff.output_format;
  exit_success: bool;
  version: bool;
}

let ok_exit = 0
let error_exit = 1
let bug_exit = 2
let finding_exit = 3

let exit_info = [
  ok_exit, "atddiff completed successfully and either found nothing to report \
            or found issues but the --exit-success option was on.";
  error_exit, "User error: atddiff failed due to invalid command-line \
               options, missing files etc.";
  bug_exit, "Internal error: atddiff failed due to a bug (including \
             uncaught exceptions).";
  finding_exit, "atddiff successfully found one or more issues to report.";
] |> List.map (fun (code, doc) -> Cmd.Exit.info ~doc code)

let print_version_and_exit () =
  print_endline Atddiff.version;
  exit ok_exit

let run conf =
  if conf.version then
    print_version_and_exit ()
  else
    let out_data =
      Atddiff.compare_files
        ?root_types_superset:conf.root_types_superset
        ~filter:conf.filter
        ~json_defaults_old:conf.json_defaults_old
        ~json_defaults_new:conf.json_defaults_new
        ~output_format:conf.output_format
        ~with_locations:conf.with_locations
        conf.old_file conf.new_file in
    let exit_code, data =
      match out_data with
      | Ok () ->
          ok_exit, ""
      | Error data ->
          (if conf.exit_success then ok_exit else finding_exit), data
    in
    (match conf.out_file with
     | None -> print_string data
     | Some out_file ->
         let oc = open_out_bin out_file in
         Fun.protect
           ~finally:(fun () -> close_out_noerr oc)
           (fun () -> output_string oc data)
    );
    exit exit_code

(***************************************************************************)
(* Command-line processing *)
(***************************************************************************)

let error msg =
  eprintf "Error: %s\n%!" msg;
  exit error_exit

let old_file_term : string Term.t =
  let info =
    Arg.info []
      ~docv:"OLD_ATD_FILE"
      ~doc:"Path to the older version of the ATD file to compare"
  in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None info)

let new_file_term : string Term.t =
  let info =
    Arg.info []
      ~docv:"NEW_ATD_FILE"
      ~doc:"Path to the newer version of the ATD file to compare"
  in
  Arg.required (Arg.pos 1 (Arg.some Arg.file) None info)

let out_file_term : string option Term.t =
  let info =
    Arg.info ["o"; "output-file"]
      ~docv:"OUTPUT_FILE"
      ~doc:"Path to the output file. The default is to print the result to \
            standard output."
  in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

let backward_term : bool Term.t =
  let info =
    Arg.info ["backward"]
      ~doc:"Ignore findings other than backward incompatibilies."
  in
  Arg.value (Arg.flag info)

let forward_term : bool Term.t =
  let info =
    Arg.info ["forward"]
      ~doc:"Ignore findings other than forward incompatibilies."
  in
  Arg.value (Arg.flag info)

let types_term : string list option Term.t =
  let info =
    Arg.info ["types"]
      ~docv:"TYPE_NAME1,TYPE_NAME2,..."
      ~doc:"Select findings that affect these types. If no '--type' filter is \
            provided, all types are selected. For example, \
            '--type foo,bar' selects the findings that affect type 'foo', \
            type 'bar', or both."
  in
  Arg.value (Arg.opt (Arg.some (Arg.list Arg.string)) None info)

let ignore_term : string list option Term.t =
  let info =
    Arg.info ["ignore"]
      ~docv:"TYPE_NAME1,TYPE_NAME2,..."
      ~doc:"Specify the root types that may not have been selected with \
            '--types'. A root type (or entry point) is a type defined in \
            an ATD file that \
            is not a dependency of the other root types. \
            Note that in the case of mutually recursive type definitions, the \
            assignment of root types may not be unique. \
            When the '--ignore' option is used, the union \
            of the types specified with '--types' and '--ignore' \
            determines the set of allowed root types. \
            Any type found in an ATD file that is not a dependency \
            of any of the allowed root types causes atddiff to fail \
            with a nonzero exit status. \
            This ensures that a newly-added root type won't be \
            accidentally ignored if the programmer forgets \
            to update the argument of '--types'."
  in
  Arg.value (Arg.opt (Arg.some (Arg.list Arg.string)) None info)

let json_defaults_term : bool Term.t =
  let info =
    Arg.info ["json-defaults"]
      ~doc:"Shorthand for '--json-defaults-old --json-defaults-new'."
  in
  Arg.value (Arg.flag info)

let json_defaults_old_term : bool Term.t =
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

let json_defaults_new_term : bool Term.t =
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

let output_format_term : Atddiff.output_format Term.t =
  let info =
    Arg.info ["output-format"; "f"]
      ~doc:(
        "Output JSON instead of text. The format is specified by the file \
         Atddiff_output.atd that's included in the source distribution of \
         ATD. At the time of writing, its location is \
         https://github.com/ahrefs/atd/blob/master/atddiff/src/lib/Atddiff_output.atd")
  in
  Arg.value (Arg.opt (Arg.enum ["text", Atddiff.Text;
                                "json", Atddiff.JSON])
               Atddiff.Text info)

let no_locations_term : bool Term.t =
  let info =
    Arg.info ["no-locations"]
      ~doc:"[EXPERIMENTAL] \
            Omit file/line/column information when printing findings. \
            This is intended for maximizing the stability of the atddiff \
            reports so that diffing successive reports gives meaningful \
            results. This causes the results to be sorted by hash rather \
            than by location. The hex hash of each result is shown in both \
            text and JSON output. It is guaranteed to be the same for \
            a given version of atddiff compiled with a given version of \
            OCaml. In practice, it is not expected to change \
            often and can be used to track findings over time. Distinct \
            findings will have the same hash if they have \
            the same error message up to location."
  in
  Arg.value (Arg.flag info)

let exit_success_term : bool Term.t =
  let info =
    Arg.info ["exit-success"]
      ~doc:(sprintf "Exit with success status %i instead of %i if there are \
                     type incompatibilities to report."
              ok_exit
              finding_exit)
  in
  Arg.value (Arg.flag info)

let version_term : bool Term.t =
  let info =
    Arg.info ["version"]
      ~doc:"Print the version of atddiff and exits."
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
      changes in JSON data that can cause some incompatibilities. \
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
      backward forward types ignore_
      json_defaults json_defaults_old json_defaults_new
      output_format no_locations
      exit_success version =
    let filter =
      let module A = Atddiff in
      let backward = if backward then [A.Filter (A.Backward)] else [] in
      let forward = if forward then [A.Filter (A.Forward)] else [] in
      let types =
        match types with
        | None -> []
        | Some types ->
            [A.Or
               (List.map (fun name ->
                  A.Filter (A.Affected_type_name name))
               types)
            ]
      in
      A.And (List.concat [backward; forward; types])
    in
    let root_types_superset =
      match types, ignore_ with
      | Some selected_root_types, Some ignore_ ->
          Some (selected_root_types @ ignore_)
      | None, Some ignore_ ->
          Some ignore_
      | None, None
      | Some _, None -> None
    in
    let json_defaults_old = json_defaults_old || json_defaults in
    let json_defaults_new = json_defaults_new || json_defaults in
    run {
      old_file;
      new_file;
      out_file;
      filter;
      root_types_superset;
      json_defaults_old;
      json_defaults_new;
      output_format;
      with_locations = not no_locations;
      exit_success;
      version;
    }
  in
  Term.(const combine
        $ old_file_term
        $ new_file_term
        $ out_file_term
        $ backward_term
        $ forward_term
        $ types_term
        $ ignore_term
        $ json_defaults_term
        $ json_defaults_old_term
        $ json_defaults_new_term
        $ output_format_term
        $ no_locations_term
        $ exit_success_term
        $ version_term
       )

let parse_command_line_and_run run =
  match Sys.argv with
  (* gross, but avoids complaints about missing input files *)
  | [| _; "--version" |] ->
      print_version_and_exit ()
  | _ ->
      let info =
        Cmd.info
          ~doc
          ~exits:exit_info
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
