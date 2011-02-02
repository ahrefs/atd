(* $Id: ag_main.ml 52381 2010-11-24 22:40:21Z martin $ *)

open Printf

let set_once varname var x =
  match !var with
      Some y ->
        if x <> y then
          failwith (sprintf "\
Command-line parameter %S is set multiple times
to incompatible values."
                      varname)
        
    | None ->
        var := Some x
          

let main () =
  let pos_fname = ref None in
  let pos_lnum = ref None in
  let files = ref [] in
  let opens = ref [] in
  let with_typedefs = ref true in
  let with_fundefs = ref true in
  let all_rec = ref false in
  let out_prefix = ref None in
  let serialization_format = ref None in
  let std_json = ref false in
  let type_aliases = ref None in
  let set_opens s =
    let l = Str.split (Str.regexp " *, *\\| +") s in
    opens := List.rev_append l !opens
  in
  let options = [
    "-biniou",
    Arg.Unit (fun () ->
                set_once "serialization format" serialization_format `Biniou),
    "
          Write serializers and deserializers for Biniou (default).";

    "-extend", Arg.String (fun s -> type_aliases := Some s),
    "MODULE
          Assume that all type definitions are provided by the specified
          module unless otherwise annotated.  Type aliases are created
          for each type, e.g.
            type t = Module.t";

    "-json",
    Arg.Unit (fun () ->
                set_once "serialization format" serialization_format `Json),
    "
          Write serializers and deserializers for JSON.";

    "-nfd", Arg.Clear with_fundefs,
    "
          Do not dump OCaml function definitions";
    
    "-ntd", Arg.Clear with_typedefs,
    "
          Do not dump OCaml type definitions";

    "-o", Arg.String (fun s ->
                        let out =
                          match s with
                              "-" -> `Stdout
                            | s -> `Files s
                        in
                        set_once "output prefix" out_prefix out),
    "[ PREFIX | - ]
          Use this prefix for the generated files, e.g. 'foo/bar' for
          foo/bar.ml and foo/bar.mli.
          `-' designates stdout and produces code of the form
            struct ... end : sig ... end";

    "-open", Arg.String set_opens,
    "MODULE1,MODULE2,...
          List of modules to open (comma-separated or space-separated)";

    "-pos-fname", Arg.String (set_once "pos-fname" pos_fname),
    "FILENAME
          Source file name to use for error messages
          (default: input file name)";

    "-pos-lnum", Arg.Int (set_once "pos-lnum" pos_lnum),
    "LINENUM
          Source line number of the first line of the input (default: 1)";

    "-rec", Arg.Set all_rec,
    "
          Keep OCaml type definitions mutually recursive";

    "-std-json",
    Arg.Unit (fun () ->
                std_json := true;
                set_once "serialization format" serialization_format `Json),
    "
          Convert tuples and variants into standard JSON and
          refuse to print NaN and infinities (implying -json).";

    "-version",
    Arg.Unit (fun () ->
                print_endline Ag_version.version;
                exit 0),
    "
          Print the version identifier of atdgen and exit.";
  ]
  in
  let msg = sprintf "\
Generate OCaml serializers and deserializers.
Default serialization format is biniou.
Usage: %s FILE.atd" Sys.argv.(0) in
  Arg.parse options (fun file -> files := file :: !files) msg;

  let format =
    match !serialization_format with
        None -> `Biniou
      | Some x -> x
  in
  let atd_file =
    match !files with
	[s] -> Some s
      | [] -> None
      | _ ->
	  Arg.usage options msg;
	  exit 1
  in
  let ocaml_prefix =
    match !out_prefix, atd_file with
	Some x, _ -> x
      | None, Some file ->
	  `Files (
            if Filename.check_suffix file ".atd" then
	      Filename.chop_extension file
	    else
	      file
          )
      | None, None -> `Stdout
  in
  let opens = List.rev !opens in
  let make_ocaml_files =
    match format with
        `Biniou -> Ag_ob_emit.make_ocaml_files
      | `Json -> Ag_oj_emit.make_ocaml_files ~std: !std_json
  in
  make_ocaml_files
    ~opens
    ~with_typedefs: !with_typedefs
    ~with_fundefs: !with_fundefs
    ~all_rec: !all_rec
    ~pos_fname: !pos_fname
    ~pos_lnum: !pos_lnum
    ~type_aliases: !type_aliases
    atd_file ocaml_prefix
    
let () =
  try main ()
  with
      Atd_ast.Atd_error s
    | Failure s ->
        flush stdout;
	eprintf "%s\n%!" s;
	exit 1
    | e -> raise e
