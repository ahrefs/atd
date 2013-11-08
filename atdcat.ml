

open Printf

let html_of_doc loc s =
  let doc = Atd_doc.parse_text loc s in
  Atd_doc.html_of_doc doc

let format_html_comments ((section, (loc, l)) as x) =
  match section with
      "doc" ->
        (try
           match List.assoc "html" l with
               (loc, Some s) ->
                 let comment = "(*html " ^ s ^ "*)" in
                 Easy_format.Atom (comment, Easy_format.atom)
             | _ -> raise Not_found
         with Not_found ->
           match List.assoc "text" l with
               (loc, Some s) ->
                 let comment = "(*html " ^ html_of_doc loc s ^ "*)" in
                 Easy_format.Atom (comment, Easy_format.atom)
             | _ ->
                 Atd_print.default_annot x
        )
    | _ ->
        Atd_print.default_annot x

let print_atd ~html_doc ~exclude_names ast =
  let annot =
    if html_doc then Some format_html_comments
    else None
  in
  let filter_names =
    match exclude_names with
      None -> None
    | Some str ->
      let re = Str.regexp str
      in
      Some (fun s -> not (Str.string_match re s 0))
  in
  let pp = Atd_print.format ?annot ?filter_names ast in
  Easy_format.Pretty.to_channel stdout pp;
  print_newline ()

let print_ml ~name ast =
  let buf = Buffer.create 1000 in
  Atd_reflect.print_full_module_def buf name ast;
  print_string (Buffer.contents buf);
  print_newline ()

let strip all sections x =
  let filter =
    if all then
      fun l -> []
    else
      List.filter (fun (name, fields) -> not (List.mem name sections))
  in
  Atd_ast.map_all_annot filter x

let parse
    ~expand ~keep_poly ~xdebug ~inherit_fields ~inherit_variants
    ~strip_all ~strip_sections files =
  let l =
    List.map (
      fun file ->
        Atd_util.load_file ~expand ~keep_poly ~xdebug
          ~inherit_fields ~inherit_variants file
    ) files
  in
  let heads, bodies = List.split l in
  let first_head =
    (* heads in other files than the first one are tolerated but ignored *)
    match heads with
        x :: l -> x
      | [] -> (Atd_ast.dummy_loc, [])
  in
  let m = first_head, List.flatten bodies in
  strip strip_all strip_sections m

let print ~html_doc ~out_format ~exclude_names ast =
  let f =
    match out_format with
        `Atd -> print_atd ~html_doc ~exclude_names
      | `Ocaml name -> print_ml ~name
  in
  f ast

let split_on_comma =
  Str.split_delim (Str.regexp ",")

let () =
  let expand = ref false in
  let keep_poly = ref false in
  let xdebug = ref false in
  let inherit_fields = ref false in
  let inherit_variants = ref false in
  let exclude_names = ref None in
  let strip_sections = ref [] in
  let strip_all = ref false in
  let out_format = ref `Atd in
  let html_doc = ref false in
  let files = ref [] in

  let options = [
    "-x", Arg.Set expand,
    "
          make type expressions monomorphic";

    "-xk", Arg.Unit (fun () -> expand := true; keep_poly := true),
    "
          keep parametrized type definitions and imply -x.
          Default is to return only monomorphic type definitions";

    "-xd", Arg.Unit (fun () -> expand := true; xdebug := true),
    "
          debug mode implying -x";

    "-i", Arg.Unit (fun () ->
                      inherit_fields := true;
                      inherit_variants := true),
    "
          expand all `inherit' statements";

    "-if", Arg.Set inherit_fields,
    "
          expand `inherit' statements in records";

    "-iv", Arg.Set inherit_variants,
    "
          expand `inherit' statements in sum types";

    "-ml", Arg.String (fun s -> out_format := `Ocaml s),
    "<name>
          output the ocaml code of the ATD abstract syntax tree";

    "-exclude-names", Arg.String (fun s -> exclude_names := Some(s)),
    "<regexp>
          definitions of type names matching the regexp will not be pretty-printed (ATD mode only)";

    "-html-doc", Arg.Set html_doc,
    "
          replace directly <doc html=\"...\"> by (*html ... *)
          or replace <doc text=\"...\"> by (*html ... *)
          where the contents are formatted as HTML
          using <p>, <code> and <pre>.
          This is suitable input for \"caml2html -ext html:cat\"
          which converts ATD files into HTML.";

    "-strip",
    Arg.String (fun s -> strip_sections := split_on_comma s @ !strip_sections),
    "NAME1[,NAME2,...]
          remove all annotations of the form <NAME1 ...>,
          <NAME2 ...>, etc.";

    "-strip-all", Arg.Set strip_all,
    "
          remove all annotations";

    "-version",
    Arg.Unit (fun () ->
                print_endline Atd_version.version;
                exit 0),
    "
          print the version of atd and exit";
  ]
  in
  let msg = sprintf "Usage: %s FILE" Sys.argv.(0) in
  Arg.parse options (fun file -> files := file :: !files) msg;
  try
    let ast =
      parse
          ~expand: !expand
          ~keep_poly: !keep_poly
          ~xdebug: !xdebug
          ~inherit_fields: !inherit_fields
          ~inherit_variants: !inherit_variants
          ~strip_all: !strip_all
          ~strip_sections: !strip_sections
          !files
    in
    print ~html_doc: !html_doc ~out_format: !out_format ~exclude_names: !exclude_names ast
  with
      Atd_ast.Atd_error s ->
        flush stdout;
        eprintf "%s\n%!" s
    | e -> raise e
