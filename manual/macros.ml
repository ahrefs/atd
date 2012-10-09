open Printf

let latex_of_string s =
  let tokens = Caml2html.Input.string s in
  let buf = Buffer.create 1000 in
  Caml2html.Output_latex.ocaml buf tokens;
  Buffer.contents buf

let print_ocaml s =
  print "\\begin{alltt}";
  print (latex_of_string s);
  print "\\end{alltt}"

(* Validate ATD syntax before printing *)
let print_atd s =
  try
    ignore (Atd_util.load_string
              ~expand:true ~keep_poly:true
              ~inherit_fields:true ~inherit_variants:true s);
    print_ocaml s
  with e ->
    let msg =
      match e with
          Failure s
        | Atd_ast.Atd_error s -> s
        | _ -> Printexc.to_string e
    in
    Printf.eprintf "\
*** Invalid ATD ***
%s
*** Error ***
%s

%!"
      s msg;
    raise e



let read_command_output f s =
  let ic = Unix.open_process_in s in
  (try
     while true do
       f (input_char ic)
     done
   with End_of_file -> ());
  match Unix.close_process_in ic with
      Unix.WEXITED 0 -> ()
    | _ -> invalid_arg ("read_command_output: " ^ s)


let file_contents fn =
  let buf = Buffer.create 1000 in
  let ic = open_in fn in
  (try
     while true do
       bprintf buf "%s\n" (input_line ic)
     done
   with End_of_file -> ()
  );
  close_in ic;
  Buffer.contents buf


let shell s =
  let buf = Buffer.create 100 in
  read_command_output (Buffer.add_char buf) s;
  print "\\begin{verbatim}";
  print (Buffer.contents buf); (* no escaping! *)
  print "\\end{verbatim}"

let suffix_of_output_type = function
    `Types -> "_t"
  | `Biniou -> "_b"
  | `Json -> "_j"
  | `Validators -> "_v"

let check_atdgen ?prefix output_type s =
  let fn =
    match prefix with
        None -> Filename.temp_file "atdgen_" ".atd"
      | Some s -> s ^ ".atd"
  in
  let prefix = Filename.chop_extension fn in
  let suffix = suffix_of_output_type output_type in
  let mli = prefix ^ suffix ^ ".mli" in
  let ml = prefix ^ suffix ^ ".ml" in
  let oc = open_out fn in
  let finally () =
    close_out_noerr oc;
    Sys.remove fn;
    (try Sys.remove mli with _ -> ());
    (try Sys.remove ml with _ -> ());
  in
  try
    output_string oc s;
    close_out oc;
    let cmd =
      sprintf "../atdgen %s %s"
        (match output_type with
             `Types -> "-t"
           | `Biniou -> "-b"
           | `Json -> "-j"
           | `Validators -> "-v")
        fn
    in
    match Sys.command cmd with
        0 ->
          let mli_data = file_contents mli in
          let ml_data = file_contents ml in
          finally ();
          mli_data, ml_data
      | n ->
          eprintf  "\
-- File %s --
%s
----
Command failed: %s
"
            fn s cmd;
          finally ();
          exit 1

  with e ->
    finally ();
    raise e


let print_atdgen ot s =
  ignore (check_atdgen ot s);
  print_atd s



let ocaml () =
  Camlmix.print_with print_ocaml

let atd () =
  Camlmix.print_with print_atd

let atdgen_biniou () =
  Camlmix.print_with (print_atdgen `Biniou)

let atdgen_json () =
  Camlmix.print_with (print_atdgen `Json)

let atdgen = atdgen_biniou


let print_atdgen_mli ?prefix ot msg s =
  let mli, ml = check_atdgen ?prefix ot s in
  print_atd s;
  print msg;
  print_ocaml mli

let print_atdgen_ml ?prefix ot msg s =
  let mli, ml = check_atdgen ?prefix ot s in
  print_atd s;
  print msg;
  print_ocaml ml


let atdgen_types_mli ?prefix msg =
  Camlmix.print_with (print_atdgen_mli ?prefix `Types msg)

let atdgen_biniou_mli ?prefix msg =
  Camlmix.print_with (print_atdgen_mli ?prefix `Biniou msg)

let atdgen_json_mli ?prefix msg =
  Camlmix.print_with (print_atdgen_mli ?prefix `Json msg)

let atdgen_validate_mli ?prefix msg =
  Camlmix.print_with (print_atdgen_mli ?prefix `Validators msg)

let atdgen_validate_ml ?prefix msg =
  Camlmix.print_with (print_atdgen_ml ?prefix `Validators msg)


let current_annot_section = ref ""
let current_annot_field = ref ""

let annot_section ?label s =
  current_annot_section := s;
  let label =
    match label with
        None -> s
      | Some s -> s
  in
  print (sprintf "\\subsection{Section \\texttt{%s}\\label{%s}}"
           s label)

let annot_field ?label field =
  current_annot_field := field;
  let s = !current_annot_section ^ "." ^ field in
  let label =
    match label with
        None -> s
      | Some s -> s
  in
  print (sprintf "\\subsubsection{Field \\texttt{%s}\\label{%s}}"
           s label)

let odoc_url =
  "http://oss.wink.com/atdgen/atdgen-" ^ version ^ "/odoc/index.html"
