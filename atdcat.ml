(* $Id: atdcat.ml 47040 2010-08-19 07:33:46Z martin $ *)

open Printf

let print_atd ast =
  let pp = Atd_print.format ast in
  Easy_format.Pretty.to_channel stdout pp;
  print_newline ()

let print_ml ~name ast =
  let buf = Buffer.create 1000 in
  Atd_reflect.print_full_module_def buf name ast;
  print_string (Buffer.contents buf);
  print_newline ()

let parse ~expand ~keep_poly ~xdebug ~inherit_fields ~inherit_variants files =
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
  first_head, List.flatten bodies

let print ~out_format ast =
  let f =
    match out_format with
	`Atd -> print_atd
      | `Ocaml name -> print_ml ~name
  in
  f ast
	

let () =
  let expand = ref false in
  let keep_poly = ref false in
  let xdebug = ref false in
  let inherit_fields = ref false in
  let inherit_variants = ref false in
  let files = ref [] in
  let out_format = ref `Atd in

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
          !files
    in
    print ~out_format: !out_format ast
  with
      Atd_ast.Atd_error s ->
	flush stdout;
	eprintf "%s\n%!" s
    | e -> raise e
