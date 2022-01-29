(*
   Entry point to the atdpy command.
*)

let run_file src_path =
  let src_name = Filename.basename src_path in
  let dst_name =
    (if Filename.check_suffix src_name ".atd" then
       Filename.chop_suffix src_name ".atd"
     else
       src_name) ^ ".py"
  in
  let dst_path = dst_name in
  let (_atd_head, atd_module), _original_types =
    Atd.Util.load_file
      ~expand:false ~inherit_fields:true ~inherit_variants:true src_path
  in
  Codegen.to_file atd_module dst_path

(* TODO: use cmdliner for a complete CLI *)
let main () =
  for i = 1 to Array.length Sys.argv - 1 do
    let atd_file = Sys.argv.(i) in
    run_file atd_file
  done

let () = main ()
