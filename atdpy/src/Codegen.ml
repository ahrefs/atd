(*
   Python code generation
*)

let to_file _atd_module dst_path =
  let tree = [] in
  Indent.to_file dst_path tree
