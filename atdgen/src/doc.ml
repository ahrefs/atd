
type inline =
    [ `Text of string
    | `Code of string ]
type block = [ `Paragraph of inline list | `Pre of string ]
type doc = [ `Text of block list ]

let parse_text loc s =
  try Some (Some (`Text (Doc_lexer.parse_string s : block list)))
  with e ->
    failwith (Printf.sprintf "%s:\nInvalid format for doc.text %S:\n%s"
                (Atd_ast.string_of_loc loc) s (Printexc.to_string e))

let get_doc loc an : doc option =
  Atd_annot.get_field (parse_text loc) None ["doc"] "text" an
