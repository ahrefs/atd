{
  open Doc_types

  let close_paragraph a1 a2 a3 =
    let a2 =
      match String.concat "" (List.rev a3) with
          "" -> a2
        | s -> Text s :: a2
    in
    match List.rev a2 with
        [] -> a1
      | l -> Paragraph l :: a1

  let rev_concat rev_fragments =
    List.rev rev_fragments |> String.concat ""

  let count_leading_spaces line =
    let n = ref 0 in
    try
      String.iter (function ' ' -> incr n | _ -> raise Exit) line;
      (* blank line = infinite indentation *)
      max_int
    with Exit -> !n

  (* Remove as many leading spaces as possible while maintaining the relative
     visual positioning of the text *)
  let trim_indentation lines =
    match lines with
    | [] -> []
    | first :: other_lines ->
        let removable_indentation =
          List.fold_left
            (fun mini line ->
               min (count_leading_spaces line) mini)
            (count_leading_spaces first) other_lines
        in
        List.map (fun str ->
          let len = String.length str in
          if removable_indentation <= len then
            String.sub str
              removable_indentation
              (String.length str - removable_indentation)
          else
            (* blank line with no or too few leading spaces *)
            ""
        ) lines
}

let line_break = '\r'? '\n'
let space = [' ' '\t' '\r' '\n']
let space' = space#['\n']

let par_special = ['\\' '{' '}']
let par_not_special = [^ '\\' '{' '}' ' ' '\t' '\r' '\n']
let verb_not_special = [^ '\\' ' ' '\t' '\r' '\n' '}']


(*
  Paragraph mode
*)
rule paragraph a1 a2 a3 = parse
    '\\' ('\\' | "{{" | "{{{" as s)
                        { paragraph a1 a2 (s :: a3) lexbuf }
  | "{{"
                        { let code = inline_verbatim [] lexbuf in
                          let a2 =
                            match String.concat "" (List.rev a3) with
                                "" -> a2
                              | s -> Text s :: a2
                          in
                          let a2 = Code code :: a2 in
                          paragraph a1 a2 [] lexbuf
                        }
  | space* "{{{" line_break?
                        { let pre = verbatim [] [] lexbuf in
                          let a1 = close_paragraph a1 a2 a3 in
                          let a1 = Pre pre :: a1 in
                          paragraph a1 [] [] lexbuf
                        }
  | par_not_special+ as s
                        { paragraph a1 a2 (s :: a3) lexbuf }
  | space'* "\n"? space'*
                        { paragraph a1 a2 (" " :: a3) lexbuf }
  | space'* "\n" (space'* "\n")+ space'*
                        { let a1 = close_paragraph a1 a2 a3 in
                          paragraph a1 [] [] lexbuf
                        }
  | space* eof          { let a1 = close_paragraph a1 a2 a3 in
                          List.rev a1 }

  | _ as c              { paragraph a1 a2 (String.make 1 c :: a3) lexbuf }



(*
  Inline verbatim mode:
  Only "}}" need to be escaped.
  Backslashes can be escaped but single backslashes are tolerated.
*)
and inline_verbatim accu = parse
    "\\\\"              { inline_verbatim ("\\" :: accu) lexbuf }
  | "\\}}"              { inline_verbatim ("}}" :: accu) lexbuf }
  | space+              { inline_verbatim (" " :: accu) lexbuf }
  | verb_not_special+ as s
                        { inline_verbatim (s :: accu) lexbuf }
  | _ as c              { inline_verbatim (String.make 1 c :: accu) lexbuf }

  | space* "}}"         { String.concat "" (List.rev accu) }

  | eof                 { failwith "Missing `}}'" }


(*
  Verbatim paragraph mode:
  Only "}}}" need to be escaped.
  Backslashes can be escaped but single backslashes are tolerated.
*)
and verbatim lines line = parse
    "\\\\"              { verbatim lines ("\\" :: line) lexbuf }
  | "\\}}}"             { verbatim lines ("}}}" :: line) lexbuf }
  | '\t'                { verbatim lines ("        " :: line) lexbuf }
  | line_break          { verbatim (rev_concat line :: lines) [] lexbuf }
  | verb_not_special+ as s
                        { verbatim lines (s :: line) lexbuf }
  | _ as c              { verbatim lines (String.make 1 c :: line) lexbuf }

  | line_break? "}}}"   { List.rev (rev_concat line :: lines)
                          |> trim_indentation }

  | eof                 { failwith "Missing `}}}'" }

{
  let parse_string s =
    let lexbuf = Lexing.from_string s in
    paragraph [] [] [] lexbuf
}
