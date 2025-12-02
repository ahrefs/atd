(*
   Convert ATD <doc text==...> annotations to Python docstrings
*)

(*
let split = Re.Str.split (Re.Str.regexp " ")

let make_ocamldoc_block = function
  | Atd.Doc.Pre s -> Atom ("\n{v\n" ^ ocamldoc_verbatim_escape s ^ "\nv}", atom)
  | Paragraph l ->
      let l = List.map (function
        | Atd.Doc.Text s -> ocamldoc_escape s
        | Code s -> "[" ^ ocamldoc_escape s ^ "]"
      ) l
      in
      let words = split (String.concat "" l) in
      let atoms = List.map (fun s -> Atom (s, atom)) words in
      List (("", "", "", plist), atoms)
*)

open Atd
open Indent

let docstring_escape str =
  (* TODO *)
  str

(* Same as String.concat but over a list of lists
   instead of a list of strings *)
let list_concat ?(sep:'a list = []) (xs: 'a list list) : 'a list =
  match xs with
  | [] -> []
  | first :: other ->
      first @ (List.map (fun x -> sep @ x) other |> List.flatten)

let translate_inline_element (x : Doc.inline) : string =
  match x with
  | Text str -> docstring_escape str
  | Code str ->
      (* Using two backticks because it's what Sphinx uses *)
      "``" ^ docstring_escape str ^ "``"

let translate_block ~max_length (block : Doc.block) : Indent.node list =
  match block with
  | Pre lines ->
      [
        Block (
          List.map (fun line ->
            Line (docstring_escape line)
          ) lines
        )
      ]
  | Paragraph elements ->
      elements
      |> List.map translate_inline_element
      |> Doc.rewrap_paragraph ~max_length
      |> List.map (fun line -> Line line)

let make_unquoted_multiline_docstring
    ~paragraph_width
    (blocks : Doc.block list) : Indent.node list =
  blocks
  |> List.map (translate_block ~max_length:paragraph_width)
  |> list_concat ~sep:[Line ""]
