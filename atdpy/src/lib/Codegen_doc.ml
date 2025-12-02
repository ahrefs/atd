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

let escape_docstring_re =
  Re.Pcre.regexp {|"""|}

(** Escape string contents to be placed within triple double-quotes.

    Warning: concatenating two escaped fragments may result in an incorrectly
    escaped fragment! When concatenating two escaped fragments, it is
    sufficient to insert one or more whitespace characters such as newlines
    as a separator to avoid problems.

    {|"""|} -> {|"\"\"|}

    This escaping is tricky but safe, correct, and simple.

    Not inserting a backslash in front of the first quote avoids introducing
    a spurious literal backslash in case (4).

    (1) {|a"""b|}  -> {|a"\"\"b|}     (common case)
    (2) {|""""""|} -> {|"\"\""\"\"|}  (escape two successive triple quotes)
    (3) {|"""""|} -> {|"\"\"""|}      (the trailing triple quote is escaped)
    (4) {|\"""|} -> {|\"\"\"|}        (still just 3 literal quotes)
    (5) {|\\"""|} -> {|\\"\"\"|}      (correct escaping)

    For neater results, we could change the docstring delimiters to
    single quotes instead of inserting backslashes. It would be
    a refinement but this function would still be required for the case
    where both kinds of triple quotes occur in the same docstring.
*)
let escape_docstring str =
  Re.Pcre.substitute ~rex:escape_docstring_re ~subst:(fun quotes ->
    {|"\"\"|}
  ) str

(* Same as String.concat but over a list of lists
   instead of a list of strings *)
let list_concat ?(sep:'a list = []) (xs: 'a list list) : 'a list =
  match xs with
  | [] -> []
  | first :: other ->
      first @ (List.map (fun x -> sep @ x) other |> List.flatten)

let translate_inline_element (x : Doc.inline) : string =
  match x with
  | Text str -> str
  | Code str ->
      (* Using two backticks because it's what Sphinx uses *)
      "``" ^ str ^ "``"

let translate_block ~max_length (block : Doc.block) : Indent.node list =
  match block with
  | Pre lines ->
      [
        Block (
          List.map (fun line ->
            Line (escape_docstring line)
          ) lines
        )
      ]
  | Paragraph elements ->
      elements
      |> List.map translate_inline_element
      |> Doc.rewrap_paragraph ~max_length
      |> List.map (fun line -> Line (escape_docstring line))

let make_unquoted_multiline_docstring
    ~paragraph_width
    (blocks : Doc.block list) : Indent.node list =
  blocks
  |> List.map (translate_block ~max_length:paragraph_width)
  |> list_concat ~sep:[Line ""]
