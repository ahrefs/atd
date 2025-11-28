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

open Printf
open Atd
open Indent

let split_on_blank =
  let rex = Re.Pcre.regexp {|[ \t]+|} in
  fun str ->
    Re.Pcre.split ~rex str

(* Concatenate a list of strings ("words") into lines where the words
   are separated by a single space character. Each line will not exceed
   'max_length' bytes unless a word is longer than this. *)
let concatenate_into_lines ~max_length (words : string list) : string list =
  let max_length = max 0 max_length in
  let buf = Buffer.create max_length in
  let finish_line () =
    let line = Buffer.contents buf in
    Buffer.clear buf;
    line
  in
  let rec make_lines orig_words =
    match orig_words with
    | [] -> [finish_line ()]
    | word :: words ->
        let word_len = String.length word in
        let len = Buffer.length buf in
        if len = 0 then (
          (* The word may be longer than 'max_length'. Putting it on its
             own line is the best we can do without hyphenating it. *)
          Buffer.add_string buf word;
          make_lines words
        )
        else
          (* Add the word to the current line only if it fits. *)
          let new_len = len + 1 + word_len in
          if new_len <= max_length then (
            bprintf buf " %s" word;
            make_lines words
          )
          else
            (* The new word doesn't fit on the current line. Start a new one. *)
            let line = finish_line () in
            line :: make_lines orig_words
  in
  make_lines words

let rewrap_paragraph ~max_length str =
  str
  |> split_on_blank
  |> concatenate_into_lines ~max_length

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
      |> String.concat ""
      |> rewrap_paragraph ~max_length
      |> List.map (fun line -> Line line)

let make_unquoted_multiline_docstring
    ~paragraph_width
    (blocks : Doc.block list) : Indent.node list =
  blocks
  |> List.map (translate_block ~max_length:paragraph_width)
  |> list_concat ~sep:[Line ""]
