(*
   Convert ATD <doc text==...> annotations to Python docstrings

   - Doc nodes at the type declaration level become class docstrings.
   - Doc nodes on record fields become Sphinx-compatible ':param' items
     within the class docstring.
   - Doc nodes variant constructors become docstrings on the class
     for that constructor.
*)

(** Format a doc node into a docstring-compatible format

    @param paragraph_width maximum line length in bytes to use when rewrapping
    a paragraph of text.
*)
val make_unquoted_multiline_docstring :
  paragraph_width:int ->
  Atd.Doc.doc -> Indent.node list
