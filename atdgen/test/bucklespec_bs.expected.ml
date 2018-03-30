let read_valid = (
  Atdgen_codec_runtime.bool
)
let valid_of_string s =
  read_valid (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let read_label = (
  Atdgen_codec_runtime.string
)
let label_of_string s =
  read_label (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
