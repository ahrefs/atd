
val parse_string
  : string
  -> [> `Paragraph of [> `Code of string | `Text of string ] list
     | `Pre of string ] list
