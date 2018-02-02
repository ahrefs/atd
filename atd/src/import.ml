module String = struct
  [@@@ocaml.warning "-3-32"]
  let lowercase_ascii = StringLabels.lowercase
  let uppercase_ascii = StringLabels.uppercase
  let capitalize_ascii = StringLabels.capitalize
  include String
end

module Char = struct
  [@@@ocaml.warning "-3-32"]
  let uppercase_ascii = Char.uppercase
  include Char
end
