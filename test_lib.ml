
type t = Foo of int

let fail _ = failwith "not implemented"

module Biniou =
struct
  type def = t
  let def_tag = 0
  let write_untagged_def = fail
  let write_def = fail
  let string_of_def = fail
  let get_def_reader = fail
  let read_def = fail
  let def_of_string = fail
end

module Json =
struct
  type def = t
  let write_def = fail
  let string_of_def = fail
  let read_def = fail
  let def_of_string = fail
end
