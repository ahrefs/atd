
type json =
  [ `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Intlit of string
  | `List of json list
  | `Null
  | `String of string
  | `Tuple of json list
  | `Variant of string * json option ]
