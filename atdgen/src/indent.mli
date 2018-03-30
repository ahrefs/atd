
type t =
  [ `Line of string        (** single line (not indented) **)
  | `Block of t list       (** indented sequence **)
  | `Inline of t list      (** in-line sequence (not indented) **)
  | `Annot of (string * t) (** arbitrary annotation **)
  ]

val strip : t -> Atd.Indent.t

val concat : 'a -> 'a list -> 'a list
