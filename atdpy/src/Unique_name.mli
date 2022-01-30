(**
   A mechanism to translate an identifier into one that's not reserved
   or already taken.

   When necessary, identifiers are modified by adding prefixes or suffixes
   that are compatible with Python syntax.
*)

type t

(** Initialize the translation tables.

    [reserved_identifiers] are forbidden identifiers, i.e. we guarantee
    that a translation will never return one of these.
    [safe_prefix] is a prefix that will be added to an identifier that
    matches one of the reserved prefixes ([reserved_prefixes]).
*)
val init :
  reserved_identifiers: string list ->
  reserved_prefixes: string list ->
  safe_prefix: string ->
  t

(** Make a local copy. This allows managing a local environment.

    Design note: if this becomes tedious, an alternative is to make the
    environment immutable, with the downside of returning the updated the
    environment each time we want to change it. *)
val copy : t -> t

(** Register a name if it's not already registered and return its
    translation into an available identifier. *)
val translate : t -> string -> string
