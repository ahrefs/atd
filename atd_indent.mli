

(**
  Simple indentation utility for code generators
*)

type t =
    [
    | `Line of string
    | `Block of t list
    | `Inline of t list
    ]
(**
   [t] is the type of the data to be printed.

   - [`Line]: single line (not indented)
   - [`Block]: indented sequence
   - [`Inline]: in-line sequence (not indented)

  Example:

{v
let l =
  [
    `Line "d";
    `Line "e";
  ]
in
[
  `Line "a";
  `Block [
    `Line "b";
    `Line "c";
  ];
  `Inline l;
  `Line "f";
]
v}

gives:

{v
a
  b
  c
d
e
f
v}
*)


val to_buffer : ?offset:int -> ?indent:int -> Buffer.t -> t list -> unit
  (** Write to a buffer.

      @param offset defines the number of space characters
      to use for the left margin. Default: 0.

      @param indent defines the number of space characters to use for
      indenting blocks. Default: 2.
  *)

val to_string : ?offset:int -> ?indent:int -> t list -> string
  (** Write to a string. See [to_buffer] for the options. *)

val to_channel : ?offset:int -> ?indent:int -> out_channel -> t list -> unit
  (** Write to a channel. See [to_buffer] for the options. *)

val to_stdout : ?offset:int -> ?indent:int -> t list -> unit
  (** Write to [stdout]. See [to_buffer] for the options. *)
