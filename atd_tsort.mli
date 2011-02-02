(* $Id: atd_tsort.mli 38554 2010-03-18 23:54:35Z martin $ *)

(*
  Generic topological sorting and cycle detection.

  This is useful for detecting which definitions are truly recursive,
  if allowed at all.
*)

type ('a, 'b) node = ('a * 'a list * 'b)

module type Ordered =
sig
  type t
  val compare : t -> t -> int
  val to_string : t -> string (* for error messages *)
end

module Make (Param : Ordered) :
sig
  val sort : (Param.t, 'a) node list -> (bool * 'a list) list
    (* bool indicates whether definitions are mutually recursive. *)
end
