(**
   TypeScript-specific ATD annotations.

   This interface serves as a reference of which TypeScript-specific
   ATD annotations are supported. Atdpy also honors JSON-related annotations
   defined in [Atdgen_emit.Json].
*)

(** Extract ["42"] from [<ts default="42">].
    The provided default must be a well-formed TypeScript expression.
*)
val get_ts_default : Atd.Annot.t -> string option
