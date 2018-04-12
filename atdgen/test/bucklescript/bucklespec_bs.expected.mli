(* Auto-generated from "bucklespec.atd" *)
              [@@@ocaml.warning "-27-32-35-39"]
open Bucklespec_t

val read_valid :  valid Atdgen_codec_runtime.Decode.t

val read_id :  id Atdgen_codec_runtime.Decode.t

val read_simple_vars :  simple_vars Atdgen_codec_runtime.Decode.t

val read_simple_var : 'a Atdgen_codec_runtime.Decode.t -> 'a simple_var Atdgen_codec_runtime.Decode.t

val read_same_pair : 'a Atdgen_codec_runtime.Decode.t -> 'a same_pair Atdgen_codec_runtime.Decode.t

val read_point :  point Atdgen_codec_runtime.Decode.t

val read_param : 'a Atdgen_codec_runtime.Decode.t -> 'a param Atdgen_codec_runtime.Decode.t

val read_pair : 'a Atdgen_codec_runtime.Decode.t -> 'b Atdgen_codec_runtime.Decode.t -> ('a, 'b) pair Atdgen_codec_runtime.Decode.t

val read_pairs : 'a Atdgen_codec_runtime.Decode.t -> 'a pairs Atdgen_codec_runtime.Decode.t

val read_label :  label Atdgen_codec_runtime.Decode.t

val read_labeled :  labeled Atdgen_codec_runtime.Decode.t

