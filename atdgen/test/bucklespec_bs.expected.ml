let read_valid = (
  Atdgen_codec_runtime.bool
)
let read_point = (
    Atdgen_codec_runtime.int
  ,
    Atdgen_codec_runtime.int
  ,
    Atdgen_codec_runtime.string
  ,
    Atdgen_codec_runtime.unit
)
let read_label = (
  Atdgen_codec_runtime.string
)
let read_labeled = (
  fun json ->
    (
      {
          flag =
          Atdgen_codec_runtime.decode
          (
            read_valid
          ) json;
          lb =
          Atdgen_codec_runtime.decode
          (
            read_label
          ) json;
          count =
          Atdgen_codec_runtime.decode
          (
            Atdgen_codec_runtime.int
          ) json;
      }
    )
)
