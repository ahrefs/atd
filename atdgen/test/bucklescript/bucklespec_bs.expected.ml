(* Auto-generated from "bucklespec.atd" *)
              [@@@ocaml.warning "-27-32-35-39"]
open Bucklespec_t

let read_valid = (
  Atdgen_codec_runtime.Decode.bool
)
let read__2 = (
  (
    Atdgen_codec_runtime.Decode.string
  ) |> (Atdgen_codec_runtime.Decode.map (fun s -> `Id s))
)
let read_id = (
  read__2
)
let read__3 = (
  Atdgen_codec_runtime.Decode.enum
  [
      (
      "Foo"
      ,
        `Decode (
        Atdgen_codec_runtime.Decode.tuple2
          (
            Atdgen_codec_runtime.Decode.int
          )
          (
            Atdgen_codec_runtime.Decode.int
          )
        |> Atdgen_codec_runtime.Decode.map (fun x -> `Foo x)
        )
      )
    ;
      (
      "Bar"
      ,
        `Single (`Bar)
      )
    ;
      (
      "Foobar"
      ,
        `Decode (
        Atdgen_codec_runtime.Decode.unit
        |> Atdgen_codec_runtime.Decode.map (fun x -> `Foobar x)
        )
      )
    ;
      (
      "Foo_id"
      ,
        `Decode (
        read_id
        |> Atdgen_codec_runtime.Decode.map (fun x -> `Foo_id x)
        )
      )
  ]
)
let read__4 = (
  Atdgen_codec_runtime.Decode.list (
    read__3
  )
)
let read_simple_vars = (
  read__4
)
let read_simple_var read__a = (
  Atdgen_codec_runtime.Decode.enum
  [
      (
      "Foo"
      ,
        `Decode (
        Atdgen_codec_runtime.Decode.tuple2
          (
            Atdgen_codec_runtime.Decode.int
          )
          (
            Atdgen_codec_runtime.Decode.int
          )
        |> Atdgen_codec_runtime.Decode.map (fun x -> `Foo x)
        )
      )
    ;
      (
      "Bar"
      ,
        `Single (`Bar)
      )
    ;
      (
      "Foobar"
      ,
        `Decode (
        read__a
        |> Atdgen_codec_runtime.Decode.map (fun x -> `Foobar x)
        )
      )
    ;
      (
      "Foo_id"
      ,
        `Decode (
        read_id
        |> Atdgen_codec_runtime.Decode.map (fun x -> `Foo_id x)
        )
      )
  ]
)
let read_same_pair read__a = (
  Atdgen_codec_runtime.Decode.tuple2
    (
      read__a
    )
    (
      read__a
    )
)
let read_point = (
  Atdgen_codec_runtime.Decode.tuple4
    (
      Atdgen_codec_runtime.Decode.int
    )
    (
      Atdgen_codec_runtime.Decode.int
    )
    (
      Atdgen_codec_runtime.Decode.string
    )
    (
      Atdgen_codec_runtime.Decode.unit
    )
)
let read_param read__a = (
  Atdgen_codec_runtime.Decode.make (fun json ->
    (
      {
          data =
            Atdgen_codec_runtime.Decode.decode
            (
              read__a
              |> Atdgen_codec_runtime.Decode.field "data"
            ) json;
          nothing =
            Atdgen_codec_runtime.Decode.decode
            (
              Atdgen_codec_runtime.Decode.unit
              |> Atdgen_codec_runtime.Decode.field "nothing"
            ) json;
      }
    )
  )
)
let read_pair read__a read__b = (
  Atdgen_codec_runtime.Decode.make (fun json ->
    (
      {
          left =
            Atdgen_codec_runtime.Decode.decode
            (
              read__a
              |> Atdgen_codec_runtime.Decode.field "left"
            ) json;
          right =
            Atdgen_codec_runtime.Decode.decode
            (
              read__b
              |> Atdgen_codec_runtime.Decode.field "right"
            ) json;
      }
    )
  )
)
let read__1 read__a read__b = (
  Atdgen_codec_runtime.Decode.list (
    read_pair read__a read__a
  )
)
let read_pairs read__a = (
  read__1 read__a read__a
)
let read_label = (
  Atdgen_codec_runtime.Decode.string
)
let read_labeled = (
  Atdgen_codec_runtime.Decode.make (fun json ->
    (
      {
          flag =
            Atdgen_codec_runtime.Decode.decode
            (
              read_valid
              |> Atdgen_codec_runtime.Decode.field "flag"
            ) json;
          lb =
            Atdgen_codec_runtime.Decode.decode
            (
              read_label
              |> Atdgen_codec_runtime.Decode.field "lb"
            ) json;
          count =
            Atdgen_codec_runtime.Decode.decode
            (
              Atdgen_codec_runtime.Decode.int
              |> Atdgen_codec_runtime.Decode.field "count"
            ) json;
      }
    )
  )
)
