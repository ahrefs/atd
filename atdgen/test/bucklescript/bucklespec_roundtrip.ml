
let r =
  { Bucklespec_t.
    flag = false
  ; lb = "foo bar"
  ; count = 123
  }

let () =
  let json =
    Bucklespec_j.string_of_labeled r
    |> Yojson.Safe.from_string in
  let r' =
    Atdgen_codec_runtime.decode
      Bucklespec_bs.read_labeled
      json in
  assert (r = r')
