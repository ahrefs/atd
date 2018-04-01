
let r =
  { Bucklespec_t.
    flag = false
  ; lb = "foo bar"
  ; count = 123
  }

let my_vars : Bucklespec_t.simple_vars =
  [ `Foo (123, 456)
  ; `Bar
  ; `Foobar ()
  ; `Foo_id (`Id "testing")
  ]

let () =
  let json =
    Bucklespec_j.string_of_labeled r
    |> Yojson.Safe.from_string in
  let r' =
    Atdgen_codec_runtime.decode
      Bucklespec_bs.read_labeled
      json in
  assert (r = r')

let () =
  let json =
    Bucklespec_j.string_of_simple_vars my_vars
    |> Yojson.Safe.from_string in
  let my_vars' =
    Atdgen_codec_runtime.decode
      Bucklespec_bs.read_simple_vars
      json in
  assert (my_vars = my_vars')
