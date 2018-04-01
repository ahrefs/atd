
type 'a test =
  { name: string
  ; to_yojson : 'a -> Yojson.Safe.json
  ; of_yojson : Yojson.Safe.json -> 'a
  ; data: 'a
  }

type test' = T : 'a test -> test'

type failure =
  { name: string
  ; actual: Yojson.Safe.json
  ; received: (Yojson.Safe.json, exn) result
  }

let pp_json fmt json =
  Format.pp_print_string fmt (Yojson.Safe.pretty_to_string ~std:true json)

let pp_received fmt = function
  | Error e -> Format.fprintf fmt "exn: %s" (Printexc.to_string e)
  | Ok json -> pp_json fmt json

let test ~name ~yojson ~buckle ~data =
  T { name
    ; to_yojson = (fun a -> Yojson.Safe.from_string (yojson a))
    ; of_yojson = (Atdgen_codec_runtime.decode buckle)
    ; data
    }

let run_test (T t) =
  let json = t.to_yojson t.data in
  let data' =
    try
      Ok (t.of_yojson json)
    with e ->
      Error e
  in
  if Ok t.data = data' then (
    Ok ()
  ) else (
    Error (
      { name = t.name
      ; actual = json
      ; received =
          (match data' with
           | Ok d -> Ok (t.to_yojson d)
           | Error e -> Error e)
      }
    )
  )

let run_tests tests =
  let failures =
    tests
    |> List.fold_left (fun failures t ->
      match run_test t with
      | Ok () -> failures
      | Error fmt -> fmt::failures
    ) [] in
  match failures with
  | [] -> exit 0
  | xs ->
      begin
        xs
        |> List.iter (fun f ->
          Format.eprintf
            "%s: Roundtrip failed.@.Have:%a@.Decoded:%a@."
            f.name
            pp_json f.actual
            pp_received f.received
        );
        exit 1
      end

let () =
  run_tests
    [ test ~name:"record"
        ~yojson:Bucklespec_j.string_of_labeled
        ~buckle:Bucklespec_bs.read_labeled
        ~data:{ Bucklespec_t.
                flag = false
              ; lb = "foo bar"
              ; count = 123
              }
    ; test ~name:"variant"
        ~yojson:Bucklespec_j.string_of_simple_vars
        ~buckle:Bucklespec_bs.read_simple_vars
        ~data:[ `Foo (123, 456)
              ; `Bar
              ; `Foobar ()
              ; `Foo_id (`Id "testing")
              ]
    ]
