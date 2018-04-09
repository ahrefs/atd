open Spec_j

let json conv e = Yojson.Safe.from_string (conv e)

let massive_json =
  `List
    [ json string_of_r1 { r1 = "testing"}
    ; json string_of_r2 { r2 = Some 2 }
    ; json string_of_r2 { r2 = None }
    ; json string_of_r3 { r3 = Some 3 }
    ; json string_of_r3 { r3 = None }
    ; json string_of_r4 { r4 = true }
    ; json string_of_r5 { r5 = Some 5 }
    ; json string_of_r5 { r5 = None }
    ; json string_of_r6 { r6 = 6 }
    ; json string_of_r6 { r6 = 42 }
    ; json string_of_r7 { r7 = -1_000 }
    ; json string_of_r8 { r888 = [1; 2; 3] }
    ; json string_of_j1 ["foo"; "bar"]
    ; json string_of_j1 []
    ; json string_of_j2 ()
    ; json string_of_j3 [|1; 2; 3|]
    ; json string_of_j4 'c'
    ; json string_of_t1 (100, "foo")
    ; json string_of_t2 (100, 200, 42)
    ; json string_of_t2 (100, 200, -1)
    ; json string_of_v1list [ `V1; `V2; `V3 "testing";
                              `V4 255; `V5 None; `V5 (Some true)]
    ; json string_of_v2 { v2 = `A }
    ; json string_of_v2 { v2 = `B 100 }
    ; json string_of_v3list [C1 ; C2 true; C2 false]
    ; json string_of_ages [ `Age 50; `Age 30; `Age (-1); `Age 400]
    ]

let pp_json fmt json =
  Format.pp_print_string fmt (Yojson.Safe.pretty_to_string ~std:true json)

let () = pp_json Format.std_formatter massive_json
