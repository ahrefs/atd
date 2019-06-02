open Spec_js.W

let make f x =
  let x = f x in
  let x = List.map (fun (k, v) -> k, `String v) x in
  `Assoc x

module M = Spec_js.Make(struct
    let r1     = make www_form_of_r1
    let r2     = make www_form_of_r2
    let r3     = make www_form_of_r3
    let r4     = make www_form_of_r4
    let r5     = make www_form_of_r5
    let r6     = make www_form_of_r6
    let r7     = make www_form_of_r7
    let r8     = make www_form_of_r8
    let j1     = make www_form_of_j1
    let j2     = make www_form_of_j2
    let j3     = make www_form_of_j3
    let j4     = make www_form_of_j4
    let o1     = make www_form_of_o1
    let o2     = make www_form_of_o2
    let t1     = make www_form_of_t1
    let t2     = make www_form_of_t2
    let v1list = make www_form_of_v1list
    let v2     = make www_form_of_v2
    let v3list = make www_form_of_v3list
    let ages   = make www_form_of_ages
  end)

let () = M.run()

