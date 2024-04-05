(*
   ATD annotations to be interpreted specifically by atdcpp.

   atdcpp also honors json-related annotations defined in Atd.Json.
*)

type assoc_repr =
  | List
  | Dict

type atd_cpp_wrap = {
  cpp_wrap_t : string;
  cpp_wrap : string;
  cpp_unwrap : string;
  cpp_templatize : bool;
}

let get_cpp_default an : string option =
    Atd.Annot.get_opt_field
      ~parse:(fun s -> Some s)
      ~sections:["cpp"]
      ~field:"default"
      an

let get_cpp_assoc_repr an : assoc_repr =
  Atd.Annot.get_field
    ~parse:(function
      | "list" -> Some List
      | "dict" -> Some Dict
      | _ -> None
    )
    ~default:List
    ~sections:["cpp"]
    ~field:"repr"
    an

(* includes etc. *)
let get_cpp_include an : string list =
  Atd.Annot.get_fields
    ~parse:(fun s -> Some s)
    ~sections:["cpp"]
    ~field:"include"
    an

let get_cpp_namespace an : string option =
  Atd.Annot.get_opt_field
    ~parse:(fun s -> Some s)
    ~sections:["cpp"]
    ~field:"namespace"
    an

let get_cpp_wrap loc an =
  let path = ["cpp"] in
  let module_ =
    Atd.Annot.get_opt_field
      ~parse:(fun s -> Some s)
      ~sections:path
      ~field:"module"
      an
  in
  let open Printf in
  let default field =
    Option.map (fun s ->
      sprintf "%s.%s" s field) module_
  in
  let default_t field =
    Option.map (fun s ->
      sprintf "%s.%s"  s field) module_
  in
  let t =
    Atd.Annot.get_field
      ~parse:(fun s -> Some (Some s))
      ~default:(default_t "t")
      ~sections:path
      ~field:"t"
      an
  in
  let wrap =
    Atd.Annot.get_field
      ~parse:(fun s -> Some (Some s))
      ~default:(default "wrap")
      ~sections:path
      ~field:"wrap"
      an
  in
  let unwrap =
    Atd.Annot.get_field
      ~parse:(fun s -> Some (Some s))
      ~default:(default "unwrap")
      ~sections:path
      ~field:"unwrap"
      an
  in
  let templatize =
    Atd.Annot.get_flag
      ~sections:path
      ~field:"templatize"
      an
  in
  match t, wrap, unwrap with
      None, None, None -> None
    | Some t, Some wrap, Some unwrap ->
        Some { cpp_wrap_t = t; cpp_wrap = wrap; cpp_unwrap = unwrap; cpp_templatize = templatize}
    | _ ->
        Atd.Ast.error_at loc "Incomplete annotation. Missing t, wrap or unwrap"
