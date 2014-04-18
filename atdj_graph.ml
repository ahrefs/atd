(* Graphical output *)

open Printf
open Atdj_env

let unique xs =
  let xs' = List.sort Pervasives.compare xs in
  let rec f = function
    | (y::z::zs) -> if y = z then f (z::zs) else y :: (f (z :: zs))
    | zs         -> zs in
  f xs'

(* Output a dot graph of the class hierarchy *)
let output_graph env =
  let filename =
    match env.input_file with
      | None   -> assert false
      | Some x -> Filename.chop_extension x ^ ".dot" in
  let out = open_out filename in
  let env = { env with
                types     = unique env.types;
                sub_types = unique env.sub_types } in
  fprintf out "digraph \"G\" {\n";
  fprintf out "  rankdir=BT\n";
  List.iter
    (function
       | `Class (c, args) ->
           (match args with
              | [] -> fprintf out "  %s [shape=box]\n" c
              | _  ->
                  let args' = String.concat "|"
                    (List.map
                       (fun (x, t) ->
                          let is_array =
                            Str.string_match (Str.regexp ".*\\[\\]$") t 0 in
                          let t' =
                            Str.global_replace (Str.regexp "\\[\\]$") "" t in
                          (* For primitive types, append the type name
                             to the field name *)
                          if List.mem t' ["int"; "IntOpt";
                                          "boolean"; "BooleanOpt";
                                          "String"; "StringOpt";
                                          "double"; "DoubleOpt"] then
                            sprintf "%s:%s" x t
                          else (
                            (* For the rest, place an arc from the type
                               to the field name, reversing the apparent
                               direction to give the expected view *)
                            fprintf out
                              "  \"%s\" -> \"%s\":%s [style=dashed,dir=back]\n"
                              t' c x;
                            (* The <> notation labels a node *)
                            sprintf "<%s>%s%s" x x
                              (if is_array then ":[]" else "")
                          )
                       )
                       args) in
                  fprintf out "  %s [shape=record, label=\"{\\N|{%s}}\"]\n"
                    c args'
           )
       | `Interface i -> fprintf out "  %s [shape=oval]\n" i
    )
    env.types;
  List.iter
    (function (ty, ty') -> fprintf out "  %s -> %s [style=solid]\n" ty ty')
    env.sub_types;
  fprintf out "}\n";
  close_out out
