module A = Atd.Ast

let not_implemented loc msg =
  A.error_at loc ("not implemented in atdcpp: " ^ msg)

let rec join_types types : string=
  match types with
  | [h] -> h
  | h::t -> h ^ ", " ^ (join_types t)
  | _ -> "";;

let prepare_header_guard name = 
  let length = String.length name in
  let name = String.uppercase_ascii name in
  String.sub name 0 (length - 4)