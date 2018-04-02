module String = struct
  [@@@ocaml.warning "-3-32"]
  let lowercase_ascii = StringLabels.lowercase
  let uppercase_ascii = StringLabels.uppercase
  let capitalize_ascii = StringLabels.capitalize
  include String
end

module Char = struct
  [@@@ocaml.warning "-3-32"]
  let uppercase_ascii = Char.uppercase
  include Char
end

module List = struct
  include List

  let rec filter_map f = function
      [] -> []
    | x :: l ->
        match f x with
          None -> filter_map f l
        | Some y -> y :: filter_map f l

  let concat_map f l =
    List.map f l
    |> List.flatten

  let map_first f = function
    | [] -> []
    | x :: l ->
        let y = f ~is_first:true x in
        y :: List.map (f ~is_first:false) l
end

module Option = struct
  let map f = function
    | None -> None
    | Some s -> Some (f s)
end

let sprintf = Printf.sprintf
let printf = Printf.printf
let eprintf = Printf.eprintf
let bprintf = Printf.bprintf
let fprintf = Printf.fprintf
