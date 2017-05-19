

type t =
    [
    | `Line of string
    | `Block of t list
    | `Inline of t list
    ]

let to_buffer ?(offset = 0) ?(indent = 2) buf l =
  let rec print n = function
      `Block l -> List.iter (print (n + indent)) l
    | `Inline l -> List.iter (print n) l
    | `Line s ->
        for i = 1 to n do
          Buffer.add_char buf ' '
        done;
        Buffer.add_string buf s;
        Buffer.add_char buf '\n';
  in
  List.iter (print offset) l

let to_string ?offset ?indent l =
  let buf = Buffer.create 1000 in
  to_buffer ?offset ?indent buf l;
  Buffer.contents buf

let to_channel ?offset ?indent oc l =
  let buf = Buffer.create 1000 in
  to_buffer ?offset ?indent buf l;
  Buffer.output_buffer oc buf

let to_stdout ?offset ?indent l =
 to_channel ?offset ?indent stdout l
