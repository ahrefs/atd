
type json =
  [ `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Intlit of string
  | `List of json list
  | `Null
  | `String of string
  | `Tuple of json list
  | `Variant of string * json option ]

type 'a t = json -> 'a

let make f = f

exception DecoderError

let decode f json = f json

let unit = function
  | `Null -> ()
  | _ -> raise DecoderError

let bool = function
  | `Bool b -> b
  | _ -> raise DecoderError

let int = function
  | `Int i -> i
  | _ -> raise DecoderError

let float = function
  | `Float f -> f
  | _ -> raise DecoderError

let char = function
  | `String s when String.length s = 1 -> s.[0]
  | _ -> raise DecoderError

let string = function
  | `String s -> s
  | _ -> raise DecoderError

let list f = function
  | `List l -> List.map f l
  | _ -> raise DecoderError

let array f = function
  | `List l -> Array.map f (Array.of_list l)
  | _ -> raise DecoderError

let optional f j =
  match f j with
  | exception DecoderError -> None
  | v -> Some v

let map f c j = f (c j)

let field s f = function
  | `Assoc v -> f (List.assoc s v)
  | _ -> raise DecoderError

let fieldOptional s f = function
  | `Assoc v ->
      begin match List.assoc s v with
        | exception Not_found -> None
        | v -> Some (f v)
      end
  | _ -> raise DecoderError

let fieldDefault s default f =
  fieldOptional s f
  |> map (function
    | None -> default
    | Some s -> s)

let tuple2 a b = function
  | `List [w ; x] -> (a w, b x)
  | _ -> raise DecoderError

let tuple3 a b c = function
  | `List [w; x; y] -> (a w, b x, c y)
  | _ -> raise DecoderError

let tuple4 a b c d = function
  | `List [w; x; y; z] -> (a x, b y, c y, d z)
  | _ -> raise DecoderError
