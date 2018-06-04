type 'a t = 'a -> Json.t

let make f = f

let encode f x = f x

let bool b = `Bool b
let char c = `String (String.make 1 c)
let string s = `String s
let unit () = `Null
let float f = `Float f
let int i = `Int i

let list f xs = `List (List.map f xs)
let array f xs = `List (Array.to_list (Array.map f xs))

let int32 s = `String (Int32.to_string s)
let int64 s = `String (Int64.to_string s)

type ('a, 'b) spec =
  { name: string
  ; data: 'a
  ; encode: 'b t
  }

type 'a field_spec =
  | Optional of ('a option, 'a) spec
  | Required of ('a, 'a) spec * 'a option

type field = F : 'a field_spec -> field

let field ?default encode ~name data =
  F (Required (
    { name
    ; data
    ; encode
    }, default
  ))

let field_o encode ~name data =
  F (Optional (
    { name
    ; data
    ; encode
    }
  ))

let obj fields =
  `Assoc (
    List.fold_left (fun acc (F f) ->
      match f with
      | Required ({ name; data; encode}, _) ->
          (name, encode data)::acc
      | Optional { name; data; encode} ->
          match data with
          | None -> acc
          | Some s -> (name, encode s)::acc
    ) [] fields
  )

let tuple2 f g (w, x) = `Tuple [f w; g x]
let tuple3 f g h (w, x, y) = `Tuple [f w; g x; h y]
let tuple4 f g h i (w, x, y, z) = `Tuple [f w; g x; h y;i z]

let constr0 s = `Variant (s, None)
let constr1 s f x = `Variant (s, Some (f x))

let contramap f g b = g (f b)

let nullable f = function
  | None -> `Null
  | Some s -> f s

let option_as_constr f = function
  | None -> `Variant ("None", None)
  | Some s -> `Variant ("Some", Some (f s))
