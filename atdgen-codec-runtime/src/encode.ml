type 'a t = 'a -> Json.t

let make f = f

let bool b = `Bool b
let char c = `String (String.make 1 c)
let string s = `String s
let null () = `Null
let float f = `Float f
let int i = `Int i

let list f xs = `List (List.map f xs)
