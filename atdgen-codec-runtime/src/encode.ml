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

let int32 s = `String (Int32.to_string s)
let int64 s = `String (Int64.to_string s)

let obj s = `Assoc s

let tuple2 f g (w, x) = `Tuple [f w; g x]
let tuple3 f g h (w, x, y) = `Tuple [f w; g x; h y]
let tuple4 f g h i (w, x, y, z) = `Tuple [f w; g x; h y;i z]
