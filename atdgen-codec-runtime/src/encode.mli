type 'a t

val make : ('a -> S.json) -> 'a t

val null : unit t
val string : string t
val float : float t
val int : int t
val bool : bool t
val char : char t
