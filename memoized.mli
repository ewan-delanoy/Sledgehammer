type ('a, 'b) map = 'a -> 'b
val make_from : ('a -> 'b) -> ('a, 'b) Hashtbl.t -> ('a, 'b) map
val make : ('a -> 'b) -> ('a, 'b) map
val recursive_from : (('a, 'b) map -> 'a -> 'b) -> ('a, 'b) Hashtbl.t -> ('a, 'b) map
val recursive : (('a->'b) -> 'a -> 'b) -> ('a, 'b) map
val reversible : ('a -> 'b) -> (('a,'b) map) * (('b,'a) map) * (('a,'a) map) * (('a list) ref) * ((('a*'a) list) ref)
val small : ('a -> 'a) -> 'a -> ((int,'a) map)


