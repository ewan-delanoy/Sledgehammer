type ('a, 'b) t 
val constructor :
  ('a -> 'a -> bool) -> ('a * 'b Tidel.set) list -> ('a, 'b) t
val add_chooser : ('a -> 'a -> bool) -> 'a * 'b -> ('a, 'b) t -> ('a, 'b) t
val solve_explicitly :
  ('a -> 'a -> bool) ->
  ('a, 'b) t -> (('a * 'b Tidel.set) * 'b) list * ('a * 'b) list
val solve : ('a -> 'a -> bool) -> ('a, 'b) t -> ('a * 'b) list
