type t 
val to_string_list : t -> string list 
val of_string_list : string list -> t 
val concat_two : t -> t -> t 
val concat : t list -> t 
val print : t -> string 
val itemize : ('a -> string) -> 'a list -> t 

val max_line_length_ref : int ref 
type inner_separator = string
val make_aggregates : inner_separator -> string list -> t

