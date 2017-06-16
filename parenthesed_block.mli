type parenthesis_pair = string * string
type associator = string

val decompose_without_taking_blanks_into_account :
  parenthesis_pair list -> string -> (parenthesis_pair option * string) list


val decompose_with_associator :
  associator -> parenthesis_pair list -> string -> string list

val decompose :
  parenthesis_pair list -> string -> (parenthesis_pair option * string) list
