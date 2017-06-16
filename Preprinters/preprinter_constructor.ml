(*

#use"preprinter_constructor.ml";;

Returns the list of tokens in a description of the argument object.

*)


let int=
  let tempf=(fun i->[string_of_int i]) in
  (tempf: int Preprinter.t);;


