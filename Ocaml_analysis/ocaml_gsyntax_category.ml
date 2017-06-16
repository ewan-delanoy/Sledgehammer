(*

#use"Ocaml_analysis/ocaml_gsyntax_category.ml";;

*)

type t=
     Value
    |Type
    |Exception 
    |Module_opener
    |Module_closer 
    |Module_inclusion;;
    
    