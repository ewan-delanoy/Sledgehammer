
(* 


#use"Country/Germany/german_make_module_optional.ml";;


*)


exception Already_optional of Half_dressed_module.t;;

let on_targets (mdata,old_tgts) old_name=
    let s_old_name=Half_dressed_module.to_string(old_name) in
    if Substring.begins_with s_old_name "Optional/"
    then raise(Already_optional(old_name))
    else 
    German_relocate_module.on_targets 
    (mdata,old_tgts) old_name (Subdirectory.of_string "Optional");;    
