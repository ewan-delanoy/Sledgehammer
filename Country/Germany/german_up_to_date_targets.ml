
(* 

#use"Country/Germany/german_up_to_date_targets.ml";;


*)


let loadings (dirs,tgts)=
  let s_root=Directory_name.connectable_to_subpath(German_constant.root) in
  let part1="\n(*\n #use\""^s_root^(German_constant.name_for_loadingsfile)^"\";"^";\n*)\n\n" in
  let temp5=Image.image(
     fun sd->
     "#directory\""^s_root^(Subdirectory.connectable_to_subpath sd)^"\";"^";"
  ) dirs in
  let part2=String.concat "\n" temp5 
  and part3="\n\n#load\"nums.cma\";"^";\n#load\"str.cma\";"^";\n#load\"unix.cma\";"^";\n\n\n" in
  let temp2=Option.filter_and_unpack (
    function (Ocaml_target.CMO(x))->
      Some("#load\""^(Half_dressed_module.to_string x)^".cmo\";"^";") 
    |_->None
  ) tgts in
  let temp3="\n\n\n"::(List.rev ("\n\n\n"::temp2)) in
  let part4=String.concat "\n" temp3 in
  part1^part2^part3^part4;; 
  


let add_target_perhaps opt_tgt l=Option.add_perhaps opt_tgt l;;

  
