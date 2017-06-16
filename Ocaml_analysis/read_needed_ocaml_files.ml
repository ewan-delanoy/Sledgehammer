(*

#use"Ocaml_analysis/read_needed_ocaml_files.ml";;

Given a module, first computes all the dependencies needed to
define this module, then reads the dependencies and 
finally the module itself.

*)

let read_needed_ocaml_files hm=
   let temp1=German_wrapper.data() in
   let md1=Option.find_really (fun md->Modulesystem_data.name md=hm) temp1 in
   let temp2=(Modulesystem_data.all_ancestors md1)@[hm] in
   let all_files=Image.image  (fun hm->
   	 Mlx_ended_absolute_path.to_path(Mlx_ended_absolute_path.join hm Ocaml_ending.Ml)
   ) temp2 in
   Read_ocaml_files.read_ocaml_files all_files;;
