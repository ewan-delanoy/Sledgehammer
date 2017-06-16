
(* 

#use"Country/Alaska/alaskan_data.ml";;


*)

let all_mlx_files mdata=
  List.flatten
  (Image.image Modulesystem_data.acolytes mdata);; 

let all_mlx_paths mdata=Image.image Mlx_ended_absolute_path.to_absolute_path 
  (all_mlx_files mdata);;  

let all_short_paths mdata=List.flatten(
  Image.image Modulesystem_data.short_paths mdata
);;

let default_toplevel main_toplevel_name mdata=
  let temp2=List.filter Modulesystem_data.is_not_optional mdata in
  let temp3=Image.image Modulesystem_data.name temp2 in
  let temp4=List.filter (fun hm->
     Half_dressed_module.to_string(hm)<>Debugger_name.debugger_name
  ) temp3
  in
  Ocaml_target.toplevel main_toplevel_name temp4;; 
 
let find_module_registration mdata hm=
  Option.find_it(fun a->Modulesystem_data.name a=hm) mdata;;   