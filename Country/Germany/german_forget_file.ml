(*

#use"Country/Germany/german_forget_file.ml";;

*)

    
exception FileWithDependencies of 
	Mlx_ended_absolute_path.t*(Half_dressed_module.t list);;


let on_targets triple ap=
  let (mdata,dirs,tgts)=triple in
  let root=German_constant.root in
  let hm=Half_dressed_module.of_path_and_root ap root 
  and mlx=Mlx_ended_absolute_path.of_path_and_root ap root  in
  match Alaskan_data.find_module_registration mdata hm with
   None->triple
  |Some(_)->
   let bel=German_data.below mdata (Mlx_ended_absolute_path.half_dressed_core mlx) in
    if bel=[]
    then let s_hm=Half_dressed_module.to_string hm in
         let fn=(Directory_name.connectable_to_subpath(root))^s_hm in
         let _=Image.image
         (fun edg->Shell_command.do_and_notice_failure("rm -f "^fn^edg^"*"))
         [".cm";".d.cm";".caml_debuggable"] in
         German_unregister_mlx_file.on_targets (mdata,tgts) mlx
    else raise(FileWithDependencies(mlx,bel));;
