
(* 


#use"Country/Germany/german_relocate_module.ml";;

We separate renaming from relocating. The latter changes
directories while the former changes only the name, staying in the
same directory. 

Speech follows action : file displacement takes place first, 
then the Ocaml data values are updated.

*)


exception NonoptDependingOnOpt of Half_dressed_module.t*(Half_dressed_module.t list);;  
exception Nonregistered_module of Half_dressed_module.t;;  
 
let on_monitored_modules mdata old_name new_subdir=
   let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=old_name) mdata in
   if opt=None
   then raise(Nonregistered_module(old_name))
   else 
   let old_dt=Option.unpack opt in
   let old_acolytes=Modulesystem_data.acolytes old_dt in
   let old_files=Image.image (fun mlx->Mlx_ended_absolute_path.short_path mlx) old_acolytes in 
   let new_acolytes=Image.image (fun mlx->Mlx_ended_absolute_path.do_file_displacing mlx new_subdir) old_acolytes in
   let new_files=Image.image (fun mlx->Mlx_ended_absolute_path.short_path mlx) new_acolytes in 
   let new_name=Mlx_ended_absolute_path.half_dressed_core(List.hd new_acolytes) in
   let desc=German_data.descendants mdata [old_name] in
   let data_renamer=Modulesystem_data.rename (old_name,new_name) in
   let bowls=(	Half_dressed_module.is_optional old_name,
				Half_dressed_module.is_optional new_name) in
   let cmd2=Shell_command.usual ("rm -f "^(Half_dressed_module.to_string old_name)^".cm* ") in
   let cmd=Shell_command.take_care_of_root_directory German_constant.root [cmd2] in
   let _=Image.image Shell_command.announce_and_do cmd in
   if bowls=(false,true)
   then 
        let mandatory_desc=List.filter Modulesystem_data.is_not_optional desc in
        if mandatory_desc<>[]
        then let temp1=Image.image Modulesystem_data.name mandatory_desc in
             raise(NonoptDependingOnOpt(new_name,temp1))
        else 
        let (before2,after2)=German_data.optionality_partition after in
        let part1=before@before2
        and part2=Image.image data_renamer (old_dt::after2) in
        (part1@part2,(old_files,new_files))
   else
   if bowls=(true,false)
   then let (before1,after1)=German_data.optionality_partition before in
        let part2=Image.image data_renamer (old_dt::after1@after) in
        (before1@part2,(old_files,new_files))
   else let part2=Image.image data_renamer (old_dt::after) in
        (before@part2,(old_files,new_files));;

let on_targets (old_mdata,old_tgts) old_name new_subdir= 
  let untouched_tgts=List.filter
   (fun tgt->not(Alaskan_ingredients_for_ocaml_target.module_dependency_for_ocaml_target
   old_mdata [old_name] tgt)&&(Ocaml_target.main_module(tgt)<>Some(old_name)) ) old_tgts in
  let (new_mdata,(old_files,new_files))=on_monitored_modules old_mdata old_name new_subdir in
  let default_top=(German_data.default_toplevel new_mdata) in
  let (new_mdata2,new_tgts2)=
   snd(Alaskan_make_ocaml_target.make 
     German_constant.root
    (new_mdata,untouched_tgts) default_top) in
  ((new_mdata2,new_tgts2),(old_files,new_files));;   
 
