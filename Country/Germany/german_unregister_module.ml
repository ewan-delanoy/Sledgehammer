
(* 


#use"Country/Germany/german_unregister_module.ml";;


*)


 
 exception Non_registered_module of Half_dressed_module.t;;  
 exception Derelict_children of Half_dressed_module.t*(Half_dressed_module.t list);;  

 
let on_monitored_modules mdata hm=
    let desc=German_data.descendants mdata [hm] in
    if desc<>[]
    then let temp1=Image.image Modulesystem_data.name desc in
         raise(Derelict_children(hm,temp1))
    else
    let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=hm) mdata in
    if opt=None 
    then raise(Non_registered_module(hm))  
    else 
    let acolytes=Modulesystem_data.acolytes(Option.unpack opt) in
    let short_paths=Image.image Mlx_ended_absolute_path.short_path acolytes in
    (before@after,short_paths);;
    
let on_targets (old_mdata,old_tgts) hm=
 let (new_mdata,short_paths)=on_monitored_modules old_mdata hm in
 let new_dirs=German_directories.from_data new_mdata 
 and new_tgts=List.filter (fun tgt->
   match Ocaml_target.main_module tgt with
   None->false |Some(hm2)->hm2<>hm
 ) old_tgts in
 let default_top=(German_data.default_toplevel new_mdata) in
 let (new_mdata2,new_tgts2)=
   snd(Alaskan_make_ocaml_target.make 
       German_constant.root (new_mdata,new_tgts) default_top) in
  ((new_mdata2,new_dirs,new_tgts2),short_paths);;   
  
