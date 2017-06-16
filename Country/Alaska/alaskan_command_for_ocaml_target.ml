(*

#use"Country/Alaska/alaskan_command_for_ocaml_target.ml";;

*)


let ocamlc="ocamlc  -bin-annot ";;
let ocamlopt="ocamlopt  -bin-annot ";;
let cee=" -c ";;

exception Command_called_on_nodep of Mlx_ended_absolute_path.t;;
exception Unregistered_cmo  of Half_dressed_module.t;;
exception Unregistered_dcmo of Half_dressed_module.t;;
exception Unregistered_cmi  of Half_dressed_module.t;;
exception Unregistered_cma  of Half_dressed_module.t;;
exception Unregistered_cmx  of Half_dressed_module.t;;
exception Unregistered_ml_from_mll of Half_dressed_module.t;;
exception Unregistered_ml_from_mly of Half_dressed_module.t;;
exception Unregistered_executable of Half_dressed_module.t;;
exception Unregistered_debuggable of Half_dressed_module.t;;
exception Unregistered_modules_in_toplevel of string*(Half_dressed_module.t list);;  

let ingr=Alaskan_ingredients_for_ocaml_target.ingredients_for_ocaml_target;;

let cmx_manager=function
 Ocaml_target.CMX(hm2)->
    let s_hm2=Half_dressed_module.to_string hm2 in
    Some(s_hm2^".cmx")
 |_->None;;

let dcmo_manager=function
 Ocaml_target.DCMO(hm2)->
    let s_hm2=Half_dressed_module.to_string hm2 in
    Some(s_hm2^".d.cmo")
 |_->None;;

let command_for_nodep mlx=[];;

let command_for_ml_from_mll dir hm=
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.connectable_to_subpath dir in
          let s_fhm=s_root^s_hm in
          let s_for_display=
          "ocamllex "^
          " -o "^s_hm^".ml"^
          	 " "^s_hm^".mll" 
          and s_for_execution=
          "ocamllex "^
          " -o "^s_fhm^".ml"^
          	 " "^s_fhm^".mll" in
          [Shell_command.semi_usual (s_for_execution,s_for_display)];; 
 
let command_for_ml_from_mly dir hm=
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.connectable_to_subpath(dir) in
          let s_fhm=s_root^s_hm in
          let s_for_display="ocamlyacc "^s_hm^".mly" 
          and s_for_execution="ocamlyacc "^s_fhm^".mly" in
          [Shell_command.semi_usual (s_for_execution,s_for_display)];;  

let command_for_cmi dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cmi(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.connectable_to_subpath(dir) in
          let s_fhm=s_root^s_hm in
          let ending=(
          if Modulesystem_data.mli_present dt
          then ".mli"
          else ".ml"
          ) in
          let s1=
          ocamlc^(Modulesystem_data.needed_dirs_and_libs false dt)^
          cee^s_hm^ending 
          and long_s1=ocamlc^(Modulesystem_data.needed_dirs_and_libs false dt)^
          cee^s_fhm^ending in
          let full_mli=s_root^s_hm^".mli" in
          if (not(Modulesystem_data.mli_present dt))
             &&(Sys.file_exists(full_mli))
          then (* 
                 in this situation the mli file exists but is not registered.
                 So the modulesystem manager must treat it as though it didn't
                 exist. We temporarily rename it so that ocamlc will ignore it.
                *)
                let dummy_mli=s_root^"uvueaoqhkt.mli" in
                let s2="mv "^full_mli^" "^dummy_mli
                and s3="mv "^dummy_mli^" "^full_mli in
                [Shell_command.usual s2;
                 Shell_command.semi_usual (long_s1,s1);
                 Shell_command.usual s3]
          else   [Shell_command.semi_usual (long_s1,s1)];;

let command_for_cmo dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cmo(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.connectable_to_subpath(dir) in
          let s_fhm=s_root^s_hm in
          let s_for_display=
          ocamlc^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_hm^".cmo"^
          cee^s_hm^".ml" 
          and s_for_execution=
          ocamlc^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_fhm^".cmo"^
          cee^s_fhm^".ml"in
          [Shell_command.semi_usual (s_for_execution,s_for_display)];;

let command_for_dcmo dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_dcmo(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.connectable_to_subpath(dir) in
          let s_fhm=s_root^s_hm in
          let s_for_display=
          ocamlc^" -g "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_hm^".d.cmo"^
          cee^s_hm^".ml" 
          and  s_for_execution=
          ocamlc^" -g "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_fhm^".d.cmo"^
          cee^s_fhm^".ml" in
          [Shell_command.semi_usual (s_for_execution,s_for_display)];;

          
let command_for_cma dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cma(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.connectable_to_subpath(dir) in
          let s_fhm=s_root^s_hm in
          let s_for_display=
          ocamlopt^" -a "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_hm^".cma"^
          cee^s_hm^".ml" 
          and s_for_execution=
          ocamlopt^" -a "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_fhm^".cma"^
          cee^s_fhm^".ml" in
          [Shell_command.semi_usual (s_for_execution,s_for_display)];;
 
let command_for_cmx dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cmx(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.connectable_to_subpath(dir) in
          let s_fhm=s_root^s_hm in
          let s_for_display=
          ocamlopt^(Modulesystem_data.needed_dirs_and_libs true dt)^
          " -o "^s_hm^".cma"^
          cee^s_hm^".ml" 
          and s_for_execution=
          ocamlopt^(Modulesystem_data.needed_dirs_and_libs true dt)^
          " -o "^s_fhm^".cma"^
          cee^s_fhm^".ml"in
          [Shell_command.semi_usual (s_for_execution,s_for_display)];; 
 

let command_for_executable dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_executable(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.connectable_to_subpath(dir) in
          let s_fhm=s_root^s_hm in
          let temp1=ingr mdata (Ocaml_target.EXECUTABLE(hm)) in
          let temp2=Option.filter_and_unpack cmx_manager temp1 in
          let long_temp2=Image.image (fun t->s_root^t) temp2 in
          let s_for_display=
          ocamlopt^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_hm^".caml_executable"^
          (String.concat " " temp2) 
          and s_for_execution=
          ocamlopt^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_fhm^".caml_executable"^
          (String.concat " " long_temp2) in
          [Shell_command.semi_usual (s_for_execution,s_for_display)];; 
  
let command_for_debuggable dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_debuggable(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.connectable_to_subpath(dir) in
          let s_fhm=s_root^s_hm in
          let temp1=ingr mdata (Ocaml_target.DEBUGGABLE(hm)) in
          let temp2=Option.filter_and_unpack dcmo_manager temp1 in
          let long_temp2=Image.image (fun t->s_root^t) temp2 in
          let s_for_display=
          ocamlc^" -g "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_hm^".ocaml_debuggable "^
          (String.concat " " temp2) 
          and s_for_execution=
          ocamlc^" -g "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_fhm^".ocaml_debuggable "^
          (String.concat " " long_temp2) in
          [Shell_command.semi_usual (s_for_execution,s_for_display)];; 
  
let command_for_toplevel dir mdata name l=
          let temp1=Image.image (fun hm->(hm,Alaskan_data.find_module_registration mdata hm)) l  in
          let temp2=List.filter (fun x->snd(x)=None) temp1 in
          if temp2<>[]
          then let temp3=Image.image fst temp2 in
               raise(Unregistered_modules_in_toplevel(name,temp3))
          else
          let l_dt=Image.image (fun (_,y)->Option.unpack y) temp1 in
          let temp4=Image.image (fun fd->
             let hm=Modulesystem_data.name fd in
             let s_hm=(Half_dressed_module.to_string hm) in
             if Modulesystem_data.ml_present fd 
             then s_hm^".cmo"
             else " "
          ) l_dt in 
          let s_lhm=String.concat " " temp4 in
          let s_root=Directory_name.connectable_to_subpath(dir) in
          let long_temp4=Image.image (fun fd->
             let hm=Modulesystem_data.name fd in
             let s_hm=(Half_dressed_module.to_string hm) in
             if Modulesystem_data.ml_present fd 
             then s_root^s_hm^".cmo"
             else " "
          ) l_dt in 
          let long_s_lhm=String.concat " " long_temp4 in
          let s_for_display=
          "ocamlmktop "^(Modulesystem_data.needed_dirs_and_libs_for_several false l_dt)^
          " -o "^name^" "^
          "  "^s_lhm^" " 
          and s_for_execution=
          "ocamlmktop "^(Modulesystem_data.needed_dirs_and_libs_for_several false l_dt)^
          " -o "^s_root^name^" "^
          "  "^long_s_lhm^" " in
          [Shell_command.semi_usual (s_for_execution,s_for_display)];;   
 
let command_for_ocaml_target dir mdata tgt=
   match tgt with
  Ocaml_target.NO_DEPENDENCIES(mlx)->command_for_nodep mlx 
 |Ocaml_target.ML_FROM_MLL(hm)->command_for_ml_from_mll dir hm
 |Ocaml_target.ML_FROM_MLY(hm)->command_for_ml_from_mly dir hm
 |Ocaml_target.CMI(hm)->command_for_cmi dir mdata hm
 |Ocaml_target.CMO(hm)->command_for_cmo dir mdata hm
 |Ocaml_target.DCMO(hm)->command_for_dcmo dir mdata hm
 |Ocaml_target.CMA(hm)->command_for_cma dir mdata hm
 |Ocaml_target.CMX(hm)->command_for_cmx dir mdata hm
 |Ocaml_target.EXECUTABLE(hm)->command_for_executable dir mdata hm
 |Ocaml_target.DEBUGGABLE(hm)->command_for_debuggable dir mdata hm
 |Ocaml_target.TOPLEVEL(name,l)->command_for_toplevel dir mdata name l;;
   
  
  
  
 let command_for_ocaml_target_in_dir dir mdata tgt=
   Shell_command.take_care_of_root_directory dir
     (command_for_ocaml_target dir mdata tgt);; 
          

 
