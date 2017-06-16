(*

A make-style type for compiler management.
There are built-in targets, plus a variant for
manually built special targets.

Note that  when the ml file is present, the ocamlc -c command produces the
.cmi and .cmo at the same time, so that there is no need to compile the
.cmi separately.

#use"Makefile_makers/ocaml_target.ml";;

*)

type target_name=string;;

type t=
  NO_DEPENDENCIES of Mlx_ended_absolute_path.t
 |ML_FROM_MLL of Half_dressed_module.t
 |ML_FROM_MLY of Half_dressed_module.t 
 |CMI of Half_dressed_module.t
 |CMO of Half_dressed_module.t
 |DCMO of Half_dressed_module.t
 |CMA of Half_dressed_module.t
 |CMX of Half_dressed_module.t
 |EXECUTABLE of Half_dressed_module.t
 |DEBUGGABLE of Half_dressed_module.t
 |TOPLEVEL of target_name*(Half_dressed_module.t list);;
 
 
let to_string =function
  NO_DEPENDENCIES(mlx)->Mlx_ended_absolute_path.to_string mlx
 |ML_FROM_MLL(hm)->(Half_dressed_module.to_string hm)^".ml"
 |ML_FROM_MLY(hm)->(Half_dressed_module.to_string hm)^".ml" 
 |CMI(hm)->(Half_dressed_module.to_string hm)^".cmi"
 |CMO(hm)->(Half_dressed_module.to_string hm)^".cmo"
 |DCMO(hm)->(Half_dressed_module.to_string hm)^".d.cmo"
 |CMA(hm)->(Half_dressed_module.to_string hm)^".cma"
 |CMX(hm)->(Half_dressed_module.to_string hm)^".cmx"
 |EXECUTABLE(hm)->(Half_dressed_module.to_string hm)^".caml_executable"
 |DEBUGGABLE(hm)->(Half_dressed_module.to_string hm)^".caml_debuggable"
 |TOPLEVEL(name,l)->name;;

let test_path dir tgt=
  let d=Directory_name.connectable_to_subpath dir in
  Sys.file_exists(d^(to_string tgt));; 
 
let path dir tgt=
 let d=Directory_name.connectable_to_subpath dir in
 Absolute_path.of_string(d^(to_string tgt));;

let is_a_debuggable=function
  DEBUGGABLE(_)->true
 |_->false;;

let is_not_a_debuggable x=not(is_a_debuggable x);; 
 

let toplevel_data=function
  NO_DEPENDENCIES(mlx)->None
 |ML_FROM_MLL(hm)-> None
 |ML_FROM_MLY(hm)-> None 
 |CMI(hm)-> None
 |CMO(hm)-> None
 |DCMO(hm)-> None
 |CMA(hm)-> None
 |CMX(hm)-> None
 |EXECUTABLE(hm)-> None
 |DEBUGGABLE(hm)-> None
 |TOPLEVEL(name,l)->Some(name,l);;

let toplevel_name tgt=match toplevel_data tgt with
None->None |Some(name,_)->Some(name);;

let is_a_toplevel tgt=match toplevel_data tgt with
None->false |Some(_,_)->true;;

let is_a_nodep tgt=function
  NO_DEPENDENCIES(_)->true
  |_->false;;

let adhoc_test_for_renaming old_name=function
  NO_DEPENDENCIES(mlx)->(Mlx_ended_absolute_path.half_dressed_core mlx)<>old_name
 |_->true;;

let naive_main_module=function
  NO_DEPENDENCIES(mlx)->Some(Mlx_ended_absolute_path.half_dressed_core mlx)
 |ML_FROM_MLL(hm)-> Some(hm)
 |ML_FROM_MLY(hm)-> Some(hm) 
 |CMI(hm)-> Some(hm)
 |CMO(hm)-> Some(hm)
 |DCMO(hm)-> Some(hm)
 |CMA(hm)-> Some(hm)
 |CMX(hm)-> Some(hm)
 |EXECUTABLE(hm)-> Some(hm)
 |DEBUGGABLE(hm)-> Some(hm)
 |TOPLEVEL(name,l)->None;;

let main_module tgt=try naive_main_module tgt with _->None;;

let no_dependencies mlx=NO_DEPENDENCIES(mlx);;
let ml_from_mll hm=ML_FROM_MLL(hm);; 
let ml_from_mly hm=ML_FROM_MLY(hm);;
let cmi hm=CMI(hm);;
let cmo hm=CMO(hm);;
let dcmo hm=DCMO(hm);;
let cma hm=CMA(hm);; 
let cmx hm=CMX(hm);;
let executable hm=EXECUTABLE(hm);; 
let debuggable hm=DEBUGGABLE(hm);; 
let toplevel name l=TOPLEVEL(name,l);;

let direct_connection hm0=function
  NO_DEPENDENCIES(mlx)->(Mlx_ended_absolute_path.half_dressed_core mlx)=hm0
 |ML_FROM_MLL(hm)-> hm=hm0
 |ML_FROM_MLY(hm)-> hm=hm0
 |CMI(hm)-> hm=hm0
 |CMO(hm)-> hm=hm0
 |DCMO(hm)->hm=hm0
 |CMA(hm)-> hm=hm0
 |CMX(hm)-> hm=hm0
 |EXECUTABLE(hm)-> hm=hm0
 |DEBUGGABLE(hm)-> hm=hm0
 |TOPLEVEL(name,l)->List.mem hm0 l;;


 
let ml_from_lex_or_yacc_data=function 
    ML_FROM_MLL(hm)->Some(Mlx_ended_absolute_path.join hm Ocaml_ending.ml)
   |ML_FROM_MLY(hm)->Some(Mlx_ended_absolute_path.join hm Ocaml_ending.ml)
   |_->None;;
 
let complexity_level=function
  NO_DEPENDENCIES(_)->0 
 |ML_FROM_MLL(_)
 |ML_FROM_MLY(_)->1
 |CMI(_)
 |CMO(_)
 |DCMO(_)
 |CMA(_)
 |CMX(_)->2
 |EXECUTABLE(_)
 |DEBUGGABLE(_)
 |TOPLEVEL(_,_)->3;;
  
let still_up_to_date_test hms_to_be_updated=function
   TOPLEVEL(name,l_hm)->List.for_all
                    (
                      fun hm2->
                      not(List.mem hm2 hms_to_be_updated)
                    ) l_hm
  |tgt2->not(List.mem (Option.unpack(main_module tgt2)) hms_to_be_updated);;
  
let  still_up_to_date_targets hms_to_be_updated l=
  List.filter (
     still_up_to_date_test hms_to_be_updated
  ) l;; 
  
let from_modulesystem_data dt=
  let hm=Modulesystem_data.name dt 
  and (mlp,mlip,mllp,mlyp)=Modulesystem_data.presences dt in
  let temp1=[
                mllp,ml_from_mll hm;
                mlyp,ml_from_mly hm;
           mlp||mlip,cmi hm;
     	   mlp||mlip,cmo hm;
           mlp||mlip,cma hm;
           mlp||mlip,cmx hm;
                 mlp,executable hm;
  ] in
Option.filter_and_unpack (fun x->if fst x then Some(snd x) else None) temp1;;  



let rename_endsubdirectory (old_subdir,new_subdirname) x=
  let on_half_dressed_module=Half_dressed_module.rename_endsubdirectory in
  match x with
  NO_DEPENDENCIES(mlx)->
  no_dependencies(Mlx_ended_absolute_path.rename_endsubdirectory (old_subdir,new_subdirname) mlx)
 |ML_FROM_MLL(hm)->ml_from_mll(on_half_dressed_module (old_subdir,new_subdirname) hm)
 |ML_FROM_MLY(hm)->ml_from_mly(on_half_dressed_module (old_subdir,new_subdirname) hm)
 |CMI(hm)->cmi(on_half_dressed_module (old_subdir,new_subdirname) hm)
 |CMO(hm)->cmo(on_half_dressed_module (old_subdir,new_subdirname) hm)
 |DCMO(hm)->dcmo(on_half_dressed_module (old_subdir,new_subdirname) hm)
 |CMA(hm)->cma(on_half_dressed_module (old_subdir,new_subdirname) hm)
 |CMX(hm)->cmx(on_half_dressed_module (old_subdir,new_subdirname) hm)
 |EXECUTABLE(hm)->executable(on_half_dressed_module (old_subdir,new_subdirname) hm)
 |DEBUGGABLE(hm)->debuggable(on_half_dressed_module (old_subdir,new_subdirname) hm)
 |TOPLEVEL(name,l)->
          let new_l=Image.image (on_half_dressed_module (old_subdir,new_subdirname)) l in
          TOPLEVEL(name,new_l);;
  

  
let industrial_separator1=Industrial_separator.ocaml_target1;;  
let industrial_separator2=Industrial_separator.ocaml_target2;;    


let prepare_archive=function
  NO_DEPENDENCIES(mlx)->["nodep";Mlx_ended_absolute_path.archive mlx]
 |ML_FROM_MLL(hm)-> ["mll";Half_dressed_module.archive hm]
 |ML_FROM_MLY(hm)-> ["mly";Half_dressed_module.archive hm]  
 |CMI(hm)->  ["cmi";Half_dressed_module.archive hm]
 |CMO(hm)->  ["cmo";Half_dressed_module.archive hm]
 |DCMO(hm)-> ["dcmo";Half_dressed_module.archive hm]
 |CMA(hm)->  ["cma";Half_dressed_module.archive hm]
 |CMX(hm)->  ["cmx";Half_dressed_module.archive hm]
 |EXECUTABLE(hm)-> ["exe";Half_dressed_module.archive hm]
 |DEBUGGABLE(hm)-> ["dbg";Half_dressed_module.archive hm]
 |TOPLEVEL(name,l)->["top";name;
  Nonblank.make(String.concat industrial_separator1 (Image.image Half_dressed_module.archive l))];;
  
  
  
exception Unrecognized_constructor of string;;   
  
let archive x=String.concat industrial_separator2 (prepare_archive x);;
 

let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator2) s in
   let c=List.hd l1 and ms=List.nth l1 1 in
   if c="nodep" then NO_DEPENDENCIES(Mlx_ended_absolute_path.unarchive ms) else
   if c="mll"  then  ML_FROM_MLL(Half_dressed_module.unarchive ms) else
   if c="mly"  then  ML_FROM_MLY(Half_dressed_module.unarchive ms) else
   if c="cmi"  then          CMI(Half_dressed_module.unarchive ms) else
   if c="cmo"  then          CMO(Half_dressed_module.unarchive ms) else
   if c="dcmo" then         DCMO(Half_dressed_module.unarchive ms) else
   if c="cma"  then          CMA(Half_dressed_module.unarchive ms) else
   if c="cmx"  then          CMX(Half_dressed_module.unarchive ms) else
   if c="exe"  then   EXECUTABLE(Half_dressed_module.unarchive ms) else
   if c="dbg"  then   DEBUGGABLE(Half_dressed_module.unarchive ms) else
   if c="top" 
   then let v1=Str.split (Str.regexp_string industrial_separator1) (Nonblank.decode(List.nth l1  2)) in 
        TOPLEVEL(ms,Image.image Half_dressed_module.unarchive v1) 
   else
   raise(Unrecognized_constructor(c));;