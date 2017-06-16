
(* 

Gathers all (ml/mli/mll/mly) corresponding to the same module.

#use"Makefile_makers/modulesystem_data.ml";;

*)


 

type t={
    name : Half_dressed_module.t;
    ml_present  : bool;
    mli_present : bool;
    mll_present : bool;
    mly_present : bool;
    ml_modification_time : float;
    mli_modification_time : float;
    mll_modification_time : float;
    mly_modification_time : float;
    needed_libraries : Ocaml_library.t list;
    direct_fathers : Half_dressed_module.t list;
    all_ancestors : Half_dressed_module.t list;
    needed_directories : Subdirectory.t list;
};;
   
   

let name x=x.name;;
let ml_present x=x.ml_present;;
let mli_present x=x.mli_present;;
let mll_present x=x.mll_present;;
let mly_present x=x.mly_present;;
let presences x=(x.ml_present,x.mli_present,x.mll_present,x.mly_present);;
let ml_modification_time x=x.ml_modification_time;;
let mli_modification_time x=x.mli_modification_time;;
let mll_modification_time x=x.mll_modification_time;;
let mly_modification_time x=x.mly_modification_time;;
let needed_libraries x=x.needed_libraries;;
let direct_fathers x=x.direct_fathers;;
let all_ancestors x=x.all_ancestors;;
let needed_directories x=x.needed_directories;;

let modification_time x edg=match edg with
   Ocaml_ending.Ml->ml_modification_time x
  |Ocaml_ending.Mli->mli_modification_time x
  |Ocaml_ending.Mll->mll_modification_time x
  |Ocaml_ending.Mly->mll_modification_time x;;

let modification_times x=
  (
   x.ml_modification_time,
   x.mli_modification_time,
   x.mll_modification_time,
   x.mly_modification_time
  );;

let make (nam,mlp,mlip,mllp,mlyp,mlmt,mlimt,mllmt,mlymt,libned,dirfath,allanc,dirned)=
  {
    name=nam;
    ml_present=mlp;
    mli_present=mlip;
    mll_present=mllp;
    mly_present=mlyp;
    ml_modification_time=mlmt;
    mli_modification_time=mlimt;
    mll_modification_time=mllmt;
    mly_modification_time=mlymt;
    needed_libraries=libned;
    direct_fathers=dirfath;
    all_ancestors=allanc;
    needed_directories=dirned;

};;

let compact_make (dir,nam,mlp,mlip,mllp,mlyp,mlmt,mlimt,mllmt,mlymt,libned,dirfath,allanc,dirned)=
  make (Half_dressed_module.of_string_and_root nam dir,
  		mlp,mlip,mllp,mlyp,
  		mlmt,mlimt,mllmt,mlymt,
  		Image.image Ocaml_library.of_string libned,
  		Image.image (fun s->Half_dressed_module.of_string_and_root s dir) dirfath,
  		Image.image (fun s->Half_dressed_module.of_string_and_root s dir) allanc,
  		Image.image Subdirectory.of_string dirned);;

let make_ml_present x=
 {
    name=x.name;
    ml_present=true;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;

let make_mli_present x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=true;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;


};;

let make_mll_present x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=true;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;


};;


let make_mly_present x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=true;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;


};;




let make_ml_absent x=
 {
    name=x.name;
    ml_present=false;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;
  
let make_mli_absent x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=false;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;

let make_mll_absent x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=false;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;


let make_mly_absent x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=false;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;  
  
let check_presence ending dt=match ending with
   Ocaml_ending.Ml->dt.ml_present
  |Ocaml_ending.Mli->dt.mli_present
  |Ocaml_ending.Mll->dt.mll_present
  |Ocaml_ending.Mly->dt.mly_present;;  
  
let make_presence ending dt=match ending with
   Ocaml_ending.Ml->make_ml_present dt
  |Ocaml_ending.Mli->make_mli_present dt
  |Ocaml_ending.Mll->make_mll_present dt
  |Ocaml_ending.Mly->make_mly_present dt;;  

let make_absence ending dt=match ending with
   Ocaml_ending.Ml->make_ml_absent dt
  |Ocaml_ending.Mli->make_mli_absent dt
  |Ocaml_ending.Mll->make_mll_absent dt
  |Ocaml_ending.Mly->make_mly_absent dt;;  
  

let acolytes dt=
  let name=dt.name in
  Option.filter_and_unpack (fun 
    edg->
       if check_presence edg dt 
       then Some(Mlx_ended_absolute_path.join name edg)
       else None
  ) Ocaml_ending.all_endings;;
  

let registered_endings dt=
  List.filter (fun edg->
    check_presence edg dt 
  ) Ocaml_ending.all_endings;;

let short_paths dt=Image.image Mlx_ended_absolute_path.short_path (acolytes dt);;
  

let compute_modification_times hm=
  let dir=Half_dressed_module.bundle_main_dir hm in
  Ocaml_ending.exhaustive_uple (fun edg->
    let mlx=Mlx_ended_absolute_path.join hm edg in
    let file=(Directory_name.connectable_to_subpath dir)^(Mlx_ended_absolute_path.to_string mlx) in
    if not(Sys.file_exists file) then 0. else
    let st=Unix.stat file in
    st.Unix.st_mtime 
  );;

let rename1 new_name x=
   let (ml_mt,mli_mt,mly_mt,mll_mt)=compute_modification_times new_name in
   {
    name=new_name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=ml_mt;
    mli_modification_time=mli_mt;
    mll_modification_time=mll_mt;
    mly_modification_time=mly_mt;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

   };;
   
let rename (old_name,new_name) x=
  if x.name=old_name
  then rename1 new_name x
  else if not(List.mem old_name (x.all_ancestors)) 
       then x
       else 
       let renamer=(fun t->if t=old_name then new_name else t) in
       let renamed_fathers=Image.image renamer x.direct_fathers
       and renamed_ancestors=Image.image renamer x.all_ancestors in
       let renamed_directories=Option.filter_and_unpack(
         fun mlx->
         let s=Half_dressed_module.to_string mlx in
         if String.contains s '/' 
         then Some(Subdirectory.of_string(Father_and_son.father s '/') )
         else None
       ) renamed_ancestors in
       {
    		name=x.name;
    		ml_present=x.ml_present;
   			mli_present=x.mli_present;
    		mll_present=x.mll_present;
    		mly_present=x.mly_present;
    		ml_modification_time=x.ml_modification_time;
    		mli_modification_time=x.mli_modification_time;
    		mll_modification_time=x.mll_modification_time;
    		mly_modification_time=x.mly_modification_time;
    		needed_libraries=x.needed_libraries;
    		direct_fathers=renamed_fathers;
    		all_ancestors=renamed_ancestors;
    		needed_directories=renamed_directories;
	   };;
       
let update_anclibdir changer l_data x=
   if not(List.mem changer.name (x.all_ancestors))
   then x
   else 
   let (anc,llib,dir)=(changer.all_ancestors,changer.needed_libraries,changer.needed_directories) in
   let new_ancestors=Option.filter_and_unpack(
     fun fd->
       let hm=name fd in
       if (List.mem hm x.all_ancestors)||(List.mem hm anc)
       then Some(hm)
       else None
   ) l_data in
   let new_lib=List.filter (
      fun lib->(List.mem lib llib)||(List.mem lib x.needed_libraries)
   ) Ocaml_library.all_libraries in
   let temp1=Option.filter_and_unpack(
     fun hm->
       let s_hm=Half_dressed_module.to_string hm in
       let s_dir=Father_and_son.father s_hm '/' in
       if s_dir="" then None else
       Some(Subdirectory.of_string s_dir)
   )  new_ancestors in
  let new_dir=Ordered.forget_order(Tidel.diforchan temp1) in
   {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=new_lib;
    direct_fathers=x.direct_fathers;
    all_ancestors=new_ancestors;
    needed_directories=new_dir;

   };;       
       
let is_optional x=Half_dressed_module.is_optional(x.name);;
let is_not_optional x=not(is_optional x);;

let is_executable x=Half_dressed_module.is_executable(x.name);;
let is_not_executable x=not(is_executable x);;

let compute_needed_directories l_md=
  let temp1=Image.image(
     fun md->Tidel.safe_set(md.needed_directories)
  ) l_md in
  let temp2=Tidel.big_teuzin temp1 in
  Ordered.forget_order temp2;;
  
let compute_needed_libraries l_md=
  List.filter 
  (
   fun lib->
   List.exists(fun md->List.mem lib md.needed_libraries)
     l_md
  ) 
  Ocaml_library.all_libraries;;  
  
let outdated_acolytes dt=
  let hm=dt.name in
  let (n_ml,n_mli,n_mll,n_mly)=compute_modification_times hm in
  let temp1=[
    Ocaml_ending.mll,dt.mll_modification_time,n_mll;
    Ocaml_ending.mly,dt.mly_modification_time,n_mly;
    Ocaml_ending.ml ,dt.ml_modification_time,n_ml  ;
    Ocaml_ending.mli,dt.mli_modification_time,n_mli;
  ] in
  Option.filter_and_unpack (
    fun (edg,x,y)->
      if x<>y
      then Some(Mlx_ended_absolute_path.join hm edg)
      else None
  ) temp1;;
 
let is_outdated  dt=((outdated_acolytes  dt)<>[]);;


let force_ml_modification_time x new_val=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=new_val;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;


let force_mli_modification_time x new_val=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=new_val;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;

let force_mll_modification_time x new_val=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=new_val;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;

let force_mly_modification_time x new_val=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=new_val;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;

let force_modification_time x edg new_val=match edg with
   Ocaml_ending.Ml->force_ml_modification_time x new_val
  |Ocaml_ending.Mli->force_mli_modification_time x new_val
  |Ocaml_ending.Mll->force_mll_modification_time x new_val
  |Ocaml_ending.Mly->force_mly_modification_time x new_val;; 


let fix_ancestors_and_libs_and_dirs x anc=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=compute_needed_libraries(x::anc);
    direct_fathers=x.direct_fathers;
    all_ancestors=Image.image (fun md->md.name) anc;
    needed_directories=compute_needed_directories(x::anc);
};;

let fix_ancestors x anc=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=anc;
    needed_directories=x.needed_directories;
};;




let needed_dirs_and_libs is_optimized dt=
   let extension=(if is_optimized then ".cmxa" else ".cma") in
   let dirs=String.concat(" ")
    (Image.image(fun y->let z=Subdirectory.connectable_to_subpath(y) in
     if z="" then "" else "-I "^z )
    dt.needed_directories)
	and libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    dt.needed_libraries) in
    String.concat " " ["";dirs;libs;""];;

let needed_dirs_and_libs_for_several is_optimized l_dt=
   let extension=(if is_optimized then ".cmxa" else ".cma") in
   let pre_dirs1=Image.image (fun dt->Tidel.diforchan(dt.needed_directories)) l_dt in
   let pre_dirs2=Ordered.forget_order (Tidel.big_teuzin pre_dirs1) in
   let dirs=String.concat(" ")
    (Image.image(fun y->let z=Subdirectory.connectable_to_subpath(y) in 
    if z="" then "" else "-I "^z )
    pre_dirs2) in
   let pre_libs1=Image.image (fun dt->Tidel.diforchan(dt.needed_libraries)) l_dt in
   let pre_libs2=Ordered.forget_order (Tidel.big_teuzin pre_libs1) in 
   let libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    pre_libs2) in
    String.concat " " ["";dirs;libs;""];;

let principal_mlx x=
   if x.mll_present then Mlx_ended_absolute_path.join x.name Ocaml_ending.mll else
   if x.mly_present then Mlx_ended_absolute_path.join x.name Ocaml_ending.mly else
   if x.ml_present then Mlx_ended_absolute_path.join x.name Ocaml_ending.ml else
   Mlx_ended_absolute_path.join x.name Ocaml_ending.mli;;
   
let principal_path x=Mlx_ended_absolute_path.to_path (principal_mlx x);;  

let ml_path x=Mlx_ended_absolute_path.to_path (Mlx_ended_absolute_path.join x.name Ocaml_ending.ml);;   

let rename_endsubdirectory (old_subdir,new_subdirname) x=
    let ren=Half_dressed_module.rename_endsubdirectory (old_subdir,new_subdirname) 
    and ren_sub=Subdirectory.rename_endsubdirectory (old_subdir,new_subdirname) in
{                                                                 
      name = ren (x.name);
      ml_present = x.ml_present;
      mli_present = x.mli_present;
      mll_present = x.mll_present;
      mly_present = x.mly_present;
      ml_modification_time =  x.ml_modification_time;
      mli_modification_time = x.mli_modification_time;
      mll_modification_time = x.mll_modification_time;
      mly_modification_time = x.mly_modification_time;
      needed_libraries = x.needed_libraries;
      direct_fathers = Image.image ren x.direct_fathers;
      all_ancestors = Image.image ren x.all_ancestors;
      needed_directories = Image.image ren_sub x.needed_directories;
};;

let directories_from_list l=
  let temp2=Image.image (
    fun dt->
       let hm=name dt in
       Half_dressed_module.subdirectory hm
  ) l in
  let temp3=Tidel.diforchan(temp2) in
  Ordered.forget_order temp3;;

  
let industrial_separator1=Industrial_separator.modulesystem_data1;;  
let industrial_separator2=Industrial_separator.modulesystem_data2;;    

  
  
let archive x=
   String.concat industrial_separator1
   [
     Half_dressed_module.archive x.name;
     string_of_bool x.ml_present;
     string_of_bool x.mli_present;
     string_of_bool x.mll_present;
     string_of_bool x.mly_present;
     string_of_float x.ml_modification_time;
     string_of_float x.mli_modification_time;
     string_of_float x.mll_modification_time;
     string_of_float x.mly_modification_time;
     Nonblank.make(String.concat industrial_separator2 (Image.image Ocaml_library.to_string x.needed_libraries));
     Nonblank.make(String.concat industrial_separator2 (Image.image Half_dressed_module.to_string x.direct_fathers));
     Nonblank.make(String.concat industrial_separator2 (Image.image Half_dressed_module.to_string x.all_ancestors));
     Nonblank.make(String.concat industrial_separator2 (Image.image Subdirectory.without_trailing_slash x.needed_directories));
   ];;

let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator1) s in
   let v1=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1  9))
   and v2=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1 10))
   and v3=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1 11))
   and v4=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1 12)) in
   let hm=Half_dressed_module.unarchive(List.hd l1) in
   let dir=Half_dressed_module.bundle_main_dir hm in
{
    name = hm;
    ml_present  = bool_of_string(List.nth l1 1);
    mli_present = bool_of_string(List.nth l1 2);
    mll_present = bool_of_string(List.nth l1 3);
    mly_present = bool_of_string(List.nth l1 4);
    ml_modification_time = float_of_string(List.nth l1 5);
    mli_modification_time = float_of_string(List.nth l1 6);
    mll_modification_time = float_of_string(List.nth l1 7);
    mly_modification_time = float_of_string(List.nth l1 8);
    needed_libraries =Image.image Ocaml_library.of_string v1;
    direct_fathers = Image.image (fun s->Half_dressed_module.of_string_and_root s dir) v2;
    all_ancestors = Image.image (fun s->Half_dressed_module.of_string_and_root s dir) v3;
    needed_directories = Image.image Subdirectory.of_string v4;
};;
     
