
(* 


#use"Makefile_makers/isidore_abstract_renamer.ml";;

Abstract type to handle renaming of an individual
file in a file system.

The object of type Modulesystem.t is converted
to the abstract version just before renaming,
and unconverted just after.

*)

type data={
  name : Half_dressed_module.t option;
  ml_present : bool;
  mli_present : bool;
  mll_present : bool;
  mly_present : bool;
  ml_modification_time : float;
  mli_modification_time : float;
  mll_modification_time : float;
  mly_modification_time : float;
  needed_libraries : Ocaml_library.t list;
  direct_fathers : (Half_dressed_module.t option) list;
  all_ancestors : (Half_dressed_module.t option) list;
  needed_directories : Subdirectory.t list;
   
};;

let abstractify hm x=
  let abstracter=(fun hm2->
  if hm=hm2 then None else Some(hm2)
  ) in
  
  {
  name = abstracter(Modulesystem_data.name x);
  ml_present = Modulesystem_data.ml_present x;
  mli_present = Modulesystem_data.mli_present x;
  mll_present = Modulesystem_data.mll_present x;
  mly_present = Modulesystem_data.mly_present x;
  ml_modification_time = Modulesystem_data.ml_modification_time x;
  mli_modification_time = Modulesystem_data.mli_modification_time x;
  mll_modification_time = Modulesystem_data.mll_modification_time x;
  mly_modification_time = Modulesystem_data.mly_modification_time x;
  needed_libraries = Modulesystem_data.needed_libraries x;
  direct_fathers =Image.image abstracter (Modulesystem_data.direct_fathers x);
  all_ancestors =Image.image abstracter (Modulesystem_data.all_ancestors x);
  needed_directories = Modulesystem_data.needed_directories x;
};;

let unabstractify hm x=
  let unabstracter=(fun opt->match opt with
  None->hm
  |Some(hm2)->hm2
  ) in
  
  Modulesystem_data.make(
  unabstracter(x.name),
  x.ml_present,
  x.mli_present, 
  x.mll_present, 
  x.mly_present, 
  x.ml_modification_time, 
  x.mli_modification_time, 
  x.mll_modification_time, 
  x.mly_modification_time, 
  x.needed_libraries, 
  Image.image unabstracter (x.direct_fathers),
  Image.image unabstracter (x.all_ancestors),
  x.needed_directories
 );;






 