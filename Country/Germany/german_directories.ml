
(* 

#use"Country/Germany/german_directories.ml";;


*)


let from_data mdata=
  let temp2=Image.image (
    fun dt->
       let hm=Modulesystem_data.name dt in
       Half_dressed_module.subdirectory hm
  ) mdata in
  let temp3=Tidel.diforchan(temp2) in
  Ordered.forget_order temp3;;
 

