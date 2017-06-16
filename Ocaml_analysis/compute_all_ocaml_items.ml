(*

#use"Ocaml_analysis/compute_all_ocaml_items.ml";;

if the string argument has a dot inside it, we interpret it
as a value inside a module.
Otherwise we interpret it as a mere string.

*)


let caoi mdata=
   let temp1=List.filter Modulesystem_data.ml_present mdata in
   let temp2=Image.image (fun md->
     let hm=Modulesystem_data.name md in
     let mlx=Mlx_ended_absolute_path.join hm Ocaml_ending.ml in
     Mlx_ended_absolute_path.to_absolute_path mlx
   ) temp1 in
   Read_ocaml_files.read_ocaml_files temp2
  ;;
   
  
