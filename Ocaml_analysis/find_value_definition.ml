(*

#use"Ocaml_analysis/find_value_definition.ml";;

if the string argument has a dot inside it, we interpret it
as a value inside a module.
Otherwise we interpret it as a mere string.

*)

let fvd mdata s=
   if not(String.contains s '.')
   then None
   else
   let j1=String.index(s)('.')+1 in
   let module_name=Cull_string.beginning (j1-1) s in
   let opt=Option.find_it (fun md->
   Half_dressed_module.naked_module(Modulesystem_data.name md)=
   Naked_module.of_string(String.uncapitalize_ascii(module_name)) ) mdata in
   if opt=None
   then None
   else
   let md1=Option.unpack opt in
   let hm1=Modulesystem_data.name md1 in
   let ap1=Mlx_ended_absolute_path.to_path(Mlx_ended_absolute_path.join hm1 
   	 Ocaml_ending.Ml) in
   let temp1=Read_ocaml_files.read_ocaml_files [ap1] in	 
   Option.find_it (
      fun itm->Ocaml_gsyntax_item.name(itm)=s
   ) temp1;;
   
