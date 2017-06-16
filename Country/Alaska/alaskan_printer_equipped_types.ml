
(* 

#use"Country/Germany/german_printer_equipped_types.ml";;


*)

let from_data mdata=
   Option.filter_and_unpack (
  	fun md->
   	let hm=Modulesystem_data.name md
   	and ap=Modulesystem_data.principal_path md in
   	let text=Io.read_whole_file ap in
   	if (Substring.is_a_substring_of
     ("let "^"print_out ") text)&&
    	(not(Half_dressed_module.is_optional hm))
   	then Some(hm)
  	else None
   ) mdata;;

let instructions printer_equipped_types=
  let temp2=List.rev_map (
    function x->
      "#install_printer "^(Half_dressed_module.capitalized_module_name x)^".print_out;"^";"
  ) printer_equipped_types in
  let temp3="\n\n\n"::(List.rev ("\n\n\n"::temp2)) in
  let part2=String.concat "\n" temp3 in
  part2;;  
 
let declare_printer hm0 l=hm0::l;;
         
let undeclare_printer hm0 l=
  List.filter (fun hm->hm<>hm0) l;;    
 