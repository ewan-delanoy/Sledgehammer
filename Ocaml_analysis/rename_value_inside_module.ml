(*

#use"Ocaml_analysis/rename_value_inside_module.ml";;

Changes a value's name inside a module. 
This function should never be used by itself except for debugging.

*)

exception No_module_given of string;;

exception No_value_with_name of string;;

let get_module_inside_name s=
   let f=Father_and_son.father s '/' in
   if f=""
   then raise(No_module_given(s))
   else f;;
   
let rename_value_inside_module s new_name=
   let j=Substring.leftmost_index_of_in "." s in
   if j<0 
   then raise(No_module_given(s))
   else 
   let module_name=Cull_string.beginning (j-1) s in
   let hm=German_vague_string.to_module module_name 
   and path=German_vague_string.to_path module_name in 
   let temp1=German_wrapper.data() in
   let md1=Option.find_really (fun md->Modulesystem_data.name md=hm) temp1 in
   let temp2=(Modulesystem_data.all_ancestors md1)@[hm] in
   let all_files=Image.image  (fun hm2->
   	 Mlx_ended_absolute_path.to_path(Mlx_ended_absolute_path.join hm2 Ocaml_ending.Ml)
   ) temp2 in
   let temp3=Read_ocaml_files.read_ocaml_files all_files in
   let opt_temp4=Option.find_it (fun itm->
     (itm.Ocaml_gsyntax_item.name)=s
   ) temp3 in
   if opt_temp4=None
   then raise(No_value_with_name(s))
   else
   let temp4=Option.unpack(opt_temp4) in
   let (i1,j1)=temp4.Ocaml_gsyntax_item.interval_for_name in
   let _=Overwrite_at_intervals.inside_file [(i1,j1),new_name] path in
   let temp3_again=Read_ocaml_files.read_ocaml_files all_files in
   let beheaded_name=Cull_string.cobeginning j s in
   let s_new_beheaded_name=(fun (fa,nn)->if fa="" then nn else fa^"."^nn)
   (Father_and_son.father beheaded_name '.',Overwriter.to_string new_name) in
   let new_beheaded_name=Overwriter.of_string s_new_beheaded_name in
   let s_new_full_name=module_name^"."^s_new_beheaded_name in
   let temp4_again=Option.find_really (fun itm->
     (itm.Ocaml_gsyntax_item.name)=s_new_full_name
   ) temp3_again in
   let k1=Listennou.find_index temp4_again temp3_again in
   let temp5=Listennou.big_tail k1 temp3_again in
   
   let temp6=Option.filter_and_unpack(
      fun itm->
        let txt=itm.Ocaml_gsyntax_item.content in
        let ttemp7=Isolated_occurrences.of_in 
           beheaded_name txt in
        if ttemp7<>[]
        then  let isoc=Isolated_occurrences.of_in beheaded_name txt in
              let replacings=Image.image (fun p->(p,new_beheaded_name)) isoc in
              let new_txt=Overwrite_at_intervals.inside_string
                   replacings txt in
             Some(itm.Ocaml_gsyntax_item.interval_for_content,
                  Overwriter.of_string new_txt)
        else None   
   ) temp5 in
   Overwrite_at_intervals.inside_file temp6 path;;
   
   
   
   
   
   
   
   
   
   
   
   
   