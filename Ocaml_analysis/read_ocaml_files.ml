(*

#use"Ocaml_analysis/read_ocaml_files.ml";;

*)

module Private=struct
exception Unreadable of string;;

let accuse_final_excerpt s i=
  let j=min(String.length s)(i+100) in
  raise(Unreadable(Cull_string.interval s i j));;

let read1 s=
  let opt=Gparser_apply.apply Gparser_for_ocaml_language.main_prsr s 1 in
  if opt=None then accuse_final_excerpt s 1 else
  let res=Option.unpack opt in 
  let p=Gparser_result.final_cursor_position res in
  if p<=(String.length s) 
  then accuse_final_excerpt s p
  else 
  let temp1=Gparser_result.important_ranges res in
  Image.image (fun (i,j)->
    let opt=Gparser_apply.apply Gparser_for_ocaml_language.elt_prsr s i in
    let res=Option.unpack opt in
    ((i,j),Option.unpack(Gparser_result.disjunction_index res))
  ) temp1;;
  
let describe_value_item s (i,j)=
     let opt=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_value_making s i in
     let res=Option.unpack opt in
     let (i1,j1)=List.nth(Gparser_result.important_ranges res) 
          (Gparser_for_ocaml_language.index_for_name_in_value_parser-1)
     and (i2,j2)=List.nth(Gparser_result.important_ranges res) 
          (Gparser_for_ocaml_language.index_for_content_in_value_parser-1) 
     and (i3,j3)=Gparser_result.whole_range res in
       Ocaml_gsyntax_item.make
          Ocaml_gsyntax_category.Value
          (Cull_string.interval s i1 j1)
          (i1,j1)
          (Cull_string.interval s i3 j3)
          (* the -2 of because of the 2 characters in the double semicolon *)
          (Cull_string.interval s i2 (j2-2))
          (i2,j2-2)
          false;;

let describe_type_item s (i,j)=
     let opt=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_type_making s i in
     let res=Option.unpack opt in
     let (i1,j1)=List.nth(Gparser_result.important_ranges res) 3
     and (i2,j2)=List.nth(Gparser_result.important_ranges res) 6 
     and (i3,j3)=Gparser_result.whole_range res in
       Ocaml_gsyntax_item.make
          Ocaml_gsyntax_category.Type
          (Cull_string.interval s i1 j1)
          (i1,j1)
          (Cull_string.interval s i3 j3)
          (* the -2 of because of the 2 characters in the double semicolon *)
          (Cull_string.interval s i2 (j2-2))
          (i2,j2-2)
          false;;

let describe_exception_item s (i,j)=
     let opt=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_exception_making s i in
     let res=Option.unpack opt in
     let (i1,j1)=List.nth(Gparser_result.important_ranges res) 2
     and (i2,j2)=List.nth(Gparser_result.important_ranges res) 3 
     and (i3,j3)=Gparser_result.whole_range res in
       Ocaml_gsyntax_item.make
          Ocaml_gsyntax_category.Exception
          (Cull_string.interval s i1 j1)
          (i1,j1)
          (Cull_string.interval s i3 j3)
          (* the -2 of because of the 2 characters in the double semicolon *)
          (Cull_string.interval s i2 (j2-2))
          (i2,j2-2)
          false;;

let describe_module_opener_item s (i,j)=
     let opt=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_module_opener s i in
     let res=Option.unpack opt in
     let (i1,j1)=List.nth(Gparser_result.important_ranges res) 2
     and (i3,j3)=Gparser_result.whole_range res in 
       Ocaml_gsyntax_item.make
          Ocaml_gsyntax_category.Module_opener
          (Cull_string.interval s i1 j1)
          (i1,j1)
          (Cull_string.interval s i3 j3)
          ""
          (0,0)
          false;;


let describe_module_closer_item=
       Ocaml_gsyntax_item.make
          Ocaml_gsyntax_category.Module_closer
          ""
          (0,0)
          ""
          ""
          (0,0)
          false;;


let describe_module_inclusion_item s (i,j)=
     let opt=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_module_inclusion s i in
     let res=Option.unpack opt in
     let (i1,j1)=List.nth(Gparser_result.important_ranges res) 2 
     and (i3,j3)=Gparser_result.whole_range res in 
       Ocaml_gsyntax_item.make
          Ocaml_gsyntax_category.Module_inclusion
          (Cull_string.interval s i1 j1)
          (i1,j1)
          (Cull_string.interval s i3 j3)
          ""
          (0,0)
          false;;
          
 let describe_item s ((i,j),idx)=
   if idx=Gparser_for_ocaml_language.index_for_value
   then Some(describe_value_item s (i,j))
   else
   if idx=Gparser_for_ocaml_language.index_for_type
   then Some(describe_type_item s (i,j))
   else
   if idx=Gparser_for_ocaml_language.index_for_exception
   then Some(describe_exception_item s (i,j))
   else
   if idx=Gparser_for_ocaml_language.index_for_module_opener
   then Some(describe_module_opener_item s (i,j))
   else
   if idx=Gparser_for_ocaml_language.index_for_module_closer
   then Some(describe_module_closer_item)
   else          
   if idx=Gparser_for_ocaml_language.index_for_module_inclusion
   then Some(describe_module_inclusion_item s (i,j))
   else None;;
   
let read2 s=
   Option.filter_and_unpack (describe_item s) (read1 s);;   
   
let module_inclusion_in_pusher    
   (graet,current_full_namespace,current_names) x=
    let included_module=x.Ocaml_gsyntax_item.name in
  		  let full_namespace=current_full_namespace^"."^included_module in
  		  let maybe_included_items=List.filter(
  		     fun y->let nm_y=y.Ocaml_gsyntax_item.name in
  		     (Substring.begins_with nm_y full_namespace)
  		     ||
  		     (Substring.begins_with nm_y included_module)  
  		  ) graet in 
  		  (* local redifinition has priority over an outside definition *)
  		  let chosen_namespace=(if
  		    List.exists(fun y->
  		      y.Ocaml_gsyntax_item.name=included_module
  		    ) maybe_included_items
  		    then included_module
  		    else full_namespace
  		  ) in
         let included_items=List.filter(
         	fun y->y.Ocaml_gsyntax_item.name=chosen_namespace
         ) maybe_included_items in
         let renamed_included_items=Image.image 
         (Ocaml_gsyntax_item.include_in_new_namespace full_namespace )
         included_items in
         (List.rev_append renamed_included_items graet,current_full_namespace,current_names);;
   
let first_pusher_from_level2_to_level3  
   walker_state x=
   let (graet,current_full_namespace,current_names)=walker_state in
  match x.Ocaml_gsyntax_item.category with
    Ocaml_gsyntax_category.Value                                                                          
  | Ocaml_gsyntax_category.Type
  | Ocaml_gsyntax_category.Exception->
          let new_x=Ocaml_gsyntax_item.prepend_prefix current_full_namespace x in
          (new_x::graet,current_full_namespace,current_names)
  | Ocaml_gsyntax_category.Module_opener->
          let new_name=x.Ocaml_gsyntax_item.name in
          let new_names=current_names@[new_name] in
          let new_full_namespace=String.concat "." new_names in
          (graet,new_full_namespace,new_names)
  | Ocaml_gsyntax_category.Module_closer->
          let new_names=List.rev(List.tl(List.rev(current_names))) in
          let new_full_namespace=String.concat "." new_names in
          (graet,new_full_namespace,new_names)
  | Ocaml_gsyntax_category.Module_inclusion->
         module_inclusion_in_pusher (graet,current_full_namespace,current_names) x;;

exception Pusher23_exn;;

let pusher_from_level2_to_level3 (walker_state,da_ober)=
   match da_ober with
   []->raise(Pusher23_exn)
   |x::peurrest->(first_pusher_from_level2_to_level3 walker_state x,peurrest);;    

         
let rec iterator_from_level2_to_level3 (walker_state,da_ober)=
   if da_ober=[] 
   then let  (graet,_,_)=walker_state in List.rev graet
   else iterator_from_level2_to_level3(pusher_from_level2_to_level3 (walker_state,da_ober));; 


let from_level2_to_level3 data_before (current_module,l)=
    iterator_from_level2_to_level3 
      ((data_before,current_module,Strung.split '.' current_module),l);;

end;;

let read_ocaml_files l_ap=
   let temp1=Image.image( fun ap->
   let s_ap=Absolute_path.to_string ap
   and text=Io.read_whole_file ap in
   let unpointed=Father_and_son.father s_ap '.' in
   let module_name=String.capitalize_ascii (Father_and_son.son unpointed '/') in
   (module_name,Private.read2 text)   
   ) l_ap in 
   List.fold_left Private.from_level2_to_level3 [] temp1;;
   
   
(*

let g1=German_wrapper.data();;
let g2=List.filter Modulesystem_data.ml_present g1;;
let g3=List.flatten (image Modulesystem_data.acolytes g2);;
let g4=List.filter (fun mlx->snd(Mlx_filename.decompose mlx)=Ocaml_ending.ml) g3;;
let g5=image Mlx_filename.to_absolute_path g4;;

let g6=read3 g5;;


let g6=image (fun ap->let s=Io.read_whole_file ap in
  (-(String.length s),(ap,s))
) g5 ;;
let g7=image snd (ofo(Tidel2.diforchan g6));;
let g8=Explicit.image (fun (ap,s)->(ap,read2 s)) g7;;
let g9=Explicit.image (fun (ap,l)->
  from_level2_to_level3 ([],"Moody") l
) g8;;

*)

  
(*  

let s1="let jiving=234  ;;";;
describe_value_item s1 (1,String.length s1);;

let s2="type ('a,'b) sister=('a list)*'b*string;;";;
describe_type_item s2 (1,String.length s2);;

let s3="type sister=(int list)*float*string;;";;
describe_type_item s3 (1,String.length s3);;

let s4="exception Foobar of string*int;;";;
describe_exception_item s4 (1,String.length s4);;

let s5="exception Foobar;;";;
describe_exception_item s5 (1,String.length s5);;

let s6="module  Foobar=struct";;
describe_module_opener_item s6 (1,String.length s6);;

let s7="end\n;;";;
describe_module_opener_item s7 (1,String.length s7);;

let s8="include Leap\n;;";;
describe_module_inclusion_item s8 (1,String.length s8);;
   
*)   
   
     
  