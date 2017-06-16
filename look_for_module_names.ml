(*

#use"look_for_module_names.ml";;

*)

let indices_in_string s=
  let temp1=Outside_comments_and_strings.good_substrings s in
  let temp2=Image.image (fun (a,b,t)->
     let ttemp3=My_str.find_all_occurrences My_str_example.moodle_cases t 1 in
     Image.image (fun (case_index,(u,v))->
        (case_index,(u+a-1,v+a-1))
     ) ttemp3
  ) temp1 in
  List.flatten temp2;;

let names_in_string z=
  let temp1=indices_in_string z in
  let temp2=Image.image (fun (_,(a,b))->String.sub z (a-1) (b-a+1) ) temp1 in
  let temp3=Three_parts.generic temp2 in
  let temp4=List.filter (fun (x,y,z)->not(List.mem y x)) temp3 in
  let temp5=Image.image (fun (x,y,z)->Naked_module.of_string (String.uncapitalize_ascii  y)) temp4 in
  temp5;;

let indices_in_file file=indices_in_string(Io.read_whole_file file);;  
let names_in_file file=names_in_string(Io.read_whole_file file);;

type module_name=string;;

let change_module_name_in_string
   (old_name:module_name)
   (new_name:module_name) s=
   let itv=(fun a b->String.sub s (a-1) (b-a+1)) in
   let temp1=indices_in_string s in
   let temp2=List.filter (fun (j,(a,b))->(itv a b)=old_name ) temp1 in
   if temp2=[]
   then s
   else
   let (_,(a1,b1))=List.hd(temp2) in
   let rec sub_f=(fun (graet,ax,bx,da_ober)->
     match da_ober with
      []->List.rev((itv (bx+1) (String.length s))::graet)
     |(_,(ay,by))::peurrest->
       let s1=itv (bx+1) (ay-1) in
       sub_f(new_name::s1::graet,ay,by,peurrest)
   ) in
   let temp3=sub_f([new_name;itv 1 (a1-1)],a1,b1,List.tl(temp2)) in
   String.concat "" temp3;;
   
 let change_module_name_in_file old_name new_name file=
   let s=Io.read_whole_file file in
   let new_s=change_module_name_in_string old_name new_name s in
   Io.erase_file_and_fill_it_with_string file new_s;;  
   
(*   
   
indices_in_string "123 Haag.012 open Garfield;8";;

indices_in_string "(* Haag. *)234 Dog.\"open Garfield;\"67 Corn.4";;

   
*)   