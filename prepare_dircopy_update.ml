(*

#use"prepare_dircopy_update.ml";;

*)


let compute_deleted_in_diff sourcedir destdir=
   let s_sourcedir=Directory_name.connectable_to_subpath sourcedir
   and s_destdir=Directory_name.connectable_to_subpath destdir in
   let temp1=More_unix.quick_beheaded_complete_ls s_destdir in
   List.filter(
       fun s->(s<>"README")
              &&(not(Substring.begins_with s ".git/")) 
              &&(not(Sys.file_exists(s_sourcedir^s)))
   ) temp1;;
   
let compute_nondeleted_in_diff (sourcedir,l) destdir=
   let s_sourcedir=Directory_name.connectable_to_subpath sourcedir
   and s_destdir=Directory_name.connectable_to_subpath destdir in
   let created_accu=ref[]
   and changed_accu=ref[] in
   let _=Image.image(
   	  fun s->
   	    if (not(Sys.file_exists(s_destdir^s)))
   	    then created_accu:=s::(!created_accu)
   	    else 
   	    (
   	    let txt1=Io.read_whole_file
   	    (Absolute_path.of_string(s_sourcedir^s))
   	    and txt2=Io.read_whole_file
   	    (Absolute_path.of_string(s_destdir^s)) in
   	    if txt1<>txt2
   	    then changed_accu:=s::(!changed_accu)
   	    )
   ) l in
   (Recently_created.of_string_list (!created_accu),
    Recently_changed.of_string_list (!changed_accu));;   
   
  
let compute_diff (sourcedir,l) destdir=
   let (created,changed)=compute_nondeleted_in_diff (sourcedir,l) destdir in
   Dircopy_diff.veil
   
   	(Recently_deleted.of_string_list(compute_deleted_in_diff sourcedir destdir))
   	changed
   	created
   ;;
   
let greedy_list sourcedir=
   let source_paths=More_unix.complete_ls_with_nondirectories_only sourcedir in
   Image.image (fun ap->
   Directory_name.cut_beginning sourcedir (Absolute_path.to_string ap) ) source_paths;;
      
   
let compute_greedy_diff sourcedir destdir=
   compute_diff (sourcedir,greedy_list sourcedir) destdir;;
   
let commands_for_update destination_dir diff=
   if Dircopy_diff.is_empty diff
   then []
   else 
   let s_destination=Directory_name.connectable_to_subpath destination_dir in
   let created_ones=Dircopy_diff.recently_created diff  in
   let temp2=Option.filter_and_unpack
   (fun fn->
     if String.contains fn '/'
     then let dn=Father_and_son.father fn '/' in
          Some("mkdir -p "^s_destination^dn)
     else None 
    )
   created_ones in
   let temp3=Ordered.forget_order(Ordered_string.diforchan temp2) in
   let s_source=Directory_name.connectable_to_subpath German_constant.root in
   let temp4=Image.image(
      fun fn->
      "cp "^s_source^fn^" "^s_destination^(Father_and_son.father fn '/')
   ) created_ones in
   let changed_ones=Dircopy_diff.recently_changed diff in
   let temp5=Image.image(
      fun fn->
      "cp "^s_source^fn^" "^s_destination^fn
   ) changed_ones in
   let temp7=Image.image(
      fun fn->
      "rm "^s_destination^fn
   ) (Dircopy_diff.recently_deleted diff) in
   (temp3@temp4@temp5@temp7);;  
   
   