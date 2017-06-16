(*

#use"Country/Germany/german_backup_target_system.ml";;

*)



let github_after_backup=ref(true);;

let commands_for_backup diff=
   if Dircopy_diff.is_empty diff
   then ([],[])
   else 
   let destination_dir=German_constant.dir_for_backup in
   let s_destination=Directory_name.connectable_to_subpath destination_dir in
   let created_ones=Dircopy_diff.recently_created diff in
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
   let temp6=Image.image(
      fun fn->
      "git add "^fn
   ) (created_ones@changed_ones) in
   let temp7=Image.image(
      fun fn->
      "git rm "^fn
   ) (Dircopy_diff.recently_deleted diff) in
   (temp3@temp4@temp5,temp6@temp7);;

let backup_with_message diff msg=
  let destination_dir=German_constant.dir_for_backup  
  and (nongit_cmds,git_cmds)=commands_for_backup diff in
  let s_destination=Directory_name.connectable_to_subpath destination_dir in
  let _=Image.image Shell_command.do_and_notice_failure nongit_cmds in
  let _=(
  if (!github_after_backup)
  then let cwd=Sys.getcwd() in
       let _=Sys.chdir s_destination in
       let _=Image.image Shell_command.do_and_notice_failure git_cmds in
       let _=Image.image Shell_command.do_and_notice_failure
       [
         "git commit -m \""^msg^"\"";
         "git push"
       ] in
       Sys.chdir cwd
  else ()
  ) in
  ();;

let backup diff opt=
  let msg=(
   match opt with
    None->Dircopy_diff.explain diff
   |Some(msg0)->msg0) in
  backup_with_message diff msg;;
  







   
   
  