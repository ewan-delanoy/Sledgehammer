(*

#use"Country/Alaska/alaskan_remove_debuggables.ml";;

*)



let rd dir mdata=
   let d=20
   and sroot=Directory_name.connectable_to_subpath dir in
   let tempf=(fun j->
   let stars=String.concat "" (Ennig.doyle (fun t->"*/") 1 j) in
   sroot^stars^"*.d.cm*"
   ) in
   let temp1=String.concat " " (Ennig.doyle tempf 0 d) in
   Shell_command.do_and_notice_failure("rm -f "^temp1);;
   
  

