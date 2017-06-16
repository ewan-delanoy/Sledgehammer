(*

It is assumed that no "manual tampering" is made,
e.g. manual rewriting of a ml coming from a mll, etc.


#use"Country/Germany/german_write_makefile.ml";;

*)




let write_makefile_element mdata tgt=
  let temp1=Alaskan_ingredients_for_ocaml_target.ingredients_for_ocaml_target mdata tgt
  and temp2=Alaskan_command_for_ocaml_target.command_for_ocaml_target 
                        German_constant.root mdata tgt in
  let temp3=Image.image Ocaml_target.to_string temp1 in
  let temp4=Sliced_string.make_aggregates " " temp3 in
  let temp5=Sliced_string.to_string_list temp4 in
  let temp6=Image.image Shell_command.command_content temp2 in
  let pre_temp7=String.concat "\n" temp6 in
  let temp7=Str.split(Str.regexp" ") pre_temp7 in
  let temp8=Sliced_string.make_aggregates " " temp7 in
  let temp9=Sliced_string.to_string_list temp8 in 
  let s1=(Ocaml_target.to_string tgt)^" : " 
  and s2=String.concat " \\\n" temp5
  and s3="\n\t"
  and s4=String.concat " \\\n" temp9 in
  String.concat "" [s1;s2;s3;s4];;
  
  
let write_makefile mdata=
  let temp1=German_data.default_targets mdata in
  let temp2=Image.image (write_makefile_element mdata) temp1 in
  let temp3=Image.image Ocaml_target.to_string temp1 in
  let temp4=Sliced_string.make_aggregates " " temp3 in
  let temp5=Sliced_string.to_string_list temp4 in
  let temp6=String.concat " \\\n" temp5 in
  let temp7="clean :\n\trm -r -f "^temp6^"\n\n" in
  String.concat "\n\n" (temp2@[temp7]);;

  