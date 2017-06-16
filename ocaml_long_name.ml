(*

#use"ocaml_long_name.ml";;

*)
   
let ocaml_long_name tab s=
  let temp5=Str.global_replace (Str.regexp_string "\"") "\\\"" s in
  let temp6=Str.global_replace (Str.regexp_string "\\") "\\\\" temp5 in
  let temp1=Str.split (Str.regexp_string "\n") temp6 in
  let temp2=Image.image (fun t->"\""^t^"\\n\"") temp1 in
  let (left_encloser,right_encloser,final_tab)=(
    if List.length(temp1)>1
    then (tab^"(\n"^tab^" ","\n"^tab^")","^\n"^tab^" ")
    else ("","","^\n"^tab)
  ) in
  let temp3=String.concat final_tab temp2 in
  let temp4="\n\n\n"^left_encloser^temp3^right_encloser^"\n\n\n" in
  temp4;;
     

     
   
