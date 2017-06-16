(*

#use"Optional/ordered_list_creator.ml";;

*)


let double_semicolon=";"^";";;

let unparsed_template ()=
  let john=open_in("Creators/ordered_list_template") in
  let n=in_channel_length(john)+30 in
  (* let _=(print_string("\n"^(string_of_int n)^" characters to read\n");flush stdout) in *)
  let s=Bytes.create(n) in
  (* let _=(print_string("\n"^(string_of_int j)^" characters to go\n");flush stdout) in *)
  let _=close_in(john) in
  s;;

let write_contents_of_ordered_list_module 
   ord_definition type_name list_type_name set_name file_name=
      let temp1=unparsed_template() in
      let temp2=Str.global_replace(Str.regexp "%1")(ord_definition)(temp1) in
      let temp3=Str.global_replace(Str.regexp "%2")(type_name)(temp2) in
      let temp4=Str.global_replace(Str.regexp "%3")(list_type_name)(temp3) in
      let temp5=Str.global_replace(Str.regexp "%4")(set_name)(temp4) in
      let temp6=Str.global_replace(Str.regexp "%%")("%")(temp5) in
      let temp7=Str.global_replace(Str.regexp "\000")("")(temp6) in
      (* let _=(print_string(temp7);flush stdout) in *)
      let jane=open_out("Ordered_Lists/"^file_name) in
      let _=(output_string jane temp7) in
      close_out jane;;
      
 let ord_def_for_ordered_thing_list=
 "let lt x y=x<y"^double_semicolon^"\n"^
 "let cmp=Total_ordering.standard"^double_semicolon;;
 
 let ord_def_for_ordered_pair_list=
 "let lt ((u1,u2):\'a*\'b) ((v1,v2):\'a*\'b)=\n"^
 "        if u1=v1 then u2<v2 else u1<v1"^double_semicolon^"\n"^
 "let cmp=((fun x y->\n"^
 "		if lt(x)(y) then Total_ordering.Lower else\n"^
 ("		if lt(y)(x) then Total_ordering.Greater else\n")^
 "		Total_ordering.Equal): (\'a*\'b) Total_ordering.t)"^double_semicolon^"";;
 
 let ord_def_for_ordered_triple_list=
 "let lt ((u1,u2,u3):\'a*\'b*\'c) ((v1,v2,v3):\'a*\'b*\'c)=\n"^
 "        if u1=v1"^
 "		  then if u2=v2 then u3<v3 else u2<v2"^
 "		  else u1<v1"^double_semicolon^"\n"^
 "let cmp=((fun x y->\n"^
 "		if lt(x)(y) then Total_ordering.Lower else\n"^
 ("		if lt(y)(x) then Total_ordering.Greater else\n")^
 "		Total_ordering.Equal): (\'a*\'b*\'c) Total_ordering.t)"^double_semicolon;;
 
let ord_def_for_ordered_bs_list=
 	let line1="let lt (u:\'a Tidel.set) (v:\'a Tidel.set)="
 	and line2="let lu=Tidel.length(u) and lv=Tidel.length(v) in"
 	and line3="if lu=lv then u<v else lu<lv"^double_semicolon
 	and line4="let cmp=((fun x y->"
 	and line5="		if lt(x)(y) then Total_ordering.Lower else"
 	and line6="		if lt(y)(x) then Total_ordering.Greater else"
 	and line7="		Total_ordering.Equal): (\'a Tidel.set) Total_ordering.t)"^double_semicolon in
 	String.concat "\n" [line1;line2;line3;line4;line5;line6;line7;"\n"];;

let ord_def_for_source=ord_def_for_ordered_bs_list;;
let ord_def_for_well=ord_def_for_ordered_bs_list;;

let ord_def_for_ordered_int_list=
"let cmp=(Total_ordering.standard:>(int Total_ordering.t))"^double_semicolon^
"let lt x y=x<y"^double_semicolon;;
 
let ord_def_for_ordered_string_list=
 	let line1="let lt s1 s2="
 	and line2="	let n1=String.length(s1) and n2=String.length(s2) in"
 	and line3="	if n1=n2"
 	and line4="	then match Ennig.find_it(function j->String.get(s1)(j)<>String.get(s2)(j) )(0)(n1-1)"
 	and line5="		with"
 	and line6="			 None->false"
 	and line7="			|Some(j0)->String.get(s1)(j0)<String.get(s2)(j0) "
 	and line8="	else n1<n2"^double_semicolon 
 	and line9="let cmp=((Total_ordering.from_lt lt):>(string Total_ordering.t))"^double_semicolon in
 	String.concat "\n" [line1;line2;line3;line4;line5;line6;line7;line8;line9;"\n"];; 
 
let ord_def_for_ordered_variable_list=
"let cmp=Variable.cmp"^double_semicolon^
"let lt x y=Total_ordering.lt cmp x y"^double_semicolon;; 
 
let ord_def_for_ordered_constraint_list=
"let cmp=Constraint.cmp"^double_semicolon^
"let lt x y=Total_ordering.lt cmp x y"^double_semicolon^"";;  
 
 
let write_ordered_thing_list ()=
  write_contents_of_ordered_list_module 
   ord_def_for_ordered_thing_list "\'a" "\'a set" "set" "tidel.ml";;
 
let write_ordered_pair_list ()=
  write_contents_of_ordered_list_module 
   ord_def_for_ordered_pair_list "(\'a*\'b)" "(\'a,\'b) set" "set" "tidel2.ml";; 
 
let write_ordered_triple_list ()=
  write_contents_of_ordered_list_module 
   ord_def_for_ordered_triple_list "(\'a*\'b*\'c)" "(\'a,\'b,\'c) set" "set" "tidel3.ml";;  
 
let write_ordered_bs_list ()=
  write_contents_of_ordered_list_module 
   ord_def_for_ordered_bs_list "(\'a Tidel.set)" "\'a set2" "set2" "ordered_bare_set.ml";;  

let write_sourcelike_filter ()=
  write_contents_of_ordered_list_module 
   ord_def_for_source "(\'a Tidel.set)" "\'a filter" "filter" "sourcelike.ml";; 
 
let write_welllike_filter ()=
  write_contents_of_ordered_list_module 
   ord_def_for_well "(\'a Tidel.set)" "\'a filter" "filter" "welllike.ml";;  
 
 
let write_ordered_int_list ()=
  write_contents_of_ordered_list_module 
   ord_def_for_ordered_int_list "int" "set" "set" "ordered_integer.ml";;  
 
let write_ordered_string_list ()=
  write_contents_of_ordered_list_module 
   ord_def_for_ordered_string_list "string" "set" "set" "ordered_string.ml";;  
 
let write_ordered_variable_list ()=
  write_contents_of_ordered_list_module 
   ord_def_for_ordered_variable_list "Variable.t" "set" "set" "ordered_variable.ml";;  
 
let write_ordered_constraint_list ()=
  write_contents_of_ordered_list_module 
   ord_def_for_ordered_constraint_list "Constraint.t" "set" "set" "ordered_constraint.ml";; 
 
 
let write_all ()=
  (
    write_ordered_thing_list ();
    write_ordered_pair_list ();
    write_ordered_triple_list ();
    write_ordered_bs_list();
    write_sourcelike_filter();
    write_welllike_filter();
    write_ordered_int_list ();
    write_ordered_string_list ();
    write_ordered_variable_list ();
    write_ordered_constraint_list ()
  
  );;
 
