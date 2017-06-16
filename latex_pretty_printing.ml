(*

#use"latex_pretty_printing.ml";;

*)


let latexify_name_with_index s=
  let n=String.length(s) in
  let rec tempf=(fun j->
    match String.get(s)(j) with
    'a'..'z'->if j<n-1 then tempf(j+1) else (s,"")
    |arall->(Cull_string.beginning j s,Cull_string.cobeginning j s)
  ) in
  let (a,b)=tempf(0) in
  a^"_{"^b^"}";;
  
 let string_of_intlist l=
   let temp1=Image.image string_of_int l in
   let temp2=String.concat(",")(temp1) in
   "\\lbrace "^temp2^" \\rbrace";;
   
 let write_inclusion l=
   let d=List.length(l) in
   if d=0 then "" else 
   if d=1 
   then let s=string_of_int(List.hd l) in
        s^"\\in A"
   else (string_of_intlist l)^"\\subseteq A";;

let write_anti_inclusion l=
   let d=List.length(l) in
   if d=0 then "" else 
   if d=1 
   then let s=string_of_int(List.hd l) in
        s^"\\not\\in A"
   else (string_of_intlist l)^"\\not\\subseteq A";;
   
 let symmetrized_anti_inclusion n l=
   if l=[] then " \\newline" else
   let sym_l=List.rev_map(fun x->n+1-x)(l) in
   if sym_l=l
   then "Gallout a reomp goulakaat $"^(write_anti_inclusion l)^
        "$ eta. \\newline "
   else "Gallout a reomp goulakaat $"^(write_anti_inclusion l)^
        "$ eta, ha $"^(write_anti_inclusion sym_l)^"$ ivez, dre simetriezh.\\newline ";;



 let displaymath math_text=
   let max_line_length=(!(Sliced_string.max_line_length_ref)) in
   let math_text=Str.global_replace(Str.regexp"[\n \t]")("")(math_text) in
   if (String.length(math_text)<=max_line_length)
   then "\\begin{displaymath}\n"^math_text^"\n\\end{displaymath}"
   else
   let temp1=Str.full_split(Str.regexp("+\\|*\\|-\\|="))(math_text) in
   let rec tempf=(fun (graet,breman,da_ober)->
       if da_ober=[] then List.rev(breman::graet) else
       let a=List.hd(da_ober) and peurrest=List.tl(da_ober) in
       match a with
       Str.Delim(delim)->tempf(graet,breman^delim,peurrest)
       |Str.Text(text)->tempf((breman^text)::graet,"",peurrest)
   ) in
   let temp2=tempf([],"",temp1) in
   let temp3=Sliced_string.make_aggregates "" temp2 in
   let temp4=Sliced_string.to_string_list temp3 in
   let temp5=String.concat "\\\\\n" temp4 in
   "\\begin{displaymath}\n"^
   "\\begin{array}{l}\n"^
   temp5^
   "\n\\end{array}"^
   "\n\\end{displaymath}";;
   














   
