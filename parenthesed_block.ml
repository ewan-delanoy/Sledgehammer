(*

#use"parenthesed_block.ml";;

Decompose a string into parenthesed blocks.
Sample examples at the end of this file

The same left parenthesis may correspond to different right
parentheses. This is why the currently_open_pars field has type
(parenthesis_pair list) list rather than just parenthesis_pair list.


*)

type parenthesis_pair=string*string;;
type associator=string;;


type data_for_decomposition={
   mutable partial_result : ((parenthesis_pair option)*string) list;
   mutable currently_open_pars : (parenthesis_pair list) list;
   mutable smallest_unprocessed_index : int;
   mutable cursor_location : int; 
};;

let initial_data={
  partial_result=[];
  currently_open_pars=[];
  smallest_unprocessed_index=1;
  cursor_location=1;
};;



let test_for_left_paren_at_index 
   s i ((lparen,rparen):parenthesis_pair)=Substring.is_a_substring_located_at lparen s i;;
 
let test_for_right_paren_at_index 
   s i ((lparen,rparen):parenthesis_pair)=Substring.is_a_substring_located_at rparen s i;;
 
let look_for_left_paren_at_index app s i=
   let rec finder=(fun
    possibilities->match possibilities with
    []->None
    |paren::other_parens->
      if test_for_left_paren_at_index s i paren
      then Some(paren)
      else finder other_parens
   ) in
   finder app;;
  
let process_without_open_pars app  s data=
   match look_for_left_paren_at_index app s data.cursor_location with
     None->(data.cursor_location<-data.cursor_location+1)
    |Some(paren)->
                let (lparen,rparen)=paren in
               let _=(
               if data.currently_open_pars=[]
               then let i_start=data.smallest_unprocessed_index
                    and i_end=data.cursor_location-1 in
                    let _=(data.smallest_unprocessed_index<-data.cursor_location) in
                    if i_start<=i_end 
                    then let enclosed_substring=Cull_string.interval s i_start i_end in
                    	 let new_result=(None,enclosed_substring) in
                    	 data.partial_result<-new_result::(data.partial_result)
               ) in
               let temp1=List.filter (fun par->fst(par)=lparen) app in
               (
                data.currently_open_pars<-(temp1::data.currently_open_pars);
                data.cursor_location<-data.cursor_location+String.length(lparen)
               );;
               
let process_with_open_pars app  s data=
  let temp1=List.hd(data.currently_open_pars) 
  and i=data.cursor_location in
  let opt1=Option.find_it (fun paren->test_for_right_paren_at_index s i paren) temp1 in
  if opt1=None
  then process_without_open_pars app  s data
  else 
       let best_paren=Option.unpack opt1 in
       let (lparen,rparen)=best_paren 
       and new_list=List.tl(data.currently_open_pars) in
       let _=(
          data.currently_open_pars<-new_list;
        (data.cursor_location<-data.cursor_location+String.length(rparen))
       ) in
       if new_list<>[]
       then ()
       else let i_start=data.smallest_unprocessed_index+String.length(lparen)
            and i_end=i-1 in
            let enclosed_substring=Cull_string.interval s i_start i_end in
            let new_result=(Some(best_paren),enclosed_substring) in
            (
                data.partial_result<-new_result::(data.partial_result);
                data.smallest_unprocessed_index<-data.cursor_location
             )
            ;;              

let process app s data=
  if data.currently_open_pars=[]
  then process_without_open_pars app s data
  else process_with_open_pars app s data;;

let final_touch s data=
  let a=data.smallest_unprocessed_index
  and b=String.length s in
  if (a<=b)
  then let new_result=(None,Cull_string.interval s a b) in
       data.partial_result<-new_result::(data.partial_result);;

let decompose_without_taking_blanks_into_account app s=
  let data={
  		partial_result=[];
  		currently_open_pars=[];
  		smallest_unprocessed_index=1;
  		cursor_location=1;
  } in
  while data.cursor_location<=(String.length s)
  do
     process app s data
  done;
  final_touch s data;
  List.rev(data.partial_result);;   

module With_associator=struct

   let test_for_associator_at_index  (asc:associator) s i=
     Substring.is_a_substring_located_at asc s i;;

   let process_without_open_pars (asc:associator) app  s data=
   match look_for_left_paren_at_index app s data.cursor_location with
     None->(
           if not(test_for_associator_at_index asc s data.cursor_location)
           then data.cursor_location<-data.cursor_location+1
           else
                (
                  let i_start=data.smallest_unprocessed_index
                  and i_end=data.cursor_location-1 in
                (
                  if i_start<=i_end 
                    then let enclosed_substring=Cull_string.interval s i_start i_end in
                    	 let new_result=(None,enclosed_substring) in
                    	 data.partial_result<-new_result::(data.partial_result)  
                );
                data.cursor_location<-data.cursor_location+String.length(asc);
                data.smallest_unprocessed_index<-data.cursor_location
                )
           )
    |Some(paren)->
               let (lparen,rparen)=paren in
               let temp1=List.filter (fun par->fst(par)=lparen) app in
               data.currently_open_pars<-(temp1::data.currently_open_pars);
               data.cursor_location<-data.cursor_location+String.length(lparen)
               ;;
               
let process_with_open_pars (asc:associator) app  s data=
  let temp1=List.hd(data.currently_open_pars) 
  and i=data.cursor_location in
  let opt1=Option.find_it (fun paren->test_for_right_paren_at_index s i paren) temp1 in
  if opt1=None
  then (
          match look_for_left_paren_at_index app s data.cursor_location with
     	  None->(data.cursor_location<-data.cursor_location+1)
        |Some(paren)->
               let (lparen,rparen)=paren in
               let temp1=List.filter (fun par->fst(par)=lparen) app in
               data.currently_open_pars<-(temp1::data.currently_open_pars);
               data.cursor_location<-data.cursor_location+String.length(lparen)
              
        )
  else (
       let best_paren=Option.unpack opt1 in
       let (lparen,rparen)=best_paren 
       and new_list=List.tl(data.currently_open_pars) in
       data.currently_open_pars<-new_list;
       data.cursor_location<-data.cursor_location+String.length(rparen)
       );;              

let process asc app s data=
  if data.currently_open_pars=[]
  then process_without_open_pars asc app s data
  else process_with_open_pars asc app s data;;

let final_touch s data=
  let a=data.smallest_unprocessed_index
  and b=String.length s in
  if (a<=b)
  then let new_result=(None,Cull_string.interval s a b) in
       data.partial_result<-new_result::(data.partial_result);;

let decompose_without_taking_blanks_into_account asc app s=
  let data={
  		partial_result=[];
  		currently_open_pars=[];
  		smallest_unprocessed_index=1;
  		cursor_location=1;
  } in
  while data.cursor_location<=(String.length s)
  do
     process asc app s data
  done;
  final_touch s data;
  List.rev_map snd (data.partial_result);;   


end;;

let decompose_with_associator=
  ((With_associator.decompose_without_taking_blanks_into_account):
   associator -> parenthesis_pair list -> string -> string list
  );;

let decompose app s=
  let temp1=decompose_without_taking_blanks_into_account app s in
  let temp2=Option.filter_and_unpack (
     fun (lab,t)->
       let u=Cull_string.trim_spaces t in
       if lab<>None then Some[lab,u] else
       if u="" then None else
       let ttemp1=Str.split(Str.regexp"[ \n\t]+") u in
       Some(Image.image (fun v->(None,v)) ttemp1)
  ) temp1 in
  List.flatten temp2;;

(*

Sample examples :

decompose [ ("(",")");("{","}");("BEGIN","END") ]
("How (much (research effort) is {expected} when) BEGIN posting a"^
"Code Review ENDquestion? A "^
"lot. {{(An absurd amount)}}. More BEGIN than  BEGIN you think END"^
"you ENDare capable of.");;


decompose [ ("[","]+");("[","]*");("BEGIN","END") ]
("ijk [abc [def]+ gh]* lm hhh [nop [qr]* stu]+ vw []+ ab  ");;

decompose [ ("[","]") ] "[ab]cd[efg]";;
decompose_without_taking_blanks_into_account [ ("[","]") ] "[ab]cd[efg]";;
decompose_without_taking_blanks_into_account [ ("[","]") ] "uv[ab][efg]";;
decompose_without_taking_blanks_into_account [ ("(",")") ]
"abc((de)|fgh)|ij|kl|||mno";;

decompose_with_associator "|" [ ("(",")") ]
"abc(de|fgh)|ij|kl|||mno";;

decompose_with_associator "|" [ ("(",")") ]
"|abc(de|fgh)|ij|kl|||mno";;

decompose_with_associator "asc" [ ("(",")") ]
"abc(deascfgh)ascijascklascascascmno";;

decompose_with_associator "|" [ ("(",")") ] "123((67)|012)|56|89|||345";;

*)






