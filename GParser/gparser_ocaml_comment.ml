(*

#use"GParser/kparser_ocaml_comment.ml";;

*)

module Private=struct
type mistletoe={
     comment_opener:string;
     comment_closer:string;
     quote_opener:string;
     quote_closer:string;
     processed_argument: string;
     initial_index: int;
};;

type walker={
    current_index : int;
    current_depth : int;
    quote_mode : bool;
    answer : Gparser_result.t option;
    length_of_preceding_backslash_wall :int;
};;

let update_backslash_wall_length (m,wlkr)=
if (Strung.get m.processed_argument wlkr.current_index)='\\'
then (wlkr.length_of_preceding_backslash_wall)+1
else 0;;


let new_walker_in_first_case_in_hwd (m,wlkr)=
   if Substring.is_a_substring_located_at
     m.quote_opener m.processed_argument wlkr.current_index
   then {
        	current_index =wlkr.current_index+(String.length m.quote_opener);
            current_depth =wlkr.current_depth;
            quote_mode=true;
            answer =None;
            length_of_preceding_backslash_wall=0;
        }
   else
   if Substring.is_a_substring_located_at m.comment_opener 
        m.processed_argument wlkr.current_index
   then {
        	current_index =wlkr.current_index+(String.length m.comment_opener);
            current_depth =wlkr.current_depth+1;
            quote_mode=false;
            answer =None;
            length_of_preceding_backslash_wall=0;
        }
   else   
   if not(Substring.is_a_substring_located_at m.comment_closer 
      m.processed_argument wlkr.current_index)
   then {
        	current_index =wlkr.current_index+1;
            current_depth =wlkr.current_depth;
            quote_mode=false;
            answer =None;
            length_of_preceding_backslash_wall=
               update_backslash_wall_length(m,wlkr);
        }
   else    
   if wlkr.current_depth>1
   then {
        	current_index =wlkr.current_index+(String.length m.comment_closer);
            current_depth =wlkr.current_depth-1;
            quote_mode=false;
            answer =None;
            length_of_preceding_backslash_wall=0;
        }
   else  
   let j1=wlkr.current_index+(String.length m.comment_closer) in
   let res=Gparser_result.veil
               (m.initial_index,j1-1)
               []
               j1
               None        in
        {
        	current_index =wlkr.current_index+(String.length m.comment_closer);
            current_depth =wlkr.current_depth-1;
            quote_mode=false;
            answer =Some(res);
            length_of_preceding_backslash_wall=0;
        };;
   

let first_case_in_hwd 
   (m,wlkr)=(m,new_walker_in_first_case_in_hwd (m,wlkr));;
  
let second_case_in_hwd (m,wlkr)=
  if (Substring.is_a_substring_located_at m.quote_closer
      m.processed_argument wlkr.current_index)
     &&
     ((wlkr.length_of_preceding_backslash_wall mod 2)=0) 
  then (m,{
        	current_index =wlkr.current_index+(String.length m.quote_closer);
            current_depth =wlkr.current_depth;
            quote_mode=false;
            answer =None;
            length_of_preceding_backslash_wall=0;
        })
  else (m,{
        	current_index =wlkr.current_index+1;
            current_depth =wlkr.current_depth;
            quote_mode=true;
            answer =None;
            length_of_preceding_backslash_wall=
               update_backslash_wall_length(m,wlkr);
        });;

let pusher_for_hwd w=
  let (m,wlkr)=w in
  if wlkr.answer=None
  then 
       (
         if not(wlkr.quote_mode)
         then first_case_in_hwd w
         else second_case_in_hwd w       
        )
  else w;;  
   
let rec iterator_for_main_prsr w=
   let (m,wlkr)=w in
   if wlkr.answer<>None
   then wlkr.answer
   else
   if wlkr.current_index>(String.length m.processed_argument)
   then None
   else iterator_for_main_prsr (pusher_for_hwd w);; 
  
let starter_for_main_prsr 
(comment_opener,comment_closer) 
 (quote_opener,quote_closer) s i=
  (
    {
     comment_opener=comment_opener;
     comment_closer=comment_closer;
     quote_opener=quote_opener;
     quote_closer=quote_closer;
     processed_argument=s;
     initial_index=i;
    }
  ,
    {
      current_index =i+(String.length comment_opener);
      current_depth =1;
      quote_mode=false;
      answer =None;
      length_of_preceding_backslash_wall=0;
    }
  );;
end;;  

let main_prsr
   (comment_opener,comment_closer)
     (quote_opener,quote_closer)=
   let rec tempf=(fun s i->
        if not(Substring.is_a_substring_located_at comment_opener s i)
        then None 
        else 
          
          Private.iterator_for_main_prsr 
         (Private.starter_for_main_prsr (comment_opener,comment_closer) 
            (quote_opener,quote_closer) s i)
   ) in
   (tempf:Gparser_fun.t);;      
      
(*

main_prsr ("(*","*)") ("\"","\"") "(* Bye \"*)\" bye bird *)456" 1;;
main_prsr ("(*","*)") ("\"","\"") "(* Bye \"uu\" bye bird *)456" 1;;
main_prsr ("(*","*)") ("\"","\"") "(* Bye \"uu\\\" *) \"  *)234" 1;;


*)



         
   
