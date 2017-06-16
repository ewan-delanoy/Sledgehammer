(*

#use"GParser/kparser_house_with_doors.ml";;

*)

module Private=struct
type mistletoe={
     main_opener:string;
     main_closer:string;
     other_enclosers: (string*string) list;
     processed_argument: string;
     initial_index: int;
};;

type walker={
    current_index : int;
    current_depth : int;
    awaited_closer : string option;
    answer : Gparser_result.t option;
};;


let new_walker_in_first_case_in_hwd (m,wlkr)=
   let opt1=Option.find_it(fun (opener,closer)->
     Substring.is_a_substring_located_at opener 
        m.processed_argument wlkr.current_index
   ) m.other_enclosers in
   if opt1<>None
   then let (op1,cl1)=Option.unpack opt1 in
        {
        	current_index =wlkr.current_index+(String.length op1);
            current_depth =wlkr.current_depth;
            awaited_closer=Some(cl1);
            answer =None;
        }
   else
   if Substring.is_a_substring_located_at m.main_opener 
        m.processed_argument wlkr.current_index
   then {
        	current_index =wlkr.current_index+(String.length m.main_opener);
            current_depth =wlkr.current_depth+1;
            awaited_closer=None;
            answer =None;
        }
   else   
   if not(Substring.is_a_substring_located_at m.main_closer 
      m.processed_argument wlkr.current_index)
   then {
        	current_index =wlkr.current_index+1;
            current_depth =wlkr.current_depth;
            awaited_closer=None;
            answer =None;
        }
   else    
   if wlkr.current_depth>1
   then {
        	current_index =wlkr.current_index+(String.length m.main_closer);
            current_depth =wlkr.current_depth-1;
            awaited_closer=None;
            answer =None;
        }
   else  
   let j1=wlkr.current_index+(String.length m.main_closer) in
   let res=Gparser_result.veil
               (m.initial_index,j1-1)
               []
               j1
               None        in
        {
        	current_index =wlkr.current_index+(String.length m.main_closer);
            current_depth =wlkr.current_depth-1;
            awaited_closer=None;
            answer =Some(res);
        };;
   

let first_case_in_hwd 
   (m,wlkr)=(m,new_walker_in_first_case_in_hwd (m,wlkr));;
  
let second_case_in_hwd (m,wlkr)=
  let rparen=Option.unpack wlkr.awaited_closer in
  if Substring.is_a_substring_located_at rparen 
      m.processed_argument wlkr.current_index
  then (m,{
        	current_index =wlkr.current_index+(String.length rparen);
            current_depth =wlkr.current_depth;
            awaited_closer=None;
            answer =None;
        })
  else (m,{
        	current_index =wlkr.current_index+1;
            current_depth =wlkr.current_depth;
            awaited_closer=wlkr.awaited_closer;
            answer =None;
        });;

let pusher_for_hwd w=
  let (m,wlkr)=w in
  if wlkr.answer=None
  then 
       (
         if wlkr.awaited_closer=None
         then first_case_in_hwd w
         else second_case_in_hwd w       
        )
  else w;;  
   
let rec iterator_for_hwd w=
   let (m,wlkr)=w in
   if wlkr.answer<>None
   then wlkr.answer
   else
   if wlkr.current_index>(String.length m.processed_argument)
   then None
   else iterator_for_hwd (pusher_for_hwd w);; 
  
let starter_for_hwd (main_opener,main_closer) other_enclosers s i=
  (
    {
     main_opener=main_opener;
     main_closer=main_closer;
     other_enclosers=other_enclosers;
     processed_argument=s;
     initial_index=i;
    }
  ,
    {
      current_index =i+(String.length main_opener);
      current_depth =1;
      awaited_closer=None;
      answer =None;
    }
  );;
end;;  

let hwd
   (main_opener,main_closer)
     other_enclosers=
   let rec tempf=(fun s i->
        if not(Substring.is_a_substring_located_at main_opener s i)
        then None 
        else 
          
          Private.iterator_for_hwd 
         (Private.starter_for_hwd (main_opener,main_closer) other_enclosers s i)
   ) in
   (tempf:Gparser_fun.t);;      
      
(*

hwd ("(*","*)") ["\"","\""] "(* Bye \"*)\" bye bird *)456" 1;;

*)



         
   
