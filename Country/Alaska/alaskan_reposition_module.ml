
(* 


#use"Country/Alaska/alaskan_reposition_module.ml";;


*)

exception Nonregistered_module of Half_dressed_module.t;;  
  
   
let rpm mdata hm (l_before,l_after)=
   let (before,opt,after)=Three_parts.select_center_element
   (fun dt->
      Modulesystem_data.name dt=hm) mdata in
   if opt=None
   then raise(Nonregistered_module(hm))
   else 
   let info=Option.unpack opt 
   and amputated_data=before@after in
   Alaskan_arrange_positions_in_modulesystem.insert_data 
    amputated_data info (l_before,l_after);;  

