(*

#use"Php_analizer/extract_left_block.ml";;

*)



let main
   f (left_blocker,right_blocker) tok_l=
   let rec tempf=(
   fun (graet,j,da_ober)->
     if Positioned_php_token_list.is_empty da_ober
     then None
     else 
     let (a,peurrest)=Positioned_php_token_list.ht da_ober in 
     let lxm=Positioned_php_token.fst(a) in
     if lxm=left_blocker
     then tempf(Positioned_php_token_list.cons a graet,j+1,peurrest)
     else
     if lxm=right_blocker
     then if j=1
          then Some((Positioned_php_token_list.rev graet,snd(Positioned_php_token.snd(a)),peurrest),a)
          else tempf(Positioned_php_token_list.cons  a graet,j-1,peurrest)
     else 
       if f lxm
       then tempf(Positioned_php_token_list.cons  a graet,j,peurrest)
       else None
   ) in
   tempf(Positioned_php_token_list.empty,1,tok_l);;

