(*

#use"Php_analizer/php_recognize_block.ml";;

*)


let  recognize_block blckr=
   let (left_blocker,right_blocker)=Php_blocker.token_pair blckr in
  ((function l->
     if l=Positioned_php_token_list.empty then None else
     let (a,peurrest)=Positioned_php_token_list.ht l in
     if  Positioned_php_token.fst(a)<>left_blocker
     then None
     else 
     match Extract_left_block.main (fun x->true) (left_blocker,right_blocker) peurrest 
     with
     None->None
     |Some(((u,last_lxng,others),last_tok))->
        let fst_lxng=fst(Positioned_php_token.snd(a)) in
        Some(Php_char_range.make fst_lxng last_lxng,others)
   ) : Php_recognizer.t);;

