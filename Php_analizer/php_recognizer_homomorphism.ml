(*

#use"Php_analizer/php_recognizer_homomorphism.ml";;

A php_recognizer only recognizes patterns, giving a yes/no
answer and does not go deeper
into how the pattern is realized.
It can be viewed as a special Php_combinator.parser object :
type Php_recognizer.t would be equivalent to "unit Php_combinator.parser".
This identification only works 99% of the way though.

For example, Php_combinator.chain has type
'a parser list -> 'a list parser, while the chain here has type
t list-> t, as "unit list" can be identified with "unit".


*)


exception Stepper_for_chain_exn;;   
  
let stepper_for_chain 
  (da_ober,support,nondummy_left_lexings,nondummy_right_lexings,should_exit)=
   match da_ober with
   []->raise(Stepper_for_chain_exn)
   |rcgzr::peurrest->
     (
        match Php_recognizer.recognize rcgzr support with
       None->([],Positioned_php_token_list.empty,[],[],true)
      |Some(cr,support2)->
        (peurrest,support2,
          Php_char_range.add_if_nondummy (Php_char_range.fst cr) nondummy_left_lexings,
          Php_char_range.add_if_nondummy (Php_char_range.snd cr) nondummy_right_lexings, 
          false
        )    
     )
;;  

let rec helper_for_chain x=
    let (da_ober,support,nondummy_left_lexings,nondummy_right_lexings,should_exit)=x in
    if should_exit
    then None
    else 
    if da_ober=[]
    then  let u=Php_char_range.select_head nondummy_left_lexings
          and v=Php_char_range.select_head nondummy_right_lexings in
          let cr=Php_char_range.make u v in
          Some(cr,support)
    else helper_for_chain(stepper_for_chain x);; 
  
let chain l_rcgzr=
  let tempf=(fun l->helper_for_chain
     (l_rcgzr,l,[],[],false)
  ) in
  (tempf:Php_recognizer.t);;  
  
let zzz_helper_for_ordered_disjunction l=
  let rec tempf=(fun lf->
     match lf with
      []->None
     |rcgzr::peurrest->(
                      match Php_recognizer.recognize rcgzr l with
                       None->tempf peurrest
                      |Some(cr,l2)->Some(cr,l2)
                    )
  ) in 
  tempf;;
  
let ordered_disjunction lf=
   let f=(
      fun l->zzz_helper_for_ordered_disjunction l lf
   ) in
   (f: Php_recognizer.t);;  
   
let star rcgzr=
   let rec tempf=(fun (u,v,l)->
   match Php_recognizer.recognize rcgzr l with
    None->Some(Php_char_range.make u v,l)
   |Some(cr2,l2)->tempf(u,Php_char_range.snd cr2,l2)
   ) in 
   let f=(fun l->
   match Php_recognizer.recognize rcgzr l with
    None->Some(Php_char_range.dummy,l)
   |Some(cr1,l2)->tempf (Php_char_range.fst cr1,Php_char_range.snd cr1,l2)
   ) in   
   (f: Php_recognizer.t);;  
   
let unfinished_star rcgzr=
   let rec tempf=(fun (u,v1,l2,v2,l3)->
   match Php_recognizer.recognize rcgzr l3 with
    None->Some(Php_char_range.make u v1,l2)
   |Some(cr3,l4)->tempf(u,v2,l3,Php_char_range.snd cr3,l4)
   ) in 
   let f=(fun l1->
   match Php_recognizer.recognize rcgzr l1 with
    None->None
   |Some(cr1,l2)->
        let (u1,v1)=Php_char_range.unveil cr1 in
        (
          match Php_recognizer.recognize rcgzr l2 with
           None->Some(Php_char_range.dummy,l1)
          |Some(cr2,l3)->
            let (_,v2)=Php_char_range.unveil cr2 in
            tempf(u1,v1,l2,v2,l3)
        )
   ) in   
   (f: Php_recognizer.t);;  
   
   
let plus rcgzr=chain [rcgzr;star rcgzr];;   


let optional rcgzr=
   let f=(fun l->
   match Php_recognizer.recognize rcgzr l with
    None->Some(Php_char_range.dummy,l)
   |Some(cr,l2)->Some(cr,l2)
   ) in   
   (f: Php_recognizer.t);;  

let generalize glz rcgzr=match glz with
    Generalizer.Zero_or_one->optional rcgzr
   |Generalizer.Zero_or_more->star rcgzr
   |Generalizer.One_or_more->plus rcgzr
   |Generalizer.One_or_more_with_right_end_removed->unfinished_star rcgzr;;

   
   
   