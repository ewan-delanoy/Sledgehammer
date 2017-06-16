(*

#use"Php_analizer/php_constructible_recognizer.ml";;

Generic parser for php code.


*)

type t=
   Leaf of Php_short_selector.t
  |Generalized of Generalizer.t*t
  |Chain of t list
  |Disjunction of t list;;
  
let pair_for_disjunction=("_l_","_rd_");;
let associator_for_disjunction="_u_";;  
  
let rec to_string=function
   Leaf(sel)->Php_short_selector.to_string sel
  |Generalized(grlz,x)->
                   let (lpar,rpar)=Generalizer.pair grlz in
                   lpar^(to_string x)^rpar
  |Chain(l)->String.concat " " (Image.image to_string l)
  |Disjunction(l)->
                   let (lpar,rpar)=pair_for_disjunction in
                   lpar^
                   (String.concat associator_for_disjunction 
                     (Image.image to_string l))
                   ^rpar;;



let all_pairs=pair_for_disjunction::Generalizer.all_pairs;;

exception Helper_for_string_reading_exn of ((string*string) option)*string;;

let helper_for_string_reading old_f (opt,t)=
       if opt=None then old_f t else
       let pair=Option.unpack opt in
       let opt2=Option.find_it
         (fun x->(Generalizer.pair x)=pair)
         Generalizer.all in
       if opt2<>None then Generalized(Option.unpack opt2,old_f t) else
       if pair=pair_for_disjunction
       then 
            let temp1=Parenthesed_block.decompose_with_associator
                  associator_for_disjunction all_pairs t in
            Disjunction(Image.image old_f temp1)
       else
       raise(Helper_for_string_reading_exn(opt,t));; 



exception Empty_output;;

let rec of_string rough_s=
  let s=Cull_string.trim_spaces rough_s in
  if s="" then raise(Empty_output) else
  let temp1=Parenthesed_block.decompose_without_taking_blanks_into_account all_pairs s in
  let temp2=Image.image (fun (opt,t)->(opt,Cull_string.trim_spaces t) ) temp1 in
  let temp3=List.filter (fun (opt,t)->t<>"") temp2 in
  if List.length(temp3)>1
  then Chain(Image.image (helper_for_string_reading of_string) temp3)
  else 
  let (opt,t)=List.hd temp3 in
  if opt<>None
  then helper_for_string_reading of_string (opt,t)
  else
  let temp5=Php_short_selector.list_from_string t in
  let temp4=Image.image (fun sel->Leaf(sel)) temp5 in
  if List.length(temp4)=1
  then List.hd(temp4)
  else Chain(temp4);;

let chain_content wh=
  match wh  with
   Chain(ch)->Some(ch)
  |_->None;;

let is_constant wh=
  match wh  with
   Leaf(sel)->Php_short_selector.is_constant sel
  |_->false;;



let recognize_selector=Php_short_selector.recognize;;

let recognize_generalized old_f grlz x=Php_recognizer_homomorphism.generalize grlz (old_f x);;

let recognize_chain old_f ch=Php_recognizer_homomorphism.chain (Image.image old_f ch);;

let recognize_disjunction old_f l=Php_recognizer_homomorphism.ordered_disjunction (Image.image old_f l);;


let rec recognize wh l=
  match wh  with
   Leaf(sel)->recognize_selector sel l
  |Generalized(grlz,x)->recognize_generalized recognize grlz x l
  |Chain(ch)->recognize_chain recognize ch l
  |Disjunction(dis)->recognize_disjunction recognize dis l;;


exception Reverse_sleepy_parse_exn of string;;

let reverse_sleepy_parse wh l=
  match wh  with
   Leaf(sel)->(Leaf(sel),l)
  |Generalized(grlz,x)->raise(Reverse_sleepy_parse_exn("generalized"))
  |Chain(ch)->raise(Reverse_sleepy_parse_exn("chain"))
  |Disjunction(dis)->raise(Reverse_sleepy_parse_exn("dis"));;





