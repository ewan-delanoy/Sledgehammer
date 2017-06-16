(*

#use"Php_analizer/Beavers/beaver_for_statement.ml";;

*)

type plex=Positioned_php_token.t;;
type plexl=Positioned_php_token_list.t;;

type php_var=Positioned_php_token.t;;
type php_assign_op=Positioned_php_token.t;;
type php_class=Positioned_php_token.t;;
type php_static_method=Positioned_php_token.t;;
type php_class_property=Positioned_php_token.t;;
type php_fun_name=Positioned_php_token.t;;
type php_interface_name=Positioned_php_token.t;;
type php_trait_name=Positioned_php_token.t;;
type php_namespace=Positioned_php_token.t;;
type php_index=Positioned_php_token.t;;


type t=
    External_echo of string*Php_char_range.t
   |Comment of string*Php_char_range.t
   |Class_decl of Php_class_modifier.t*plexl*plexl*Php_char_range.t
   |Ivy of plexl*plexl*plexl*Php_char_range.t
   |Script_inclusion of Php_script_includer.t*plexl*Php_char_range.t
   |Assignment of php_var*php_assign_op*plexl*Php_char_range.t
   |Assignment_on_class_property of php_var*php_class_property*php_assign_op*plexl*Php_char_range.t
   |Static_assignment of php_var*php_assign_op*plexl*Php_char_range.t
   |Yuze_decl of plexl*Php_char_range.t
   |Static_method_call of php_class*php_static_method*plexl*Php_char_range.t
   |Nonstatic_method_call of plexl*php_static_method*plexl*Php_char_range.t
   |Class_property_call of plexl*php_class_property*Php_char_range.t
   |TryCatch of plexl*plexl*plexl*Php_char_range.t
   |Fun_def of php_fun_name*plexl*plexl*Php_char_range.t
   |Returning of plexl*Php_char_range.t
   |Interface_decl of php_interface_name*plexl*plexl*Php_char_range.t
   |Fun_call of bool*php_fun_name*plexl*Php_char_range.t
   |Namespace_def of php_namespace*Php_char_range.t
   |Appending of php_var*plexl*Php_char_range.t
   |Cell_assignment of php_var*php_index*plexl*Php_char_range.t
   |Trait_decl of php_trait_name*plexl*Php_char_range.t
   |Echo of plexl*Php_char_range.t
   |Exit of Php_char_range.t
   |WhileLoop of plexl*plexl*Php_char_range.t
   |ForeachLoop of plexl*plexl*Php_char_range.t
   |NamespaceBlock of (plex option)*plexl*Php_char_range.t
   |Declare of plexl*Php_char_range.t
   |Switch of plex*plexl*Php_char_range.t;;
   
let char_range=function
    External_echo(_,cr)->cr
   |Comment(_,cr)->cr
   |Class_decl(_,_,_,cr)->cr
   |Ivy(_,_,_,cr)->cr 
   |Script_inclusion(_,_,cr)->cr 
   |Assignment(_,_,_,cr)->cr
   |Assignment_on_class_property(_,_,_,_,cr)->cr
   |Static_assignment(_,_,_,cr)->cr
   |Yuze_decl(_,cr)->cr
   |Static_method_call(_,_,_,cr)->cr 
   |Nonstatic_method_call(_,_,_,cr)->cr 
   |Class_property_call(_,_,cr)->cr
   |TryCatch(_,_,_,cr)->cr
   |Fun_def(_,_,_,cr)->cr
   |Returning(_,cr)->cr
   |Interface_decl(_,_,_,cr)->cr
   |Fun_call(_,_,_,cr)->cr
   |Namespace_def(_,cr)->cr
   |Appending(_,_,cr)->cr
   |Cell_assignment(_,_,_,cr)->cr
   |Trait_decl(_,_,cr)->cr
   |Echo(_,cr)->cr
   |Exit(cr)->cr
   |WhileLoop(_,_,cr)->cr
   |ForeachLoop(_,_,cr)->cr
   |NamespaceBlock(_,_,cr)->cr
   |Declare(_,cr)->cr
   |Switch(_,_,cr)->cr;;
     
let dummy=External_echo("",Php_char_range.dummy);;     

type element={
    name               : string;
    content            : string;
    unadbriged_content : string;
    catalyser          : string;
    helper             : (plexl list -> Php_char_range.t -> t);
};;   
   
let element_cmp elt1 elt2=
   let step1=Dictionary_order.dictionary_order elt1.name elt2.name in
   if step1<>Total_ordering.Equal
   then step1
   else Tidel.cmp
         (elt1.content,elt1.unadbriged_content,elt1.catalyser)     
         (elt2.content,elt2.unadbriged_content,elt2.catalyser);;
          
let element_order=(element_cmp: element Total_ordering.t);; 

let classical_parser elt=
   let f=(fun l->
      let opt2=Termite.parse (Termite.of_string elt.unadbriged_content) l in
      if opt2=None then None else
      let (l2,cr2,peurrest)=Option.unpack opt2 in
      let catalyser_check=(
        if elt.catalyser=""
        then true
        else (Termite.parse (Termite.of_string elt.catalyser) peurrest)<>None
      ) in
      if catalyser_check
      then None
      else Some(elt.helper l2 cr2,cr2,peurrest)
   ) in
   (f : t Php_parser.t);;
   
let current_data_list=ref ([]:element list);;
let shortcuts_list=ref ([]:(string*string) list);;

let expand_element elt={
    name               = elt.name;
    content            = elt.content;
    unadbriged_content = Replace_inside.replace_several_inside_string
      						(!shortcuts_list) elt.content;
    catalyser          = elt.catalyser;
    helper             = elt.helper;
};;   

let add_data a b c d=
   let old_version=Ordered.safe_set element_order (!current_data_list) in
   let elt={
    name               = a;
    content            = b;
    unadbriged_content = Replace_inside.replace_several_inside_string
      						(!shortcuts_list) b;
    catalyser          = c;
    helper             = d;
   }  in
   let new_set=Ordered.insert element_order elt old_version in
   let new_version=Ordered.forget_order new_set in 
   (current_data_list:=new_version);;
    
let update_data_list ()=
   let new_list=Image.image expand_element (!current_data_list) in
   current_data_list:=new_list;;    
    
let add_shortcut x y=
   (
     shortcuts_list:=(x,y)::(!shortcuts_list);
     update_data_list ()
   );;    
    
let current_main_parser=
  (
    fun l->Option.find_and_stop 
    (fun tr->classical_parser tr l) (!current_data_list)
  );;

let display_data ()=
   let m1=snd(Max.maximize_it(fun elt->String.length(elt.name) 
   ) (!current_data_list)) in  
   let temp1=Image.image (fun 
      elt->
         let d=m1-(String.length(elt.name)) in
         let filler=String.make d ' ' in
         "\""^elt.name^"\""^filler^"   ,   \""^elt.content^"\""
   )(!current_data_list) in
   let temp2="\n\n\n"^(String.concat "\n" temp1)^"\n\n\n" in
   print_string temp2;;
   
   

let helper_for_byref_append l1 cr=
   let a=Positioned_php_token_list.hd(List.hd l1) in
   Appending(a,List.nth l1 1,cr);;

add_data 
	"append_byref"
	"variable [ ] = ##( & _l_no_semicolon _r*_ )## ;"
	""
	helper_for_byref_append
	;;
   
let helper_for_assignment l1 cr=
  let a=Positioned_php_token_list.hd(List.nth l1 0) 
  and b=Positioned_php_token_list.hd(List.nth l1 1)  in
  Assignment(a,b,List.nth l1 2,cr);;   

add_data 
	"assign1"
	"variable assign ##( () ?  _l_ id _u_ sqs _rd_  :  _l_ id _u_ variable _rd_  )## ;"
	""
	helper_for_assignment
	;;

add_data 
	"assign2"
	"variable assign  ##( sqs . id . dqs . variable -> id () . dqs . variable -> id () . dqs )## ;"
	""
	helper_for_assignment
	;;  

add_data 
	"assign_byref"
	"variable assign ##( & _l_loose= _r*_ )## ;"
	""
	helper_for_assignment;;
	
let helper_for_servant_assign l1 cr=
   let tf=(fun j->Positioned_php_token_list.hd(List.nth l1 j)) in
   Assignment_on_class_property(tf 0,tf 1,tf 2,List.nth l1 3,cr);;

add_data 
	"assign_on_servant"
	"variable -> id_or_var ##( assign )## _l_ loose= _r*_ ;"
	""
	helper_for_servant_assign;;

add_data 
	"assign_on_static"
	"id :: id_or_var ##( assign )## _l_ loose= _r*_ ;"
	""
	helper_for_servant_assign
	;;

add_data 
	"assign_to_cell"
	"variable assign ##( variable [ sqs ] )## ;"
	""
	helper_for_assignment
	;;
	
	

let assignables=
[
  "coerce           id ()";
  "nmspc            _l_ :: id _r?_ _l_ () _r?_";
  "id ::            id ()";
  "id () ?          _l_ no_ternary _r+_ : no_semicolon";
  "id () .          sqs";
  "id ()            ";
  "include_like     _l_ loose= _r*_ ";
  "integer          ";
  "new id           ()";
  "new nmspc        ()";
  "sqs .            variable . sqs";
  "sqs";
  "variable .       sqs";
  "variable =       sqs";
  "variable ->      id _l_ () _r?_ _l_ -> id _l_ () _r?_ _r*_";
  "variable +       _l_ loose= _r*_ ";
  "variable";
  "@                id ()";
];;


let assignable=" _l_ "^(String.concat " _u_ " assignables)^" _rd_";;


add_shortcut "assignable" assignable;;


add_data 
  "assign_to_simple"
  "variable ##( assign )## assignable ;"
  ""
  helper_for_assignment
  ;;

add_data 
	"assign_to_terna"
	"variable ##( assign )## ##( () ? : new nmspc () )## ;"
	""
	helper_for_assignment
	;;

let helper_for_cell_assign l1 cr=
   let a=Positioned_php_token_list.hd(List.hd l1) in
   Cell_assignment(a,Positioned_php_token_list.hd(List.nth l1 1),List.nth l1 2,cr);;


add_data 
	"cell_assign"
	"variable [ ##( int_or_string_or_var )## ] = _l_loose= _r*_ ;"
	""
	helper_for_cell_assign
	;;

add_data 
	"cell_assign_byref"
	"variable [  ##( int_or_string_or_var )##  ]  =  ##( & _l_loose= _r*_ )## ;"
	""
	helper_for_cell_assign
	;;


let helper_for_abstract_class l1 cr=
   Class_decl(Php_class_modifier.Abstract,List.nth l1 0,List.nth l1 1,cr);;
   
add_data
  "class_abstract"
  "abstract class _l_ no_left_brace _r*_ {}"
  ""
  helper_for_abstract_class
  ;;   
  
let helper_for_final_class l1 cr=
   Class_decl(Php_class_modifier.Final,List.nth l1 0,List.nth l1 1,cr);;
   
add_data 
   "class_final"
   "final class _l_ no_left_brace _r*_ {}"
   ""
   helper_for_final_class;;   
  
let helper_for_usual_class l1 cr=
   Class_decl(Php_class_modifier.Usual,List.nth l1 0,List.nth l1 1,cr);;
   
add_data
  "class_usual"
  "class _l_ no_left_brace _r*_ {}"
  ""
  helper_for_usual_class
  ;;   

let helper_for_decl l1 cr=Declare(List.hd l1,cr);;

add_data 
	"decl"
	"declare () ;"
	""
	helper_for_decl
	;;


let helper_for_echo l1 cr=Echo(List.hd l1,cr);;


add_data 
	"echo1"
	"echo _l_ no_semicolon _r*_ ;"
	""
	helper_for_echo
	;;

add_data 
	"echo2"
	"echo variable"
	"ext"
	helper_for_echo
	;;
	
let helper_for_exit l1 cr=Exit(cr);;

add_data 
	"exit"
	"exit ;"
	""
	helper_for_exit
	;;

let helper_for_foreach1 l1 cr=ForeachLoop(List.nth l1 0,List.nth l1 1,cr);;

add_data 
	"foreach1"
	"foreach () {}"
	""
	helper_for_foreach1
	;;





let helper_for_foreach2 l1 cr=ForeachLoop(List.nth l1 0,List.nth l1 1,cr);;

add_data 
	"foreach2"
	"foreach () : ##( _l_no_breach _r*_ )## endforeach ;"
	""
	helper_for_foreach2
	;;

let helper_for_fun_call l1 cr=Fun_call(
not(Positioned_php_token_list.is_empty(List.hd(l1))),
Positioned_php_token_list.hd(List.nth l1 1),List.nth l1 2,cr);;

add_data 
	"fun_call"
	"_l_ @ _r?_ ##(id)## () ;"
	""
	helper_for_fun_call
	;;

let helper_for_fun_def l1 cr=
   let a=Positioned_php_token_list.hd(List.hd l1) in
   Fun_def(a,List.nth l1 1,List.nth l1 2,cr);;

add_data 
	"fun_def"
	"function id () {}"
	""
	helper_for_fun_def
	;;



let helper_for_fun_returning l1 cr=Returning(List.hd(l1),cr);;

add_data 
	"fun_returning"
	"return ##( function () {} )## ;"
	""
	helper_for_fun_returning
	;;


let helper_for_include_like l1 cr=
      let a=Positioned_php_token_list.hd(List.hd l1) in
      let fa=Positioned_php_token.fst a in
      let na=Php_script_includer.from_lexeme fa in
      Script_inclusion(na,List.nth l1 1,cr);; 

add_data 
   "include_like"
   "include_like _l_stringy _r*_ ;"
   ""
   helper_for_include_like
   ;; 

let helper_for_interface_decl l1 cr=
     let temp1=List.hd l1 in
     Interface_decl(Positioned_php_token_list.hd temp1,
       Positioned_php_token_list.tl temp1,List.nth l1 1,cr);;

add_data 
	"interface_decl"
	"interface _l_ no_left_brace _r*_ {}"
	""
	helper_for_interface_decl
	;;



let helper_for_ivy l1 cr=
  Ivy(List.nth l1 0,List.nth l1 1,List.nth l1 2,cr);;

add_data 
   "ivy1"
   "if () ##( {} )## ##( _l_else if () {} _r*__l_else {} _r?_ )##"
   ""
   helper_for_ivy
   ;;
     

let helper_for_ivy2 l1 cr=Ivy(List.nth l1 0,List.nth l1 1,Positioned_php_token_list.empty,cr);; 

add_data 
  "ivy2"
  "if () ##( exit ; )##"
  ""
  helper_for_ivy2
  ;;    

add_data 
  "ivy3"
  "if () ##( {} )## else ##(if () {})## "
  ""
  helper_for_ivy
  ;;    


add_data 
   "ivy4"
   "if () : ##( _l_no_ivies _r*_ )## endif ;"
   ""
   helper_for_ivy2
   ;; 

add_data 
  "ivy5"
  "if () : ##( _l_no_ivies _r*_ if () : _l_no_ivies _r*_ endif _l_no_ivies _r*_ )## endif ;"
  ""
  helper_for_ivy2
  ;; 
  


add_data 
  "ivy6"
  "if () : ##( _l_no_ivies _r*_ if () : _l_no_ivies _r*_ else : _l_no_ivies _r*_ endif ; _l_no_ivies _r*_ )## endif ;"
  ""
  helper_for_ivy2
  ;; 
  
let helper_for_meth_call_on_snake l1 cr=
  Nonstatic_method_call(List.nth l1 0,Positioned_php_token_list.hd(List.nth l1 1),List.nth l1 2,cr);;


 add_data 
	"meth_call_on_snake"
	"##( id :: id () _l_-> id () _r~_ )## -> id () ;"
	""
	helper_for_meth_call_on_snake
	;;

 
let helper_for_nmspc_block l1 cr=
   let optionized=(fun x->if Positioned_php_token_list.is_empty x
    					  then None 
    					  else Some(Positioned_php_token_list.hd x) )(List.hd l1) in
   NamespaceBlock(optionized,List.nth l1 1,cr);;

add_data 
	"nmspc_block"
	"namespace _l_ _l_ id_u_nmspc _rd_ _r?_ {}"
	""
	helper_for_nmspc_block
	;;


let helper_for_nmspc_definition l1 cr=
   let a=Positioned_php_token_list.hd(List.hd l1) in
   Namespace_def(a,cr);;

add_data 
	"nmspc_definition"
	"namespace nmspc ;"
	""
	helper_for_nmspc_definition
	;;



let helper_for_nmspc_lonely l1 cr=
   let a=Positioned_php_token_list.hd(List.hd l1) in
   Namespace_def(a,cr);;

add_data 
	"nmspc_lonely"
	"namespace id ;"
	""
	helper_for_nmspc_lonely
	;;

let helper_for_returning l1 cr=Returning(List.hd(l1),cr);;

add_data 
	"returning"
	"return _l_ no_semicolon _r*_ ;"
	""
	helper_for_returning
	;;






	
exception Singleton_exn;; 
   
let helper_for_singleton l1 cr=
     match Positioned_php_token.fst(Positioned_php_token_list.hd(List.hd l1)) with
       Php_token.External_echo(s)->External_echo(s,cr)
      |_->raise(Singleton_exn);;
   
add_data 
  "singleton"
  "ext"
  ""
  helper_for_singleton
  ;;   


let helper_for_snake_call l1 cr=
  let temp1=Positioned_php_token_list.concat (List.hd l1) (List.nth l1 1) 
  and last_leader=Positioned_php_token_list.hd(List.nth l1 2) in
  if Positioned_php_token_list.is_empty (List.nth l1 3)
  then Class_property_call(temp1,last_leader,cr)
  else Nonstatic_method_call(temp1,last_leader,List.nth l1 3,cr);;

add_data 
	"snake_call"
	"id_or_var _l_ -> id_or_var _l_() _r?_  _r~_ -> id_or_var _l_() _r?_ ;"
	""
	helper_for_snake_call
	;;




let helper_for_static_assignment l1 cr=
  let a=Positioned_php_token_list.hd(List.nth l1 0) 
  and b=Positioned_php_token_list.hd(List.nth l1 1)  in
  Static_assignment(a,b,List.nth l1 2,cr);; 

add_data 
  "static_assignment"
  "static variable assign ##( id () )## ;"
  ""
  helper_for_static_assignment
  ;;



let helper_for_static_meth l1 cr=
   let a1=Positioned_php_token_list.hd(List.hd l1) 
   and a2=Positioned_php_token_list.hd(List.nth l1 1) in
   Static_method_call(a1,a2,List.nth l1 2,cr);;

add_data 
	"static_meth"
	"id :: id_or_var () ;"
	""
	helper_for_static_meth
	;;



let helper_for_statmeth_call l1 cr=
  Static_method_call(Positioned_php_token_list.hd(List.nth l1 0),
  Positioned_php_token_list.hd(List.nth l1 1),List.nth l1 2,cr);;

add_data 
	"statmeth_call"
	"_l_ id_u_nmspc _rd_ :: ##(id)## () ;"
	""
	helper_for_statmeth_call
	;;


let helper_for_switch l1 cr=Switch(Positioned_php_token_list.hd(List.hd l1),List.nth l1 1,cr);;

add_data 
	"switch"
	"switch () {}"
	""
	helper_for_switch
	;;

let helper_for_trait_decl l1 cr=
  let temp1=List.hd l1 in
  Trait_decl(Positioned_php_token_list.hd temp1,List.nth l1 1,cr);;

add_data 
	"trait_decl"
	"trait id {}"
	""
	helper_for_trait_decl
	;;



let helper_for_trycatch l1 cr=TryCatch(List.nth l1 0,List.nth l1 1,List.nth l1 2,cr);;

add_data 
	"trycatch"
	"try {} catch () {}"
	""
	helper_for_trycatch
	;;



let helper_for_while_loop l1 cr=WhileLoop(List.nth l1 0,List.nth l1 1,cr);;

add_data 
	"while_loop"
	"while () {}"
	""
	helper_for_while_loop
	;;


let helper_for_yuze_decl l1 cr=Yuze_decl(List.hd l1,cr);;

add_data 
	"yuze_decl"
	"use _l_no_semicolon _r*_ ;"
	""
	helper_for_yuze_decl
	;;



let parser=(current_main_parser:t Php_parser.t);; 


 
 



