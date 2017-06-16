
  (*here we implement the simplex method for the optimization problem max( <c,x> | Ax+s=b,x>=0,s>=0 ) where the coordinates
  of the vector b are nonnegative. We do not implement the algorithm that reduces the "arbitrary b" case to the
  "b>=0" case. *)	   
	   
   type t=YY of 
   Vector.t*(Simplex_relation.t list)*(Variable.t list)*
   (Descriptive_system.t)*(Descriptive_system.t)*Vector.t;;
   
  
   (*make an elementary transformation on the system, using variable var as pivot
	  and the ith equation in l. *)
	  
 let unveil (YY(p,l,visual,descr,old_descr,old_p))=(p,l,visual,descr,old_descr,old_p);;  	  
	  
 let main_vector (YY(p,l,visual,descr,old_descr,old_p))=p;;
 
 let constraints (YY(p,l,visual,descr,old_descr,old_p))=l;;
    
 let visible_variables (YY(p,l,visual,descr,old_descr,old_p))=visual;;   
 
 let descriptive_system (YY(p,l,visual,descr,old_descr,old_p))=descr;;
 
 let initial_descriptive_system (YY(p,l,visual,descr,old_descr,old_p))=old_descr;;
    
    let cleanup (YY(p,l,visual,descr,old_descr,old_p))=
   let cleaned_l=List.filter(function sr->not(Simplex_relation.is_superfluous sr) )(l) in
   YY(p,cleaned_l,visual,descr,old_descr,old_p);;  
   
   type tt=TR of ((int*Variable.t) list)*t;;
 
 let switch var i (YY(p,l,visual,descr,old_descr,old_p))=
	let eq1=Simplex_relation.to_relation(List.nth(l)(i-1)) in
	let definition_for_var=Relation.isolate(eq1)(var) in
	let indexed_l=Ennig.index_everything(l)
	and tempf=(function (j,r)->
	  if j=i 
	  then Simplex_relation.change_adjusters(r)(var)
	  else Simplex_relation.subs(r)(var)(definition_for_var) ) in
	let new_l=Image.image(tempf)(indexed_l)
	and new_p=Vector.subs(p)(var)(definition_for_var) in
	YY(new_p,new_l,visual,descr,old_descr,old_p);;
	
 let switch_and_remember var i (TR(accu,syst))=TR((i,var)::accu,switch var i syst);;	
	
  let uncurrified_switch syst (i,var)=switch var i syst;;	
  
  let uncurrified_switch_and_remember tr (i,var)=
     switch_and_remember var i tr;;
  
  let fold_switch syst mlist=List.fold_left uncurrified_switch syst mlist;;
	
let deduce_standard_preserving_lines_from_pivot (YY(p,l,visual,descr,old_descr,old_p)) var=
  let indexed_l=Ennig.index_everything(l) in
  let almost_willie=Image.image(function (j,sr)->
    let vec=Simplex_relation.vector_part(sr) in
        let rat=Vector.coeff(vec)(var) in
        (j,(sr,rat))
  ) indexed_l in
  let willie=List.filter(function (j,(sr,rat))->
        Rational.is_positive rat
  ) almost_willie in
 let part_willie=List.partition(function (j,(sr,rat))->
     Simplex_relation.is_critical sr
 ) willie in
 let critical_willie=fst(part_willie)
 and noncritical_willie=snd(part_willie) in
 let always_there=Image.image fst critical_willie in
 if List.exists(function (j,(sr,rat))->Rational.is_positive rat)(critical_willie)
 then  always_there
 else  let temp1=Image.image(function (j,(sr,rat))->
              (j,Simplex_relation.ratio sr var) )(noncritical_willie) in
       let rat_max=snd(Rational.maximize snd temp1) in 
       let temp2=List.filter (function (j,rat)->Rational.eq rat rat_max) temp1 in
       let temp3=Tidel.safe_set(Image.image fst temp2) in
       let temp4=Tidel.teuzin(temp3)(Tidel.safe_set always_there ) in
       Tidel.forget_order temp4;;
 
 let basic_variables (YY(p,l,visual,descr,old_descr,old_p))=
  let tempf=(function sr->Vector.support(Simplex_relation.vector_part(sr)))
  in
     let temp1=List.rev_map tempf l in
     Ordered_variable.big_teuzin temp1;;
     
 
  let standard_preserving_pairs syst=
     let bas_var=basic_variables(syst) 
     and tempg=(function pivot->
        let lines=deduce_standard_preserving_lines_from_pivot(syst)(pivot) in
        Image.image (function line->(line,pivot)) lines
     )    in
     let temp1=Ordered_variable.image tempg bas_var in
     List.flatten temp1;;
     
 
 (*
       There are two ways for a standard-preserving pair to be progressive : either
   the coeff of the pivot in p is positive so that p will be increased by the change, 
   or the coeff is 0 and the number of positive coefficients decreases in p. 
  *)
 
  let is_strongly_progressive (YY(p,l,visual,descr,old_descr,old_p)) (line,pivot)=
   Rational.is_positive(Vector.coeff(p)(pivot));;
 
  let is_progressive (YY(p,l,visual,descr,old_descr,old_p)) (line,pivot)=
    let r=Vector.coeff(p)(pivot) in
    if Rational.is_positive(r) then true else
    if not(Rational.eq(r)(Rational.zero)) then false else
    let eq1=Simplex_relation.to_relation(List.nth(l)(line-1)) in
	let definition_for_pivot=Relation.isolate(eq1)(pivot) in
    let new_p=Vector.subs(p)(pivot)(definition_for_pivot) in
    Vector.number_of_positive_coefficients(new_p)<
    Vector.number_of_positive_coefficients(p);;
 
  let progressive_pairs  syst=
      List.filter (is_progressive syst) (standard_preserving_pairs syst);;
 
  let find_best_weakly_progressive_pair (YY(p,l,visual,descr,old_descr,old_p)) candidates=
    let tempf=(function 
     (line,pivot)->
     let eq1=Simplex_relation.to_relation(List.nth(l)(line-1)) in
	let definition_for_pivot=Relation.isolate(eq1)(pivot) in
    let new_p=Vector.subs(p)(pivot)(definition_for_pivot) in
    Vector.number_of_positive_coefficients(p)-
    Vector.number_of_positive_coefficients(new_p)
    ) in
    fst(Min.minimize_it tempf candidates);;
 
  let find_best_strongly_progressive_pair (YY(p,l,visual,descr,old_descr,old_p)) candidates=
	 let tempf=(function
	  (line,pivot)->
	  let sr=List.nth(l)(line-1) in
	  let c0=Vector.coeff(p)(Variable.dummy)
	  and c1=Vector.coeff(p)(pivot)
	  and c2=Simplex_relation.constant_part(sr) in
	  let c3=Rational.mult(c1)(c2) in
	  let c4=Rational.add(c0)(c3) in
	  ((line,pivot),c4)
	 ) in
	 let temp1=List.rev_map tempf candidates in
	 fst(fst(Rational.maximize snd temp1));;
	 
   let find_best_progressive_pair syst=
     let candidates=progressive_pairs(syst) in
     if candidates=[] then None else
     let strong_candidates=List.filter(is_strongly_progressive syst)(candidates) in
     if strong_candidates=[] 
     then Some(find_best_weakly_progressive_pair syst candidates)
     else Some(find_best_strongly_progressive_pair syst strong_candidates);;
     
   
     
   let has_correct_pair syst=((find_best_progressive_pair syst)<>None);;  
     
   let number_of_positive_coefficients (YY(p,l,visual,descr,old_descr,old_p))=
     Vector.number_of_positive_coefficients p;;
  
     
   let first_chain syst=
     let rec tempf=(function
       (accu,yy)->match find_best_progressive_pair(yy) with
          None->List.rev(accu)
          |Some(line,pivot)->tempf((line,pivot)::accu,switch pivot line yy)
     ) in
     tempf([],syst);;
     
     
   let first_breakthrough syst=
   cleanup(fold_switch syst (first_chain syst));;
   
   let double_presentation bindings=
    let list_for_s1=Image.image
     (function (var,value)->(Variable.latex_alpha var)^"="^(Rational.latex_name value))(bindings) in
    let s1=String.concat(",")(list_for_s1) in
    let temp1=Ordered.diforchan Rational.cmp (List.rev_map snd bindings) in
    let temp2=Ordered.rev_map(function y0->
        let ttemp1=List.filter(function (x,y)->Rational.eq y y0)(bindings) in
        (y0,Image.image fst ttemp1)
      )(temp1) in
    let unregulated_ordering=(fun (y0,ly0) (y1,ly1)->
      let d0=List.length(ly0) and d1=List.length(ly1) in
      if d0=d1 then Rational.compare(y0)(y1) else d0<d1
    ) 
    and eq_test=(fun (y0,ly0) (y1,ly1)->(Rational.eq(y0)(y1))&&(ly0=ly1) ) in
    let regulated_ordering=Total_ordering.standard_completion
      (unregulated_ordering)(eq_test) in
    let temp3=Ordered.diforchan(regulated_ordering)(temp2) in
    let descriptor=(function (y,ly)->
      let ttemp1=Image.image(Variable.latex_alpha)(ly) in
      let ttemp2=String.concat("=")(ttemp1) in
      ttemp2^"="^(Rational.latex_name y)
    ) in
    let list_for_s2=Ordered.image descriptor temp3 in
    let s2=String.concat(",")(list_for_s2) in
    let list_for_s3=Image.image
     (function (var,value)->
        let s=Variable.latex_alpha var in
        s^"="^(Rational.latex_name value))(bindings) in
    let s3=String.concat(",")(list_for_s3) in 
    (s3,s1,s2);;
    
   
   let optimal_binding syst=
    (* we assume that syst is an already solved sytem*)
   match syst with YY(p1,l1,visual1,descr1,old_descr,old_p1)->
    if number_of_positive_coefficients(syst)>0
    then failwith("There was a mistake somewhere.")
    else let adjusters_set_to_max=List.rev_map(function sr->
            (Simplex_relation.adjuster_part sr,
            Simplex_relation.constant_part sr)  )(l1) 
         and others_set_to_0=List.flatten(List.rev_map(function sr->
           let vec=Simplex_relation.vector_part sr in
           Ordered_variable.rev_map (function x->(x,Rational.zero)) (Vector.support vec) )(l1)) in
         let complete_set=(adjusters_set_to_max@others_set_to_0) in
         let final_descr=Descriptive_system.insert_several_simple_equalities
           (complete_set)(descr1) in  
         let tempf=(fun x->
           List.hd(Descriptive_system.eval final_descr [x])
         )  in
         let all_vars=Ordered_variable.diforchan(visual1) in
         Ordered_variable.image (fun x->(x,tempf x)) all_vars;;
     
let solve syst=
    let syst2=first_breakthrough(syst) in
    if number_of_positive_coefficients(syst2)>0
    then failwith("Sorry, IÕm stuck. Call first_chain to see how far I got.")
    else match syst2 with
         YY(p,l,visual,descr,old_descr,old_p)->
         let big_m=Vector.coeff(p)(Variable.dummy) in
         let before_q=Vector.unveil(Vector.sub(Vector.of_rational big_m)(p)) 
         and tempf=(fun (lamb,var)->
          if Variable.is_a_toggler(var)
          then (lamb,Variable.toggle var)
          else (Rational.opposite lamb,var) ) in
         let q=Vector.safe_vector(Image.image tempf before_q) in
         let new_version_of_q=(Descriptive_system.transform_vector q old_descr) in
         let difference=(Vector.sub new_version_of_q old_p) in
         let g=Vector.gauss_multiplier(q) in
         let bindings=optimal_binding(syst2) in
         let other_q=Vector.dot g q in
         let more_bindings=Descriptive_system.insert_several_simple_equalities
             bindings old_descr in
         let tempf1=(fun (lamb,var)->
           (lamb,var,List.hd(Descriptive_system.eval more_bindings [var]))
          ) in
         let other_bindings=Image.image tempf1 (Vector.unveil(other_q)) in
         (big_m,other_q,g,Vector.dot g difference,bindings,other_bindings);;
         
       

 let deduce_latex_solution 
   (big_m,other_q,g,restachou,bindings,other_bindings)=
   let v1=Vector.lonely(g)(Variable.of_string "S") in
   let v2=Vector.add(v1)(restachou) in
   let temp1=Image.image(fun (lamb,anv,v)->
     if Rational.eq(lamb)(Rational.one)
     then Rational.print(v)
     else "("^(Rational.print lamb)^"\\times"^(Rational.print v)^")"
   )(other_bindings) in
   let temp2=String.concat("+")(temp1) in
   let funk=Image.image(fun (var,rat)->
      let veg1=Vector.of_variable var
      and veg2=Vector.of_rational rat in
      Relation.of_vector (Vector.sub veg1 veg2)
   )(bindings) in
   let descr=Descriptive_system.make(funk) in
   let temp3=Descriptive_system.transform_vector(restachou)(descr) in
   let temp4=Rational.print(Vector.coeff(temp3)(Variable.dummy)) in
   let temp5=Image.image(fun (lamb,anv,v)->
     Rational.mult lamb v
   )(other_bindings) in
   let temp6=Rational.print(Rational.big_sum temp5) in
   let temp7=Rational.print(Rational.floor big_m) in
   let temp8=(Vector.print v2)^"="^(Vector.print other_q) in
   let temp9=(fun s->
      if String.sub(s)(0)(2)="+-" 
      then String.sub(s)(1)(String.length(s)-1)
      else s
   )("+"^temp4) in
   let s1="Diwar ar parder "
   and s234=Latex_pretty_printing.displaymath(temp8)
   and s5="e tastumomp $"^(Vector.print v2)^"\\leq "^temp2^"$"
   and s6=" ac\'hann $"^(Vector.print v1)^temp9^"\\leq "^temp6^"$" 
   and s7=" ha $S \\leq "^temp7^"$. " in
   let s=String.concat "\n" [s1;s234;s5^s6^s7] in
   let (a,b,c)=double_presentation bindings in
   (s, a,b,c);;

 
            
 let latex_solution syst=deduce_latex_solution (solve syst);;
   


 let impose_simple_equality_naively var v0 (YY(p0,l0,visual0,descr0,old_descr,old_p0))=
  let vv0=Vector.of_rational (v0)  in
  let p_nevez=Vector.subs(p0)(var)(vv0) 
  and tempf=(function sr->
    let vec=Simplex_relation.vector_part(sr) in
    if Vector.support(vec)=Ordered_variable.singleton(var)
    then None
    else Some(Simplex_relation.subs sr var vv0)
  ) in
  let uncleaned_l_nevez=Option.filter_and_unpack(tempf)(l0) in
  let l_nevez=List.filter
  (function sr->not(Simplex_relation.is_superfluous sr) )(uncleaned_l_nevez) 
  and descr_nevez=Descriptive_system.insert_simple_equality(var)(v0)(descr0) in
  YY(p_nevez,l_nevez,visual0,descr_nevez,old_descr,old_p0);;
   
  
  let impose_several_simple_equalities_naively l_eq yy0=
   List.fold_left(fun yy (var,valeu)->impose_simple_equality_naively var valeu yy) yy0 l_eq;;
   
  let rec impose_several_simple_equalities l_eq yy0=
    let syst=impose_several_simple_equalities_naively l_eq yy0 in
    let (YY(p0,l0,visual0,descr0,old_descr,old_p0))=syst in
    match Option.find_it(Simplex_relation.is_a_gift)(l0) with
     None->syst
    |Some(sr)->
       let supp=Vector.support(Simplex_relation.vector_part(sr)) in
       let new_equalities=Ordered_variable.image(fun x->(x,Rational.zero))(supp) in
       impose_several_simple_equalities new_equalities syst;;
   
 let impose_simple_equality var v0 syst=impose_several_simple_equalities [var,v0] syst;;
   
  let impose_extremal_simple_equalities l0 l1=
   let tempf=(fun z i->let rat=Rational.of_int(i) in List.rev_map (function x->(x,rat)) z) in
   let l=(tempf l0 0)@(tempf l1 1) in
   impose_several_simple_equalities l;;
  
  let uncurrified_impose_simple_equality yy (var,v0)=impose_simple_equality var v0 yy;;
   
 let impose_equalities equalities (YY(p0,l0,visual0,descr0,old_descr,old_p0))=
  let p_nevez=Vector.fold_subs(equalities)(p0)
  and tempf=(function sr->Simplex_relation.fold_subs sr equalities) in
  let uncleaned_l_nevez=Image.image(tempf)(l0) in
  let l_nevez=List.filter
  (function sr->not(Simplex_relation.is_superfluous sr) )(uncleaned_l_nevez) in
  let relations=List.rev_map(function (var,vec)->
        Relation.of_vector(Vector.sub(Vector.of_variable var)(vec))
     ) equalities in
  let descr_nevez=Descriptive_system.insert_many(relations)(descr0) in   
  YY(p_nevez,l_nevez,visual0,descr_nevez,old_descr,old_p0);;
     
    
   let see_solution syst=
    let bindings=optimal_binding(syst) in
    let temp1=Image.image(function (var,value)->
      (var,Vector.of_rational value) ) bindings in
    impose_equalities temp1 syst;;
      
   
 
   
  let well_at_point (YY(p,l,visual,descr,old_descr,old_p)) var=
   List.filter(function sr->
     let v=Simplex_relation.vector_part(sr) in
     not(Rational.eq (Vector.coeff(v)(var)) Rational.zero)
   )(l);;
   
 let gifts (YY(p,l,visual,descr,old_descr,old_p))=List.filter (Simplex_relation.is_a_gift) l;;
 
 let eval_variable (YY(p,l,visual,descr,old_descr,old_p)) var=
   let tempf=(function sr->
    if Simplex_relation.adjuster_part(sr)=var
    then let vec1=Vector.of_rational(Simplex_relation.constant_part(sr))
         and vec2=Simplex_relation.vector_part(sr) in
         Some(Vector.sub vec1 vec2)
    else None     
   ) in
   match Option.find_and_stop(tempf)(l) with
   None->Vector.of_variable(var)
   |Some(v)->v;;
   
  
  let eval_vector yy vec=
    let l=Vector.unveil(vec) in
    let temp1=Image.image(function
     (lambda,var)->(lambda,eval_variable yy var) )(l) in
    Vector.linear_combination temp1;;
     
 type int_set=int Tidel.set;;   
   
  let isolate_items (li:int_set) (YY(p,l,visual,descr,old_descr,old_p))=
    let ind_l=Ennig.index_everything(l) in
    let temp1=List.filter(function x->Tidel.elfenn (fst x) li)(ind_l) in
    let new_l=Image.image snd temp1 in
    YY(p,new_l,visual,descr,old_descr,old_p);;
   
  let remove_items (li:int_set) (YY(p,l,visual,descr,old_descr,old_p))=
    let ind_l=Ennig.index_everything(l) in
    let temp1=List.filter(function x->Tidel.elfenn (fst x) li)(ind_l) in
    let new_l=Image.image snd temp1 in
    YY(p,new_l,visual,descr,old_descr,old_p);; 
   
   let read (YY(p,l,visual,descr,old_descr,old_p))=
 let s1="p="^Vector.print(p)^"\n"
 and s2=Sliced_string.print(Sliced_string.itemize(Simplex_relation.print)(l)) in
 "\n"^s1^s2^"\n\n";;
 

 let print_out (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(read x);
   Format.close_box();;
  

let a_below_1 i=
  let var=Variable.basic(i) in
  let vec=Vector.of_variable var
  and varc=Variable.toggle(Variable.basic i) in
  Simplex_relation.constructor vec varc (Rational.one);;

 
let original_descr l=
  let tempf=(fun sr->
    let toggled_var=Simplex_relation.adjuster_part(sr) in
    let var=Variable.toggle(toggled_var) 
    and veg=Simplex_relation.vector_part(sr) in
    let d1=Vector.sub(Vector.of_variable var)(veg)
    and d2=Vector.add(Vector.of_variable toggled_var )(veg) in
    [Relation.of_vector d1;Relation.of_vector d2]
  ) in
  let temp1=List.flatten(Image.image(tempf)(l)) in
  Descriptive_system.make(temp1);; 
 
type int_interdiction=int Nontranslatable_interdiction.t;; 
 
let general_theodoracopulos_rel  (c:int_interdiction)=
 match Nontranslatable_interdiction.unveil(c) with
 (set_ii,mu_of_ii,anv)->
 let ii=Tidel.forget_order(set_ii) in
 let completer=Variable.toggle
 (Variable.of_string(Nontranslatable_interdiction.complete_name(c))) in
 let v1=Vector.sum_of_variables(Image.image Variable.basic ii)  in
 Simplex_relation.constructor v1 completer (Rational.of_int mu_of_ii);;
 
type int_variety=int Theodoracopulos.variety;;

let system_from_theodoracopulos (th:int_variety)=
 let temp1=Theodoracopulos.determinations(th) in
 let temp2=Ordered.image(function c->match 
              Nontranslatable_interdiction.unveil(c) with
              (x,m,anv)->x )(temp1) in
 let supp=Tidel.big_teuzin(temp2) in
 let main_list=Tidel.rev_map Variable.basic supp in
 let main_vector=Vector.sum_of_variables(main_list) 
 and temp3=Ordered.image general_theodoracopulos_rel temp1
 and temp4=Tidel.image(a_below_1)(supp) in
 let l=temp3@temp4 in
 YY(main_vector,l,List.rev main_list,
   Descriptive_system.make [],original_descr l,main_vector);;

let set_value_of_ne_element s i v=
  let vi=Variable.basic(i) in
  impose_simple_equality vi v s;;

let add_n1_element s i=set_value_of_ne_element s i Rational.zero;;
  
let add_e1_element s i=set_value_of_ne_element s i Rational.one;; 
  
 
let add_n1_elements l s=List.fold_left add_n1_element s l;; 
let add_e1_elements l s=List.fold_left add_e1_element s l;; 

let add_ne_elements (ln,le) syst=add_n1_elements ln (add_e1_elements le syst);;



let system_from_theodoracopulos_and_inert_variables (th:int_variety) (yy:int_set)=
 let temp1=Theodoracopulos.determinations(th) in
 let temp2=Ordered.image(function c->match 
              Nontranslatable_interdiction.unveil(c) with
              (x,m,anv)->x )(temp1) in
 let supp=Tidel.big_teuzin(yy::temp2) in
 let main_list=Tidel.rev_map Variable.basic supp in
 let main_vector=Vector.sum_of_variables(main_list) 
 and temp3=Ordered.image general_theodoracopulos_rel temp1
 and temp4=Tidel.image(a_below_1)(supp) in
 let l=temp3@temp4 in
 YY(main_vector,l,List.rev main_list,
   Descriptive_system.make [],original_descr l,main_vector);;


let system_from_impure_theodoracopulos_and_inert_variables (th:int_variety) (yy:int_set)=
 let syst0=system_from_theodoracopulos_and_inert_variables(th)(yy) 
 and ln=Tidel.forget_order(Theodoracopulos.n1_determinations(th))
 and le=Tidel.forget_order(Theodoracopulos.e1_determinations(th)) in
 add_ne_elements (ln,le) syst0;;
 


let change_main_vector yy new_p=
   match yy with YY(p,l,visual,descr,old_descr,old_p)->
  let supp=(Vector.support new_p) in
  let temp1=Image.image(function simplex_rel->
    let ttemp1=Simplex_relation.possible_adjusters(simplex_rel) in
    (simplex_rel,Tidel.lemel(ttemp1)(supp))
  )(l) in
  let hall_data=Hall_algorithm.constructor(Simplex_relation.eq)(temp1) in
  let hall_solution=Hall_algorithm.solve(Simplex_relation.eq)(hall_data) in
  let tempf=(function (sr,adjuster)->
     Simplex_relation.change_adjusters_carefully
      sr adjuster
     ) in
  let new_l=Image.image(tempf)(hall_solution) in
  YY(new_p,new_l,visual,descr,old_descr,new_p);;
  
let rewrite_main_vector yy new_p=
 match yy with YY(p,l,visual,descr,old_descr,old_p)->
  let normalized_new_p=eval_vector(yy)(new_p) in
  let d=Vector.sub(p)(normalized_new_p) in
  if Vector.unveil(d)=[]
  then change_main_vector yy new_p
  else failwith("What you provided was not a rewriting.\n"^
       "There is a difference of "^(Vector.print d));;

let flatten syst=
    let p=main_vector(syst) in
    let temp1=Vector.unveil(p) in
    match Option.find_it(function (lambda,x)->
       (Rational.is_negative lambda)&&(x<>Variable.dummy) )(temp1) with
    None->failwith("There is nothing to flatten.")
    |Some(lambda0,x0)->
     let temp2=Vector.suppress_one_component(p)(Variable.dummy) in
     let temp3=Vector.suppress_one_component(temp2)(x0) 
     and mu=Rational.opposite_of_inverse(lambda0) in
     let temp4=Vector.dot mu temp3 in
     impose_equalities [x0,temp4] syst;;
  
 let standard_constructor positive_vectors vector_to_maximize=
    let temp1=Vector.extract_basis_carefully(positive_vectors) in
    let the_basis=fst(temp1)
    and positivities=Image.image snd (snd temp1) in
    match Vector.write_as_linear_combination(vector_to_maximize)(the_basis) with
    None->failwith("Not enough info: The variable vector is not a combination of the positive vectors.")
    |Some(towards_p)->
       let n=List.length(towards_p) in
       let basic_names=Ennig.doyle(fun i->Variable.of_string("xiii"^(string_of_int i)))(1)(n) in
        let rewriter=(fun l_rat->
          Vector.safe_vector(List.combine(l_rat)(basic_names))
       ) in
       let p=rewriter(towards_p) in
       let temp3=List.rev_map(Vector.support)(positive_vectors) in
       let temp4=Ordered_variable.big_teuzin(temp3) in
       let visual=Ordered_variable.lemel(temp4)(Ordered_variable.singleton Variable.dummy) in
       let visual_vectors=Ordered_variable.image(Vector.of_variable)(visual)
       and decomposer=(fun v->match Vector.write_as_linear_combination(v)(the_basis) with
       None->failwith("bad initial system : vector "^(Vector.print v)^"not decomposable")
       |Some(dig)->
           let ttemp1=List.combine(dig)(basic_names) in
           let w=Vector.safe_vector(ttemp1) in
            Relation.of_vector(Vector.sub v w) ) in
       let relations=Image.image(decomposer)(visual_vectors) in
       let descr=Descriptive_system.make(relations) in     
       let temp5=Ennig.index_everything(Image.image rewriter positivities) in
       let l=Image.image (fun (j,v)->
          let var=Variable.of_string("yiii"^(string_of_int j)) 
          and w=Vector.dot(Rational.minus_one)(v) in
          Simplex_relation.constructor w var Rational.zero 
       ) temp5 in
       YY(p,l,Ordered_variable.forget_order visual,descr,original_descr l,p);;
 

let cumulative_van_der_waerden n=
  let th=Theodoracopulos.cumulate_intervals_for_vdw(n) in
  system_from_theodoracopulos_and_inert_variables th (Tidel.safe_set(Ennig.ennig 1 n));;

let cumulative_van_der_waerden_with_constraints n l=
  let th=Theodoracopulos.cumulate_intervals_and_add_constraints_for_vdw(n)(l) in
  system_from_theodoracopulos_and_inert_variables th (Tidel.safe_set(Ennig.ennig 1 n));;


let constructor_from_absolute_values lv p=
  let tempf=(fun (v,bound)->
   let v_bound=Vector.of_rational(bound) in
   [Vector.add v_bound v;Vector.sub v_bound v]
  ) in
  let temp1=Image.image tempf lv in
  let temp2=List.flatten(temp1) in
  standard_constructor temp2 p;;

 let maximum_value_from_first_breakthrough syst2=
    if number_of_positive_coefficients(syst2)>0
    then failwith("Sorry, IÕm stuck. Call first_breakthrough to see how far I got.")
    else match syst2 with
         YY(p,l,visual,descr,old_descr,old_p)->
         Vector.coeff p (Variable.dummy);;
         

 let maximum_value syst=
   maximum_value_from_first_breakthrough (first_breakthrough syst);;
    
         