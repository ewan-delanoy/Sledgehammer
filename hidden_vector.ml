
type t=V of (Rational.t*Variable.t) list;;


let unveil (V l)=l;;

 type relation=Is_zero of (Rational.t*Variable.t) list;;
 type descriptive_system=DS of (Variable.t*t) list;; 

let unveil_relation (Is_zero l)=l;;
let unveil_descriptive_system (DS l)=l;;


let to_relation (V l)=Is_zero l;;

let dot lambda (V v1)=
  if Rational.is_zero(lambda)
  then V[]
  else V(Image.image(function (r,anv)->(Rational.mult lambda r,anv) )(v1));;
  
 let print_constant (V l)=match l with
 []->Rational.zero
 |a::b->if b<>[]
        then failwith("the vector "^Variable.unveil(snd(List.hd b))^" is not constant")
        else if snd(a)<>Variable.dummy
             then failwith("the vector "^(Variable.unveil(snd(a)))^" is not constant")
             else fst(a);;

 let isolate (Is_zero l) anv1=
   let temp1=List.partition(function (r,anv)->anv=anv1)(l) in
   let c=fst(temp1) in
   if c=[] then failwith("scope error") else 
   let rx=fst(List.hd c) and peurrest=snd(temp1) in
   let irx=Rational.opposite_of_inverse(rx) in
   (dot irx (V peurrest));;


 let dissect_descriptive_system (DS l) variables=
  if l=[] then failwith("nothing to do") else
  if List.hd(l)=(Variable.dummy,V[]) then failwith("the system has no solution") else
  let tempf=(function anv->match Listennou.assoc(l)(anv) with
  None->Rational.zero
  |Some(V(z))->if z=[] then Rational.zero else fst(List.hd(z)) ) in
  Image.image tempf variables;;
 
  
  let test_for_injectivity (DS l)=
   if List.hd(l)=(Variable.dummy,V[]) then failwith("the system has no solution (so it is injective)") else
   List.for_all(function (x,y)->y=V[])(l);;
   
 let gauss_multiplier (V l)=Rational.gauss_multiplier (Image.image fst l);;  

 let gauss_operation (V l)=V(Rational.gauss_operation l);;
   
let sign_preserving_gauss_operation (V l)=V(Rational.sign_preserving_gauss_operation l);;   

let add (V v1) (V v2)=
 let rec tempf=(function
 (graet,da_ober1,da_ober2)->
  if da_ober1=[] then List.rev_append(graet)(da_ober2) else
  if da_ober2=[] then List.rev_append(graet)(da_ober1) else
  let c1=List.hd(da_ober1) and peurrest1=List.tl(da_ober1) 
  and c2=List.hd(da_ober2) and peurrest2=List.tl(da_ober2) in
  let r1=fst(c1) and x1=snd(c1) 
  and r2=fst(c2) and x2=snd(c2) in
  match Variable.cmp(x1)(x2) with
   Total_ordering.Lower->tempf(c1::graet,peurrest1,da_ober2)
  |Total_ordering.Equal->let r=Rational.add(r1)(r2) in
	                     if Rational.is_zero(r) 
			             then tempf(graet,peurrest1,peurrest2)
			             else tempf((r,x1)::graet,peurrest1,peurrest2)
  |Total_ordering.Greater->tempf(c2::graet,da_ober1,peurrest2)
 ) in
 V(tempf([],v1,v2));;
 
  
let unsafe_vector l=V(l);;

let ocaml_name (V l)=
  let temp1=Image.image(function
   (lambda,var)->
     "("^(Rational.ocaml_name lambda)^","^(Variable.ocaml_name var)^")"
  )(l) in
  let name_for_l="["^String.concat(";")(temp1)^"]" in
  "Vecto"^"r.unsafe_vector("^name_for_l^")";;

let number_of_positive_coefficients (V l)= 
  let temp1=List.filter(function (lambda,x)->
    (Rational.is_positive lambda)&&(x<>Variable.dummy)
  )(l) in
  List.length temp1;;
  

let safe_vector l=
  let temp0=List.filter(function (lambda,x)->not(Rational.eq lambda Rational.zero))(l) in
  let temp1=List.rev_map (function (lambda,x)->V[lambda,x]) temp0 in
  List.fold_left add (V[]) temp1;;
 
let scalar_product  (V v1) (V v2)=
 let rec tempf=(function
 (graet,da_ober1,da_ober2)->
  if da_ober1=[] then graet else
  if da_ober2=[] then graet else
  let c1=List.hd(da_ober1) and peurrest1=List.tl(da_ober1) 
  and c2=List.hd(da_ober2) and peurrest2=List.tl(da_ober2) in
  let r1=fst(c1) and x1=snd(c1) 
  and r2=fst(c2) and x2=snd(c2) in
  match Variable.cmp(x1)(x2) with
   Total_ordering.Lower->tempf(graet,peurrest1,da_ober2)
  |Total_ordering.Equal->let r=Rational.mult(r1)(r2) in
                         tempf(Rational.add graet r,peurrest1,peurrest2)
  |Total_ordering.Greater->tempf(graet,da_ober1,peurrest2)
 ) in
 tempf(Rational.zero,v1,v2);;
  

 let sub v1 v2=
   add v1 (dot (Rational.minus_one) v2);;
   
 let subs (V v1) anv1 ersatz=
   let temp1=List.partition(function (r,anv)->anv=anv1)(v1) in
   let c=fst(temp1) in
   if c=[] then (V v1) else 
   let rx=fst(List.hd c) and peurrest=snd(temp1) in
   add (V peurrest) (dot rx ersatz);;
   
 let uncurrified_subs v (anv,ersatz)=subs v anv ersatz;;  
 
  let fold_subs ds v=List.fold_left(uncurrified_subs) v ds;;
 
 let big_subs v (DS ds)=List.fold_left(uncurrified_subs) v ds;;
 
 let big_sum l=List.fold_left add (V[]) l;;
 
 let linear_combination lc=
  let temp1=List.rev_map (function (a,vec)->dot a vec) lc in
  big_sum temp1;;
 
 let weak_subs anv1 rat (V v1) =
    let temp1=List.partition(function (r,anv)->anv=anv1)(v1) in
   let c=fst(temp1) in
   if c=[] then (V v1) else 
   let rx=fst(List.hd c) and peurrest=snd(temp1) in
   add (V peurrest) (dot (Rational.mult rx rat) (V[Rational.one,Variable.dummy]));;
   
  let uncurrified_weak_subs v (anv,ersatz)=weak_subs anv ersatz v;;   
 
  let fold_weak_subs ds v=List.fold_left(uncurrified_weak_subs) v ds;;
 


  let replace_in_descriptive_system  anv1 ersatz (DS l)=
   (*it is assumed that anv1 is not described in l*)
   DS(Image.image (function (anv_x,lx)->(anv_x,subs  lx anv1 ersatz) ) l);;
   
  let replace_many_in_descriptive_system 
   replacements (DS l)=List.fold_left
     (fun ds (anv,anv_nevez)->replace_in_descriptive_system anv anv_nevez ds)
     (DS l) replacements;;
   
   
  let insert_in_a_and_b (x1,descr_x1) (DS lx)=
    let rec tempf=(function
	  (graet,da_ober)->match da_ober with
	  []->List.rev_append(graet)([(x1,descr_x1)])
	  |(x,descr_x)::peurrest->
	    if Total_ordering.lt (Variable.cmp) x1 x
		then List.rev_append(graet)( (x1,descr_x1)::da_ober)
		else tempf((x,descr_x)::graet,peurrest)
	)  in
	DS(tempf([],lx));;
   
  let rec insert_in_descriptive_system  (Is_zero(l)) (DS ds)=
   match List.rev(l) with
   []->(DS ds)
   |(lambda,x)::peurrest->
	      let lambda1=Rational.opposite_of_inverse(lambda)
          and vect1=(V (List.rev peurrest)) in
		  let vect2=dot(lambda1)(vect1) in
		  let vect3=big_subs(vect2)(DS ds) in
		  match Listennou.assoc(ds)(x) with
		  None->let ds1=replace_in_descriptive_system(x)(vect3)(DS ds) in
		                  insert_in_a_and_b(x,vect3) ds1   
		  |Some(other_descr_of_x)->
		     let new_relation=Is_zero((function V(u)->u)(sub  vect3 other_descr_of_x)) in
			 insert_in_descriptive_system  new_relation (DS ds);;
   
  let insert_simple_equality_in_descriptive_system var lambda=
    let temp1=V[Rational.one,var] in
    let temp2=sub(temp1)(V[lambda,Variable.dummy]) in
    let temp3=to_relation(temp2) in
    insert_in_descriptive_system temp3;;
    
  let insert_several_simple_equalities_in_descriptive_system several_eqs ds0=
   List.fold_left(fun ds (var,valeu)->insert_simple_equality_in_descriptive_system var valeu ds)
   ds0 several_eqs;;
  
  
  let insert_many_in_descriptive_system several_eqs ds0=
   List.fold_left(fun ds rel->insert_in_descriptive_system rel ds)
   ds0 several_eqs;;
   
  let make_descriptive  lr=Listennou.fold_right(insert_in_descriptive_system ) (DS []) lr;; 
   
  let equality  v1 v2=match (sub  v1 v2) with
    V(u)->Is_zero u;;
	
   let variable_equals_combination  anv lv=
     equality  (V[Rational.one,anv]) lv;;
	
  let coeff (V v) anv=
   	let rec tempf=(function
	 []->Rational.zero
	 |(lambda,s)::peurrest->
	    match Variable.cmp(anv)(s) with
	     Total_ordering.Lower->Rational.zero
	    |Total_ordering.Equal->lambda
	    |Total_ordering.Greater->tempf(peurrest)
	) in
  tempf v;;	
  
  let support_for_many_vectors  lv=
   let temp1=List.rev_map(function V(u)->Ordered_variable.unsafe_set(Image.image(snd)(u)) )(lv) in
   Ordered_variable.big_teuzin(temp1);;
  
   let the_solution_is_a_line  (DS l) variables= 
   (*here we assume that dummy_variable is a minimal element. *)
    let temp1=Ordered_variable.unsafe_set(Image.image(fst)(l))   
    and temp2=Ordered_variable.diforchan(variables) in
    let temp3=Ordered_variable.lemel(temp2)(temp1) in
    Ordered_variable.length(temp3)=1;;
  
  let abstract_linear_combination  lv=
	let support=support_for_many_vectors(lv) in
	let temp2=Ennig.index_everything(lv) in
    let temp3=Image.image(function (i,v)->(Variable.specific_name(i),v))(temp2) in
    let tempf=(function a->
              let ttemp1=Image.image(function (anv_yyy,v)->(coeff(v)(a),anv_yyy) )(temp3) in
			  let ttemp2=List.filter(function (r,anv)->not(Rational.is_zero r) )(ttemp1) in
			   (V(ttemp2),a)  
				) in
	let temp4=Ordered_variable.image(tempf)(support)	in			
	temp4;;
	
   
  let write_out_system  (V w) lv=
   (*we assume that the support of w is included in the support of lv*)
	    let temp3=abstract_linear_combination(lv) 
		and tempf=(function
		  (vec,anv)->
		    match Listennou.r_assoc(w)(anv) with
			None->equality(vec)(V[])
			|Some(lambda)->equality(vec)(V[lambda,Variable.dummy])

		) in
		let temp4=Image.image(tempf)(temp3) in
		make_descriptive  temp4;;
   
    let write_as_linear_combination  (V w) lv=
   let temp1=Ordered_variable.unsafe_set(Image.image(snd)(w)) in
   let temp2=support_for_many_vectors(lv) in
   let temp3=Ennig.doyle(function j->Variable.specific_name(j))(1)(List.length lv) in
   if Ordered_variable.nental(temp1)(temp2)
   then None
   else if w=[] then Some[] else
        let temp4=write_out_system(V w)(lv) in
		try (Some(dissect_descriptive_system(temp4)(temp3) )) with
		Failure(x)->None;;
		 
   let write_as_a_linear_combination_of_l_variables w old_lv=
    let unan=V[Rational.one,Variable.dummy] 
    and rlv=Three_parts.generic(old_lv) in
    let search=Option.find_it(fun (x,y,z)->y=unan)(rlv) in
    let lv=(function None->unan::old_lv 
                    |Some(x,y,z)->y::(List.rev_append x z) )(search) 
    and lvar=(function 0->Variable.of_string("u")
                      |j->let s="\202\160"^(string_of_int(j)) in Variable.of_string(s) )in
    match (write_as_linear_combination w lv) with
    None->None
    |Some(lc)->
       let ilc=Ennig.index_everything(lc)
       and tempf=(fun (j,lambda)->(lambda,lvar (j-1))) in
       Some(safe_vector (Image.image tempf ilc));;
       



     let extract_basis  lv=
	   let rec tempf=(function 
	     (graet,da_ober)->match da_ober with
	      []->List.rev(graet)
		  |v1::peurrest->match write_as_linear_combination(v1)(graet) with
		      None->tempf(v1::graet,peurrest)
			  |Some(x)->tempf(graet,peurrest)
 	   ) in
	   tempf ([],lv);;
	   
	   let extract_basis_carefully  lv=
	   let rec tempf=(function 
	     (graet,da_ober,jedadennou)->match da_ober with
	      []->(List.rev(graet),List.rev_map (fun (v,dig)->(v,List.rev dig) ) jedadennou)
		  |v1::peurrest->match write_as_linear_combination(v1)(graet) with
		      None->tempf(v1::graet,peurrest,Image.image (fun (v,dig)->
		                             (v,Rational.zero::dig) ) jedadennou)
			  |Some(x)->tempf(graet,peurrest,(v1,x)::jedadennou)
 	   ) in
	   tempf ([],lv,[]);; 
	
	let is_zero (V v1)=(v1=[]);;
	
	let eq v1 v2=is_zero(sub v1 v2);;
	   
	
 let extract_named_basis lv=
	  let vektorou=Image.image snd lv in
	  let anv_klok=(fun v->
	    Option.find_really(fun (x,y)->eq y v)(lv)
	  ) in
	  let (temp1,temp2)=extract_basis_carefully(vektorou) in
	  let temp3=Image.image anv_klok temp1 in
	  let tempf=(function (v,l)->
	     let ttemp1=List.combine l temp3 in
	     let ttemp2=Image.image (fun (lamb,(anv,veg))->(lamb,anv)) ttemp1 in
	     (fst(anv_klok v),safe_vector ttemp2)
	  ) in
	  let temp4=Image.image tempf temp2 in
	  (temp3,temp4);;
	
 let test_for_independence  lv=
	 let rec tempf=(function 
	   (graet,da_ober)->match da_ober with
	    []->true
		|v1::peurrest->match write_as_linear_combination(v1)(graet) with
		    None->tempf(v1::graet,peurrest)
			|Some(x)->false
 	  ) in
	  tempf ([],lv);;    
	
 let particular_ray  ds an_holl unan=
     let tempf=(function x->if x=unan then Rational.one else Rational.zero) in
     let temp1=Image.image(function x->
              variable_equals_combination(x)(V[tempf(x),Variable.dummy]) )(an_holl) in
     let new_ds=Listennou.fold_right(insert_in_descriptive_system )(ds)(temp1) in
          match new_ds with
      DS(ll)->let temp1=Image.image(function (x,v)->(print_constant v,x) )(ll) in
              let temp2=List.filter(function (c,anv)->c<>Rational.zero)(temp1) in
               V temp2;; 
          
 let enumerate_all_rays  (DS l)=
    let temp1=List.rev_map(snd)(l) in
    let an_holl=Ordered_variable.forget_order(support_for_many_vectors(temp1)) in
    Image.image(particular_ray  (DS l) an_holl)(an_holl);;
        
	   
 let defining_equations_for_subspace  lv=
	 let temp1=List.rev_map(function V(l)->Is_zero(l))(lv) in
	 let ds=make_descriptive(temp1) in
	 enumerate_all_rays  ds;; 
	     
 let general_inverse_matrix  lv1 lv2=
	let tempf=(function (i,w)-> 
	   let ttemp=write_as_linear_combination(w)(lv1) in
	  Option.propagate (function z->(i,z)) ttemp ) in
	  Option.filter_and_unpack tempf (Ennig.index_everything lv2);;
	     
 let inverse_matrix  lv=
   let temp1=Ordered_variable.forget_order(support_for_many_vectors(lv)) in
	if List.length(lv)<>List.length(temp1)
	then failwith("dimension problem")
	else let tempf=(function anv->let w=V[Rational.one,anv] in 
	     let ttemp=write_as_linear_combination(w)(lv) in
	     Option.propagate (function z->(anv,z)) ttemp ) in
	     Option.filter_and_unpack tempf temp1;;
	      
 let of_list l=
   let temp1=List.rev_map(function x->V[Rational.one,x])(l) in
    List.fold_left(add)(V[]) temp1;;
        
 let of_dlist  l1 l2=
    let v1=of_list(l1)
     and v2=of_list(l2) in
     sub  v1 v2;;
 

 let support v=support_for_many_vectors [v];;
 
 
 let suppress_one_component (V v) var=
 V(List.filter(function (x,var1)->var1<>var) v);;
 
 let suppress_unity_and_normalize v=
  let dum=Variable.dummy in
  let valw=coeff(v)(dum) in
  let  vec1=suppress_one_component(v)(dum) in
  let vec2=gauss_operation(dot(Rational.minus_one)(vec1)) in
  (valw,vec2);;
 
 
 let special_left_print a t1=
  if t1=Variable.dummy 
  then Rational.print(a)
  else Rational.print_leftmost(a)^(Variable.unveil t1);;
   
 let print (V v1)=match v1 with
[]->"0"
|(a,t1)::peurrest->
   let temp1=special_left_print a t1
   and temp2=Image.image(function (lambda,t)->Rational.print_in_between(lambda)^(Variable.unveil t) )(peurrest) in
   String.concat("")(temp1::temp2);;
   
 let print_off v=print_string(print v);;  
 
 let print_out (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print x);
   Format.close_box();;
  
 let print_homogeneous_vector (V v)=
 if v=[] then "" else
   let temp1=List.partition(function (x,anv)->Rational.is_negative x)(v) in
   let neg=Image.image(function (x,anv)->(Rational.minus x, anv))(fst temp1) 
   and pos=snd(temp1) in
   (print (V neg))^"="^(print(V pos));;
   
  
 let print_relation rel=
  let vec=(function Is_zero(l)->V(l))(rel) in
  let c=coeff(vec)(Variable.dummy) in
  if (c=Rational.zero)
  then print_homogeneous_vector(vec)
  else let epsilon=(function ()-> if Rational.is_positive(c)
       then Rational.one else Rational.minus_one)() in
       let oc=Rational.mult(epsilon)(c) in
       let vec2=dot(epsilon)(vec) in
       let vec3=(function anything->if oc=Rational.zero then V[] else V[oc,Variable.dummy])() in
       let vec4=sub(vec3)(vec2) in
       print(vec4)^"="^(Rational.print oc);;
   
 let print_out_relation (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print_relation x);
   Format.close_box();;
    
   
   
 let print_inequality (V v)=
   if v=[] then "" else
   let temp1=List.partition(function (x,anv)->Rational.is_negative x)(v) in
   let neg=Image.image(function (x,anv)->(Rational.minus x, anv))(fst temp1) 
   and pos=snd(temp1) in
   (print (V neg))^"<"^(print(V pos));;
   
   
   
 let print_descriptive_system (DS ds)=
  let temp1=Image.image(function (anv,descr)->(Variable.unveil anv)^"="^print(descr) )(ds) in
  let temp2=String.concat(";")(temp1) in
  temp2;;
  
 let print_out_descriptive_system (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print_descriptive_system x);
   Format.close_box();; 
   
 let is_positive (V v1)=
 if v1=[] 
 then false
 else List.for_all(function (x,y)->Rational.is_positive x)(v1);;	
	 
 let compare v1 v2=(*this is a partial ordering. *)
 is_positive(sub v2 v1);;	 
 
 let cmp=
 ((fun (V v1) (V v2)->
 let u1=Total_ordering.standard(List.length v1)(List.length v2) in
 if u1<>Total_ordering.Equal then u1 else
 let w1=Ordered_variable.unsafe_set(Image.image(snd)(v1))
 and w2=Ordered_variable.unsafe_set(Image.image(snd)(v2)) in
 let u2=Ordered_bare_set.cmp(w1)(w2) in
 if u2<>Total_ordering.Equal then u2 else
 Tidel.cmp(v1)(v2)): t Total_ordering.t);;
 
 
 
 
