let rec main_helper=function
(a,b,da_ober)->match da_ober with
[]->a
|(x1,y1)::peurrest->
  let temp1=Polynomial.dot(y1)(b) in
  let new_a=Polynomial.add(a)(temp1) in
  if List.for_all(function (x,y)->Rational.eq y y1)(peurrest)
  then new_a
  else let tempf=(function
        (x,y)->let dx=Rational.sub(x)(x1)
               and dy=Rational.sub(y)(y1) in
               let dd=Rational.div(dy)(dx) in
               (x,dd)
        ) in
       let minus_x1=Rational.opposite(x1) in
       let multiplier=Polynomial.constructor[minus_x1,0;Rational.one,1] in
       let new_equalities=Image.image(tempf)(peurrest)
       and new_b=Polynomial.mult(multiplier)(b) in
       main_helper(new_a,new_b,new_equalities);;

let generic_interpolation l=main_helper(Polynomial.zero,Polynomial.one,l);;

let int_to_rational l=
   let adjusted_l=Image.image(function (x,y)->(Rational.of_int x,y)) l in
   generic_interpolation adjusted_l;;
   
let int_to_int l=
   let adjusted_l=Image.image(function (x,y)->(Rational.of_int x, Rational.of_int y)) l in
   generic_interpolation adjusted_l;;   

let regularly_spaced_int_to_rational j0 l=
  let temp1=Ennig.index_everything(l) in
  let temp2=List.rev_map(fun (j,v)->(Rational.of_int(j0+j-1),v))(temp1) in
  generic_interpolation temp2;;
   
let regularly_spaced_int_to_int j0 l=
  let temp1=Ennig.index_everything(l) in
  let temp2=List.rev_map(fun (j,v)->(Rational.of_int(j0+j-1),Rational.of_int v))(temp1) in
  generic_interpolation temp2;;   
   
let usual_int l=int_to_int(Ennig.index_everything l);;  



let maple_syntax n=
 if (n<2) then failwith("n is too small") else
 let temp1=Ennig.doyle(fun j->
        let ttemp1=Ennig.doyle(fun x->if x=j then 1 else 0)(1)(n) in
        usual_int(ttemp1)
      )(1)(n) in
 let temp2=Ennig.index_everything(Image.image(Polynomial.unveil)(temp1)) in
 let tempf1=(fun k->
  let ttemp1=Image.image(fun (j,l_pol)->(List.nth l_pol k,"y"^(string_of_int j)) )(temp2) in
  List.filter(fun (x,j)->not(Rational.is_zero x))(ttemp1)
 ) in     
let temp3=Ennig.doyle(fun k->(k,tempf1 k) )(0)(n-1) in
let temp4=Image.image(fun (k,l)->(k,Vector.maple_syntax l) )(temp3) in
let tempf2=(fun (j,s_coeff)->"("^s_coeff^")*(t^"^(string_of_int j)^")") in
let s0=(fun (j,s_coeff)->"("^s_coeff^")" )(List.hd temp4)
and s1=(fun (j,s_coeff)->"("^s_coeff^")*t" )(List.nth temp4 1) in
let ls=s0::s1::(Image.image tempf2 (Listennou.big_tail 2 temp4)) in
let temp5=String.concat "+" ls in
let temp6=String.concat(",")(Ennig.doyle (function i->"y"^(string_of_int i)) 1 n) in
let r0="interpol"^(string_of_int n)^":=("
and r1=temp6^",t)->"
and r2=temp5^";" in
r0^r1^r2;;
 
let with_imposed_bound_on_degree b l=
  let d=List.length(l)-1 in
  if b>=d
  then Some(generic_interpolation l)
  else let smaller_l=Listennou.big_tail(b)(l) in
   let candidate=generic_interpolation(smaller_l) in
   if List.for_all(fun (x,y)->Polynomial.eval candidate x=y)(l)
   then Some(candidate)
   else None;;
let generic_interpolation_frac=  
  let rec generic_interpolation_frac0=(fun (a,b) l->
  (* Caution : all the values in l must be nonzero.*)
  if (a<b) 
  then let inverted_l=List.rev_map(fun (x,y)->(x,Rational.inverse y))(l) in
       let temp1=generic_interpolation_frac0(b,a)(inverted_l) in
       Option.propagate Rat_frac.inverse temp1
  else 
  if b=0
  then Option.propagate(Rat_frac.of_polynomial) (with_imposed_bound_on_degree a l) 
  else match l with
       []->Some(Rat_frac.one)
      |(x1,y1)::peurrest->
        let temp1=List.partition(fun (x,y)->y=y1)(l) in
        let temp2=List.rev_map(fst)(fst temp1) and temp3=(snd temp1) in
        if temp3=[]
        then Some(Rat_frac.constant y1)
        else
        let r=List.length(temp2) in
        let new_a=a-r in
        if (new_a<0) then None else
        let factorized_pol=Polynomial.constructor_from_roots(temp2) in
        let temp4=List.rev_map(fun (x,y)->
           let dy=Rational.sub(y)(y1) and p=Polynomial.eval(factorized_pol)(x) in
           (x,Rational.div dy p)
        )(temp3) in
        let temp5=generic_interpolation_frac0 (new_a,b) temp4 in
        let temp6=Option.propagate (fun fr->
           let ttemp1=Rat_frac.big_dot factorized_pol fr
           and ttemp2=Rat_frac.constant(y1) in
           Rat_frac.add(ttemp1)(ttemp2)
         )(temp5) in
        temp6) in
(fun (a,b) l->
  if (a<0)||(b<0) then failwith("Negative degree!") else
  let rec exteriorizer=(fun j->
    let rj=Rational.of_int(j) in
    if List.exists(fun (x,y)->y=rj)(l)
    then exteriorizer(j+1)
    else j
  ) in
  let j0=Rational.of_int(exteriorizer 0) in
  let modified_l=List.rev_map(fun (x,y)->(x,Rational.sub y j0)) l 
  and adjuster=Rat_frac.add(Rat_frac.constant j0) in
  let temp=generic_interpolation_frac0 (a,b) modified_l in
  Option.propagate adjuster temp);;
  
 let frac_for_a_list n l=generic_interpolation_frac (n,n) l;; 
 
 let frac_for_a_function n f=
   frac_for_a_list n
   (Ennig.doyle (fun x->let rx=Rational.of_int x in (rx,f x)) 1 (2*n+1));;   
   
   
   
   
   
   
   
   
   
   

