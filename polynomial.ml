type t=P of Rational.t list;;

let unveil (P l)=l;;

let rec cleanup_zeroes_on_the_left =function
[]->[]
|a::b->if Rational.eq(a)(Rational.zero)
       then cleanup_zeroes_on_the_left(b)
       else a::b;;

let cleanup_zeroes_on_the_right l=
 List.rev(cleanup_zeroes_on_the_left(List.rev l));;

let rec add0=function
(accu,x,y)->
  if x=[] then cleanup_zeroes_on_the_right(List.rev_append(accu)(y)) else
  if y=[] then cleanup_zeroes_on_the_right(List.rev_append(accu)(x)) else
  let head_x=List.hd(x) and tail_x=List.tl(x)
  and head_y=List.hd(y) and tail_y=List.tl(y) in
  add0(Rational.add(head_x)(head_y)::accu,tail_x,tail_y);;


let add (P x) (P y)=P(add0([],x,y));;

let zero=P[];;

let one=P[Rational.one];;

let constant x=if Rational.eq(x)(Rational.zero) then P[] else P[x];;

let int_constant x=constant(Rational.of_int x);;

let big_sum l=List.fold_left add zero l;;

let dot lambda (P x)=
 if Rational.eq(lambda)(Rational.zero)
 then zero
 else P (Image.image (Rational.mult lambda) x);;

let linear_combination l=
 let temp1=List.rev_map(fun (lamb,pol)->dot lamb pol)(l) in
 big_sum temp1;;

let sub p1 p2=
  let mp2=dot (Rational.minus_one) p2 in
  add p1 mp2;;
  
 let multiply_by_x (P l)=(P(Rational.zero::l));;  
 
 type exponent=int;;
  
 let rec multiply_by_monomial (j:exponent) poly=
  if j<1
  then poly
  else let q=multiply_by_x(poly) in
       multiply_by_monomial (j-1) q;;
       
 let monomial r j=multiply_by_monomial j (constant r);;   
 
 let constructor l=
   let temp1=Image.image(function (r,j)->monomial r j)(l) in
   big_sum temp1;;
   
 let short_constructor l=
  let temp1=Image.image(function (j,r)->monomial r (j-1) )(Ennig.index_everything l) in
  big_sum temp1;;
  
 let short_int_constructor l=
   short_constructor (Image.image Rational.of_int l);;
   
 let int_constructor l=
  let adjusted_l=Image.image(function (ir,j)->(Rational.of_int ir,j) )(l) in
  constructor adjusted_l;;
       
 let rec mult0=function
  (l,xj_by_q,accu)->match l with
  []->accu
  |aj::peurrest->let mj=dot(aj)(xj_by_q) in
                let new_accu=add(mj)(accu) in
                mult0(peurrest,multiply_by_x xj_by_q,new_accu);;
                
 let mult (P l) q=mult0(l,q,zero);;               
                
 let rec compose0=function
  (l,q,qj,accu)->match l with
  []->accu
  |aj::peurrest->let mj=dot(aj)(qj) in
                let new_accu=add(mj)(accu) in
                compose0(peurrest,q,mult q qj,new_accu);;               
                
let compose (P l) q=compose0(l,q,one,zero);; 

let eval0 w=
let rec tempf=(function
  (l,wj,accu)->match l with
  []->accu
  |aj::peurrest->let mj=Rational.mult(aj)(wj) in
                let new_accu=Rational.add(mj)(accu) in
                tempf(peurrest,Rational.mult w wj,new_accu)) in
 tempf;;               
                
let eval (P l) w=eval0 w (l,Rational.one,Rational.zero);; 

let eval_at_int paul i=eval paul (Rational.of_int i);;   

let coeff (P l) j=if (j>=List.length(l))||(j<0) then Rational.zero else List.nth(l)(j);;
                
let degree (P l)=(  ((List.length l)-1):>exponent);;    

let leading_coefficient (P l)=if l=[] then Rational.zero else List.hd(List.rev l);;

let make_unitary pol=
 if pol=zero
 then failwith("0 cannot be made unitary")
 else let lamb=Rational.inverse(leading_coefficient(pol)) in
      dot lamb pol;;

let euclid0 (b,deg_b,lead_b)=
 let rec tempf=(function
(quot,modified_a)->
  let d=degree(modified_a)-deg_b in
  if d<0
  then (quot,modified_a)
  else let c=Rational.div(leading_coefficient modified_a)(lead_b) in
       let temp1=multiply_by_monomial(d)(b) in
       let temp2=dot(c)(temp1) in
       let newly_modified_a=sub(modified_a)(temp2)
       and new_quot=add(quot)(monomial(c)(d)) in
       tempf(new_quot,newly_modified_a)) in
 tempf;;
                
 let euclid a b=
 if b=zero then failwith("Euclidian division by zero") else
 euclid0(b,degree(b),leading_coefficient(b)) (zero,a);;    
                
 let euclid_quotient a b=fst(euclid a b);;
 
 let euclid_remainder a b=snd(euclid a b);;
 
 
 let rec gcd0 u v=
   (* (u,v) are unitary, v is nonzero and deg(u)>=deg(v) *)
    let r=euclid_remainder(u)(v) in
        if r=zero then v else
        gcd0(v)(make_unitary(r));;
 
 let gcd old_a old_b=
  if old_a=zero 
  then if old_b=zero
       then zero
       else make_unitary(old_b)
  else
  let a1=make_unitary(old_a) in
  if old_b=zero then a1 else
  let b1=make_unitary(old_b) in
  let (a,b)=(function true->(a1,b1) |false->(b1,a1))(degree(old_a)>degree(old_b)) in
  gcd0 a b;;
 
 
 let simple_annulator r=
 constructor [Rational.opposite(r),0;Rational.one,1];;

let constructor_from_roots lr=
  let temp1=List.rev_map(simple_annulator)(lr) in
  List.fold_left mult one temp1;;
 
 let binomial r=
  let temp1=Ennig.doyle Rational.of_int 1 r in
  let paul=constructor_from_roots 
            (List.rev_map (fun x->Rational.sub x Rational.one) temp1) in
  let lamb=Rational.inverse(List.fold_left Rational.mult Rational.one temp1) in
  dot lamb paul;;


 
 let derivative (P l)=
   if l=[] then zero else
   let temp1=Ennig.index_everything(List.tl l) in
   let temp2=Image.image(function (k,lamb)->Rational.mult(Rational.of_int k)(lamb))(temp1) in
   let temp3=cleanup_zeroes_on_the_right(temp2) in
   P(temp3);;
   
  let integrate (P l)=
   if l=[] then one else
   let temp1=Ennig.index_everything(l) in
   let temp2=Image.image(function (k,lamb)->Rational.mult(Rational.frac 1 k)(lamb))(temp1) in
   P(Rational.zero::temp2);;  
   
  let integrate_and_translate pol a=
    let q=integrate(pol) in
    let v=constant(eval(q)(a)) in
    sub q v;;
   
   
 let gauss_multiplier (P l)=
  Rational.gauss_multiplier l;;
  
 let democratic_gauss_multiplier pol=
   let g=gauss_multiplier(pol) in
   match dot g pol with (P l)->
   let temp1=List.filter(fun x->not(Rational.is_zero x) )(l) in
   let temp2=List.partition(Rational.is_negative)(temp1) in
   if ((List.length(fst temp2))>(List.length(snd temp2))) 
   then Rational.opposite(g)
   else g;;
 
 let gauss_operation (P l)=
   if l=[] then zero else
   let temp1=List.rev_map(fun x->(x,()) )(l) in
   let temp2=Rational.gauss_operation(temp1) in
   let temp3=List.rev_map(fst)(temp2) in
  (P temp3);;
 

 
 let current_name_for_x=ref("x");;
 
 let print_power_of_x j=
    if j<1 
    then "" 
    else let x0=(!current_name_for_x) in
         if j=1
         then x0
         else x0^"^"^(string_of_int j);;
 
  let print_power_of_x_for_ocaml j=
    if j<1 
    then "" 
    else let x0=(!current_name_for_x) in
         if j=1
         then x0
         else let temp1=Ennig.doyle (fun t->x0) 1 j in
              String.concat "*" temp1;;
 
 let print (P l)=
  let temp1=Ennig.index_everything(l) in
  let temp2=Option.filter_and_unpack(function (j,w)->
      if Rational.eq w Rational.zero then None else Some(w,j-1) )(temp1) in
  match temp2 with
 []->"0(the zero polynomial)"
 |(w0,j0)::peurrest->
    let temp1=Image.image(function (w,j)->
    Rational.print_in_between(w)^print_power_of_x(j)
    ) (peurrest) in
    let s1=(function j->if j=0 
            then Rational.print(w0)
            else Rational.print_leftmost(w0)^print_power_of_x(j0))(j0)
    and s2=String.concat ("") temp1 in
    s1^s2;;
    
 let print_for_gp (P l)=
  let temp1=Ennig.index_everything(l) in
  let temp2=Option.filter_and_unpack(function (j,w)->
      if Rational.eq w Rational.zero then None else Some(w,j-1) )(temp1) in
  match temp2 with
 []->"0"
 |(w0,j0)::peurrest->
    let temp1=Image.image(function (w,j)->
    Rational.print_in_between(w)^"*"^print_power_of_x(j)
    ) (peurrest) in
    let s1=(function j->if j=0 
            then Rational.print(w0)
            else Rational.print_leftmost(w0)^"*"^print_power_of_x(j0))(j0)
    and s2=String.concat ("") temp1 in
    s1^s2;;   
    
  let print_for_ocaml (P l)=
  let temp1=Ennig.index_everything(l) in
  let temp2=Option.filter_and_unpack(function (j,w)->
      if Rational.eq w Rational.zero then None else Some(w,j-1) )(temp1) in
  match temp2 with
 []->"0"
 |(w0,j0)::peurrest->
    let temp1=Image.image(function (w,j)->
    if Rational.eq w Rational.one 
    then "+"^print_power_of_x_for_ocaml(j)
    else
    if Rational.eq w Rational.minus_one
    then "-"^print_power_of_x_for_ocaml(j) 
    else Rational.print_in_between(w)^"*"^print_power_of_x_for_ocaml(j)
    ) (peurrest) in
    let s1=(function j->if j=0 
            then Rational.print(w0)
            else 
            if Rational.eq w0 Rational.one 
    		then print_power_of_x_for_ocaml(j)
    		else
    		if Rational.eq w0 Rational.minus_one
    		then "(-"^print_power_of_x_for_ocaml(j)^")" 
            else Rational.print_leftmost(w0)^"*"^print_power_of_x_for_ocaml(j0))(j0)
    and s2=String.concat ("") temp1 in
    s1^s2;;      
    
  let exact_division a b=match (euclid a b) with
 (quo,rem)->
   if rem<>P[] 
   then failwith("The division is not exact ("^(print a)^" by "^(print b)^")")
   else quo;;   
   
 let rec taylor paul a=
   if paul=P[] then [] else
   let d=P[Rational.opposite a;Rational.one] in
   let (q,r)=euclid paul d in
   (coeff r 0)::(taylor q a);;
  
 
 let laplace_interpolator1 x1 x2=
  let d=Rational.sub(x2)(x1) in
  let temp1=dot(Rational.of_int 2)(constructor_from_roots [x1]) 
  and temp2=constant(d) in
  let temp3=add(temp1)(temp2) in
  let temp4=mult(constructor_from_roots [x2;x2])(temp3) in
  let d3=Rational.mult(d)(Rational.mult d d) in
  let inverse_of_d3=Rational.inverse(d3) in
  dot(inverse_of_d3) temp4;;

let laplace_interpolator2 x1 x2=
  let d=Rational.sub(x2)(x1) in
  let temp1=(constructor_from_roots [x1;x2;x2]) in
  let d2=Rational.mult(d)(d) in
  let inverse_of_d2=Rational.inverse(d2) in
  dot(inverse_of_d2) temp1;;

let laplace_interpolation 
   (x1,(y1,dy1)) (x2,(y2,dy2))=
     linear_combination
     [
         y1,laplace_interpolator1 x1 x2;
        dy1,laplace_interpolator2 x1 x2;
         y2,laplace_interpolator1 x2 x1;
        dy2,laplace_interpolator2 x2 x1     
     ]
   ;;   
    
 let signs_on_the_integers paul a b=
    let sp=(fun j->Rational.sign (eval paul (Rational.of_int j))) in
    Ennig.describe_fibers_as_intervals sp a b;;
    

    
 let cmp=((fun paul1 paul2->
   let d=sub paul2 paul1 in
   let a=leading_coefficient d in
   Rational.cmp Rational.zero a):> t Total_ordering.t);;
    
 let print_democratically pol=
   let h=democratic_gauss_multiplier(pol) in
   let s=print(dot h pol) and g=Rational.inverse(h) in
   if (Rational.eq g Rational.one) 
   then s
   else if Rational.is_an_integer(g)
        then  (Rational.print g)^"["^s^"]"
        else "["^(Rational.print g)^"]["^s^"]";;
    
 let unique_standardizer pol=match pol with
  P(l)->if l=[] then Rational.one else
  let c1=List.hd(List.rev l) in
  let temp1=Image.image(fun x->(x,()) )(l) in
  let temp2=Rational.gauss_operation(temp1) in
  let c2=fst(List.hd(List.rev temp2)) in
  let c3=Rational.div(c2)(c1) in
  if Rational.is_positive(c2)
  then c3
  else Rational.opposite c3;;
 
 let print_out (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print x);
   Format.close_box();;
 
 let print_out_for_gp (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print_for_gp x);
   Format.close_box();;

let print_out_for_ocaml (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print_for_ocaml x);
   Format.close_box();;
    
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
