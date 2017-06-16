
(* *)

type t=Dec of ((Big_int.big_int)*int);;

let big_zero=Big_int.big_int_of_int 0;;
let big_one=Big_int.big_int_of_int 1;;
let big_ten=Big_int.big_int_of_int 10;;
let big_hundred=Big_int.big_int_of_int 100;;
let power_of_ten j=Big_int.power_big_int_positive_int big_ten j;;
let mpower_of_ten j x=Big_int.mult_big_int (power_of_ten j) x;;

let zero=Dec(big_zero,0);;
let one=Dec(big_one,0);;

let dec (x,j)=
   if Big_int.eq_big_int(x)(big_zero)
   then zero
   else let rec tempf=(fun
           (y,k)->
         			    if k<1 then (y,0) else
           				let (q,r)=Big_int.quomod_big_int(y)(big_ten) in
           				if Big_int.eq_big_int(r)(big_zero)
           				then tempf(q,k-1)
           				else (y,k)
        ) in
        Dec(tempf(x,j));;

let add (Dec(x1,j1)) (Dec(x2,j2))=
  if j1<j2
  then let y1=mpower_of_ten(j2-j1)(x1) in
       dec(Big_int.add_big_int y1 x2,j2)
  else let y2=mpower_of_ten(j1-j2)(x2) in
       dec(Big_int.add_big_int x1 y2,j1);;     
       
let sub (Dec(x1,j1)) (Dec(x2,j2))=
  if j1<j2
  then let y1=mpower_of_ten(j2-j1)(x1) in
       dec(Big_int.sub_big_int y1 x2,j2)
  else let y2=mpower_of_ten(j1-j2)(x2) in
       dec(Big_int.sub_big_int x1 y2,j1);;     
               
let mult (Dec(x1,j1)) (Dec(x2,j2))=Dec(Big_int.mult_big_int x1 x2,j1+j2);;
 
type positive_int=int;; 
 
let pow (Dec(x1,j1)) (n:positive_int)=dec(Big_int.power_big_int_positive_int x1 n,j1*n);; 


let power_of_ten j=
   if j>=0 
   then Dec(Big_int.power_big_int_positive_int big_ten j,0)
   else Dec(big_one,-j);;

let is_positive (Dec(x1,j1))=Big_int.lt_big_int big_zero x1;;
let is_negative (Dec(x1,j1))=Big_int.lt_big_int x1 big_zero;;

let is_nonpositive (Dec(x1,j1))=Big_int.ge_big_int big_zero x1;;
let is_nonnegative (Dec(x1,j1))=Big_int.ge_big_int x1 big_zero;;

let abs d=if is_negative d then sub zero d else d;;

let is_zero (Dec(x1,j1))=Big_int.eq_big_int(x1)(big_zero);;

let lt d1 d2=is_positive(sub d2 d1);;
let gt d1 d2=is_negative(sub d2 d1);;
let eq d1 d2=is_zero(sub d2 d1);;


let leq d1 d2=is_nonnegative(sub d2 d1);;
let geq d1 d2=is_nonpositive(sub d2 d1);;

let cmp=Total_ordering.from_lt lt;;

type precision=int;;

let of_int a=Dec(Big_int.big_int_of_int a,0);;
let of_rational (k:precision) r=
   let s1="1"^(String.make k '0') in
   let temp1=Rational.string_frac(s1)("1") in
   let temp2=Rational.mult(temp1)(r) in
   let temp3=Rational.floor(temp2) in
   let temp4=Rational.numerator(temp3) in
   let temp5=Big_int.big_int_of_string(temp4) in
   dec(temp5,k);;

let of_float (k:precision) x=
 if (k>8) then failwith("Sorry, the precision is at most 8") else  
 let temp1=(10.**(float_of_int (k+1)))*.x in
 let old_temp2=int_of_float(floor(temp1)) in
 let temp2=(fun w->let a=(w/10) and b=(w mod 10) in
   if b<5 then a else a+1)(old_temp2) in
 let temp3=Big_int.big_int_of_int(temp2) in
 dec(temp3,k);;

let adhoc_index s c=try (String.index s c) with Not_found->(-1);;

let of_string_with_no_fullstops s=dec(Big_int.big_int_of_string s,0);;
   
let of_string wild_s=
  let s=Str.global_replace(Str.regexp(" "))("")(wild_s) in
  let j=adhoc_index(s)('.') in
  if (j=(-1)) 
  then of_string_with_no_fullstops s
  else let before=Cull_string.beginning(j)(s)
       and after=Cull_string.cobeginning(j+1)(s) in
       let n=String.length(after) in
       if n=0 then of_string_with_no_fullstops before else
       let s_without_fullstops=before^after in
       dec(Big_int.big_int_of_string s_without_fullstops,n);;

let to_rational (Dec(x,y))= 
   let s="1"^(String.make y '0') in
   Rational.string_frac (Big_int.string_of_big_int x) s;;

let to_string_in_positive_case sx y=
         let d=y-(String.length(sx)) in
         if d>=0
         then "0."^(String.make d '0')^sx
         else 
         if y=0 then sx else
         let before=Cull_string.coending(y)(sx)
         and after=Cull_string.ending(y)(sx) in
         before^"."^after;;

let to_string_in_negative_case sx y=
  (* we know that x is negative, so sx starts with a - *)
   let tsx=Cull_string.cobeginning 1 sx in
   "-"^(to_string_in_positive_case tsx y);;

let to_string (Dec(x,y))=
   let sx=Big_int.string_of_big_int(x) in
   if String.get(sx)(0)='-'
   then to_string_in_negative_case sx y
   else to_string_in_positive_case sx y;;
       
       
   
let floor d=of_rational(0)(Rational.floor(to_rational(d)));;  
let ceil d=of_rational(0)(Rational.ceil(to_rational(d)));;  
let nearest_integer d=of_rational(0)(Rational.nearest_integer(to_rational(d)));;  
let big_sum l=List.fold_left add zero l;;

let to_int d=int_of_string(Rational.numerator(to_rational (nearest_integer d)));;

(*
let frac_floor x y=
  let rx=to_rational(x)
  and ry=to_rational(y) in
  let rxy=Rational.div(rx)(ry) in
  let floor_rxy=Rational.floor(
  
  to_int(ceil d);;

let frac_ceil x y=to_int(ceil d);;

*)




let general_floor_transform (j:precision) d=
  match d with (Dec(x,y))->
    if y<=j then d else
    match (floor(Dec(x,y-j))) with
    Dec(u,v)->dec(u,v+j);;

let general_rounded_transform (j:precision) d=
  match d with (Dec(x,y))->
    if y<=j then d else
    match (nearest_integer(Dec(x,y-j))) with
    Dec(u,v)->dec(u,v+j);;

let to_float (Dec(x,y))=
   let temp1=Rational.floor(to_rational( dec(mpower_of_ten 2 x,y)  )) in
   let temp2=Big_int.big_int_of_string(Rational.numerator(temp1)) in
   let temp3=float_of_int(Big_int.int_of_big_int(temp2)) in
   temp3/.100.;;
   

let isqrt (r:Rational.t)=
  let temp1=Rational.floor(r) in
  let temp2=Rational.numerator(temp1) in
  let temp3=Big_int.big_int_of_string(temp2) in
  let p=Big_int.sqrt_big_int(temp3) in
  let p_plus_one=Big_int.add_big_int(p)(big_one) in
  let temp4=Big_int.square_big_int(p_plus_one) in
  let temp5=Big_int.string_of_big_int(temp4) in
  let temp6=Rational.string_frac(temp5)("1") in
  if Rational.lt(temp6)(r)
  then p
  else p_plus_one;;

let precise_sqrt (j:precision) r=
  let pt=Rational.string_frac("1"^(String.make (2*j) '0'))("1") in
  let new_r=Rational.mult(pt)(r) in
  let a=isqrt(new_r) in
  dec(a,j);;
  
let sqrt d=precise_sqrt 22 (to_rational d);;
  
let precise_inverse (j:precision) r=
  let temp1=Rational.inverse(r) in
  of_rational j temp1;;
  
let inverse d=precise_inverse 22 (to_rational d);;  
  

(*
let sqrt (j:precision) (Dec(x,y))=
  let (x1,y1)=(function d->
    if d>=0 then (mpower_of_ten(d)(x),0) else (x,-d)
  )((2*j)-y) in
  let (x2,y2)=(function ()->
     if (y1 mod 2)=0 then (x1,y1) else (mpower_of_ten 1 x1,y1+1)
  )() in
  dec(Big_int.sqrt_big_int x2,(y2/2)+j);;
 *) 

let rounded_transform d=general_rounded_transform 2 d;;

type interval_step=t;;
type interval_starting_point=t;;
type interval_bound_for_endpoint=t;;

let subdivized_interval 
  (a:interval_starting_point) 
  (b:interval_bound_for_endpoint) 
  (eps:interval_step)=
   let rec tempf=(fun
    (graet,x)->
      if lt b x then List.rev(graet) else
      tempf(x::graet,add eps x)
   ) in
   tempf([],a);;


let rational_mean l=
  let n=List.length(l) in
  if n=0 then failwith("Empty decimal mean undefined") else
  Rational.div(to_rational(big_sum(l)))(Rational.of_int n);;

let mean l=rounded_transform(of_rational 3(rational_mean l));;  

let standard_mean l=
   let temp1=Image.image(to_rational)(l) in
   let temp2=Image.image(fun r->Rational.mult r r)(temp1) in
   let m1=Rational.mean(temp1) in
   let m2=Rational.mean(temp2) 
   and m1_squared=Rational.mult(m1)(m1) in
   let sigma_squared=Rational.sub(m2)(m1_squared) in
   rounded_transform(precise_sqrt 3 sigma_squared);;

let rec multiset_merge (x,y,accu)=
  if x=[] then List.rev_append accu y else
  if y=[] then List.rev_append accu x else
  let ax=List.hd(x) and bx=List.tl(x)
  and ay=List.hd(y) and by=List.tl(y) in
  if lt(ay)(ax)
  then multiset_merge (x,by,ay::accu)
  else multiset_merge (bx,y,ax::accu);;

let rec multiset_sort l=
   if List.length(l)<2 then l else
   let (l1,l2)=Listennou.didrochan(l) in
   let m1=multiset_sort(l1)
   and m2=multiset_sort(l2) in
   multiset_merge (m1,m2,[]);;
   
let quartiles_and_extremities old_l=
  let l=multiset_sort(old_l) in
  let n=List.length(l)
  and f=(fun i->List.nth(l)(i-1)) in
  let x=(n/2) in
  let i_q1=Basic.frac_ceiling(n)(4) 
  and i_q3=Basic.frac_ceiling(3*n)(4)
  and m=(fun r->if r=0 
    then mean([(f x);(f (x+1))])
    else  f (x+1)
  )(n mod 2) in
  ( f 1, f i_q1, m, f i_q3, f n);;
 

let ugly_mean l=  
   let temp1=Image.image(fun (rat,lamb)->
      Rational.mult(rat)(to_rational lamb)
   )(l)
   and temp2=Image.image(fst)(l) in
   let r1=Rational.big_sum(temp1)
   and r2=Rational.big_sum(temp2) in
   if Rational.is_zero(r2) then failwith("Ill-defined barycenter") else
   let r=Rational.div(r1)(r2) in
   rounded_transform(of_rational 3 r);;  


let naem_constant=ref(1);;
let naem_rat_constant ()=Rational.of_int(! naem_constant);;
let get_naem_constant ()=(!naem_constant);;
let set_naem_constant x=(naem_constant:=x);;

let ugly_naem y0 l=  
   let r0=to_rational(y0) in
   let temp1=Image.image(fun (rat,lamb)->
      let ttemp1=Rational.mult(rat)(Rational.sub(r0)(to_rational lamb)) in
      Rational.div(ttemp1)(naem_rat_constant())
   )(l) in
   let r=Rational.big_sum(r0::temp1) in
   rounded_transform(of_rational 3 r);; 
   

   
 let find_integer_in_between x y=
   let i1=Rational.ceil(x)
   and i2=Rational.floor(y) in
   if Rational.leq(i1)(i2)
   then Some(i1)
   else None;;
   
 let find_large_integer_in_between x y=
   let i1=Rational.ceil(x)
   and i2=Rational.floor(y) in
   if Rational.leq(i1)(i2)
   then Some(i2)
   else None;;  
   
 let rational_ten=Rational.of_int 10;;  
 let rat_multiply_by_ten=Rational.mult rational_ten;;
   
 let find_decimal_in_between a b=
   if Rational.gt(a)(b) then failwith("Impossible") else
   let rec tempf=(function
    (j,x,y)->
       match find_integer_in_between x y with
       None->tempf(j+1,rat_multiply_by_ten x,rat_multiply_by_ten y)
       |Some(i)->
         let b=Big_int.big_int_of_string(Rational.numerator i) in
         dec(b,j)
   ) in
   tempf(0,a,b);;
   
 let find_large_decimal_in_between a b=
   if Rational.gt(a)(b) then failwith("Impossible") else
   let rec tempf=(function
    (j,x,y)->
       match find_large_integer_in_between x y with
       None->tempf(j+1,rat_multiply_by_ten x,rat_multiply_by_ten y)
       |Some(i)->
         let b=Big_int.big_int_of_string(Rational.numerator i) in
         dec(b,j)
   ) in
   tempf(0,a,b);;  
   
 let interval a b h=
   let rec tempf=(fun
     (graet,x)->if gt(x)(b) 
     then List.rev(graet)
     else tempf(x::graet,add x h)
   ) in
   tempf([],a);;
   
let int_interval i j h=interval (of_int i) (of_int j) h;;


let evaluate_polynomial0 paul w=
  let temp1=Image.image(of_rational 12)(Polynomial.unveil paul) in
  let rec tempf=(function
  		(l,wj,accu)->match l with
  		[]->accu
  		|aj::peurrest->let mj=mult(aj)(wj) in
                let new_accu=add(mj)(accu) in
                tempf(peurrest,mult w wj,new_accu)) in
 tempf (temp1,one,zero);;               
                


let evaluate_polynomial paul w=
  let inverse_of_lamb=Polynomial.unique_standardizer(paul) in
  let lamb=Rational.inverse(inverse_of_lamb) in
  let better_paul=Polynomial.dot(inverse_of_lamb)(paul) in
  let d_lamb=of_rational 12 lamb in
  mult d_lamb (evaluate_polynomial0 better_paul w);;
  
  

 let max x y=if lt(x)(y) then y else x;;  

 let print_out (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(to_string x);
   Format.close_box();;
 
