let minimize paul a b=Rational.minimize (Polynomial.eval_at_int paul) (Ennig.ennig a b);;
let maximize paul a b=Rational.maximize (Polynomial.eval_at_int paul) (Ennig.ennig a b);;


type right_or_left=Left of int*int |Right of int*int;;

let length_of_sided_interval=function
Left(a,b)->b-a
|Right(a,b)->b-a;;

let distinguished_extremity_of_sided_interval=function
Left(a,b)->a
|Right(a,b)->b;;


let rec minimize_and_explain paul a b=
  let (x0,y0)=minimize paul a b in
  if (Rational.eq y0 (Polynomial.eval_at_int paul a)) 
  then let d=Polynomial.short_int_constructor [-a;1] in
       [(Left (a,b),Polynomial.euclid_quotient paul d)]
  else
  if (Rational.eq y0 (Polynomial.eval_at_int paul b)) 
  then let d=Polynomial.short_int_constructor [-b;1] in
       [(Right (a,b),Polynomial.euclid_quotient paul d)]
  else 
  let temp1=minimize_and_explain paul a x0
  and temp2=minimize_and_explain paul x0 b in
  temp1@temp2;;
  
 let rec maximize_and_explain paul a b=
  let peter=Polynomial.dot Rational.minus_one paul in
  let (x0,y0)=maximize paul a b in
  if (Rational.eq y0 (Polynomial.eval_at_int paul a)) 
  then let d=Polynomial.short_int_constructor [-a;1] in
       [(Left (a,b),Polynomial.euclid_quotient peter d)]
  else
  if (Rational.eq y0 (Polynomial.eval_at_int paul b)) 
  then let d=Polynomial.short_int_constructor [-b;1] in
       [(Right (a,b),Polynomial.euclid_quotient peter d)]
  else 
  let temp1=maximize_and_explain paul a x0
  and temp2=maximize_and_explain paul x0 b in
  temp1@temp2;;
   
 let rec minimize_and_prove paul a b=
   let temp1=minimize_and_explain paul a b in
   let tempf=(fun
      (loro,andrew)->
        let (a0,b0,e0)=(function Left(x,y)->(x,y,x) |Right(x,y)->(x,y,y)) (loro) in
        if (Polynomial.degree(andrew)<2)||(b0-a0<2)
        then [(a0,b0),[e0],andrew]
        else 
        let ttemp1=minimize_and_prove(andrew)(a0)(b0) in
        let ttemp2=Image.image (fun 
          (itvl,chain,matthew)->(itvl,e0::chain,matthew)
        ) ttemp1 in
        ttemp2
   ) in
   let temp2=Image.image tempf temp1 in
   List.flatten temp2;;
  
  let maximize_and_prove paul a b=
   let temp1=maximize_and_explain paul a b in
   let tempf=(fun
      (loro,andrew)->
        let (a0,b0,e0)=(function Left(x,y)->(x,y,x) |Right(x,y)->(x,y,y)) (loro) in
        if (Polynomial.degree(andrew)<2)||((length_of_sided_interval loro)<2)
        then [(a0,b0),[e0],andrew]
        else 
        let ttemp1=minimize_and_prove(andrew)(a0)(b0) in
        let ttemp2=Image.image (fun 
          (itvl,chain,matthew)->(itvl,e0::chain,matthew)
        ) ttemp1 in
        ttemp2
   ) in
   let temp2=Image.image tempf temp1 in
   List.flatten temp2;; 
  
  
  (*
  
  module Paul=Polynomial;;
  
  
  let z1=Polynomial.short_int_constructor [3;4;5];;
  
  *)
