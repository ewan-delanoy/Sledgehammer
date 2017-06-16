type t=F of Polynomial.t * Polynomial.t;;

let unveil (F(x,y))=(x,y);;

let constructor a b=
  if b=Polynomial.zero then failwith("Division by zero (in rational fraction)") else
  let g=Polynomial.gcd(a)(b) in
  let better_a=Polynomial.euclid_quotient(a)(g) 
  and better_b=Polynomial.euclid_quotient(b)(g) in
  let lamb=Polynomial.unique_standardizer(better_b) in
  let final_a=Polynomial.dot(lamb)(better_a)
  and final_b=Polynomial.dot(lamb)(better_b) in
  F(final_a,final_b);;
 
let of_polynomial pol=F(pol,Polynomial.one);; 

let constant x=F(Polynomial.constant x,Polynomial.one);;
  
let zero=F(Polynomial.zero,Polynomial.one);;  
  
let one=F(Polynomial.one,Polynomial.one);;

let dot lamb (F(u,v))=if lamb=Rational.zero then zero else F(Polynomial.dot lamb u,v);;



let add (F(x1,y1)) (F(x2,y2))=
   let temp1=Polynomial.mult(x1)(y2) 
   and temp2=Polynomial.mult(x2)(y1) 
   and denom=Polynomial.mult(y1)(y2) in
   let numer=Polynomial.add(temp1)(temp2) in
   constructor numer denom;;

let sub (F(x1,y1)) (F(x2,y2))=
   let temp1=Polynomial.mult(x1)(y2) 
   and temp2=Polynomial.mult(x2)(y1) 
   and denom=Polynomial.mult(y1)(y2) in
   let numer=Polynomial.sub(temp1)(temp2) in
   constructor numer denom;;

let mult (F(x1,y1)) (F(x2,y2))=
   let temp1=Polynomial.mult(x1)(x2) 
   and temp2=Polynomial.mult(y1)(y2) in
   constructor temp1 temp2;;

let div (F(x1,y1)) (F(x2,y2))=
   let temp1=Polynomial.mult(x1)(y2) 
   and temp2=Polynomial.mult(x2)(y1) in
   constructor temp1 temp2;;

let inverse (F(x,y))=
  let lamb=Polynomial.unique_standardizer(x) in
  F(Polynomial.dot lamb y,Polynomial.dot lamb x);;

let big_dot pol (F(x,y))=
  match (constructor pol y) with
  F(a,b)->F(Polynomial.mult a x,b);;
  
  



let eval (F(u,v)) x=
  let u1=Polynomial.eval(u)(x)
  and v1=Polynomial.eval(v)(x) in
  if v1=Rational.zero
  then failwith(Rational.print(x)^" is a pole")
  else Rational.div u1 v1;;


let print (F(x,y))=
  if y=Polynomial.one
  then if x=Polynomial.zero
       then "0(the zero rational fraction)"
       else Polynomial.print(x)
  else (Polynomial.print_democratically x)^"//"^(Polynomial.print y);;


let print_out (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print x);
   Format.close_box();;
 
