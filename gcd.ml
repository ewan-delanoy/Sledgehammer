let arouez x=if x<0 then -1 else if x=0 then 0 else 1;;


let rec complete_gcd0 
(aa,xa,ya,bb,xb,yb)=
(* we suppose 0<aa<bb and aa=xa*a+ya*b, bb=xb*a+yb*b *)
let q=(bb/aa) and r=(bb mod aa) in
if r=0 
then ( ((1-q)*xa)+xb,((1-q)*ya)+yb,aa)
else complete_gcd0(r,xb-(q*xa),yb-(q*ya),aa,xa,ya);;

let complete_gcd a b=
if a=0 then if b=0 then (1,1,0) else (0,arouez(b),abs(b)) else
if b=0 then (arouez(a),0,abs(a)) else
(*now a and b are nonzero*)
let aa=abs(a) and bb=abs(b) in
if aa=bb then (arouez(a),0,abs(a)) else
if aa<bb
then complete_gcd0(aa,arouez(a),0,bb,0,arouez(b))
else complete_gcd0(bb,0,arouez(b),aa,arouez(a),0);;

let find_bezout_relation a b=(function (e1,e2,e3)->(e1,e2))(complete_gcd(a)(b));;

let gcd a b=(function (e1,e2,e3)->e3)(complete_gcd(a)(b));;

let gcd_for_many=function
[]->0
|a::b->List.fold_left gcd a b;;

let lcm a b=(a*b)/(gcd a b);;

let lcm_for_many=function
[]->1
|a::b->List.fold_left lcm a b;;

