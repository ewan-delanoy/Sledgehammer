


let find_bezout_relation=Gcd.find_bezout_relation;;
let gcd=Gcd.gcd;;
let gcd_for_many=Gcd.gcd_for_many;;
let lcm=Gcd.lcm;;
let lcm_for_many=Gcd.lcm_for_many;;


let dog_remainder x y=(*we take y>0 *)
if x>0 then (x mod y)
else y+(x mod y);;


let inv a b=(*we take a>0,b>0 *)
let temp=find_bezout_relation(a)(b) in
dog_remainder(fst(temp))(b);;

let list_of_squares_up_to n=
let rec tempf=
(function
(j2,j,accu)->if (j2>n) then List.rev(accu) else
tempf(j2+2*j+1,j+1,(j,j2)::accu)
) in
tempf(0,0,[]);;

let isqrt n=
if n<0 then failwith("negative integers not allowed") else
if n<2 then n else 
let rec tempf=
(function
(j2,j)->if (j2>n) then (j-1) else
tempf(j2+2*j+1,j+1)
) in
tempf(1,1);;

let modular_square_root z n=
Option.find_it(function j->((j*j-z) mod n)=0)(Ennig.ennig(0)(n-1));;

let p_partition p n=
if n=0 then failwith("infinite exponent") else
let rec tempf=(function
(i,pi,da_ober)->
if (da_ober mod p)=0 
then tempf(i+1,pi*p,da_ober/p)
else (i,pi,da_ober)
) in
tempf(0,1,n);;


		   
		   


