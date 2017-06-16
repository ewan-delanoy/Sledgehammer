
let currified_product x y=x*y;;

let big_product=function
[]->1
|a::b->List.fold_left(currified_product)(a)(b);;


let square_free_part n=
let rec tempf=(
function 
(m,accu)->
if m=1 then accu else
let q=Basics_on_small_primes.mp(m) in 
let m2=(m/q) in
if (m2 mod q)=0
then tempf(m2/q,accu)
else tempf(m2,q*accu)
) in
if n<0 then tempf(-n,-1) else
if n>0 then tempf(n,1) else 
failwith("square-free part of zero undefined");;


let lsm1 p=
if (p mod 4)=1
then 1
else (-1);;

let ls2 p=
if Ordered_integer.elfenn(p mod 8)(Ordered_integer.safe_set[1;7])
then 1
else (-1);;

let reciprocity_coefficient p q=
if ((p mod 4)=1)|| ((q mod 4)=1) then 1 else (-1);;

let rec naive_legendre_symbol0 accu=
function
[]->accu
|(a,p)::peurrest->(*we take p>a>0, p odd *)
if (a mod p)=0 then 0 else
if a=1 then naive_legendre_symbol0(accu)(peurrest) else
if (a mod 2)=0 then naive_legendre_symbol0(ls2(p)*accu)(((a/2),p)::peurrest) else
let q=Basics_on_small_primes.mp(a) in
let epsilon=reciprocity_coefficient(p)(q) in
naive_legendre_symbol0(epsilon*accu)(((p mod q),q)::(a/q,p)::peurrest);;

let naive_legendre_symbol a p=
if (a mod p)=0 then 0 else
if a<0
then lsm1(p)*naive_legendre_symbol0(1)([(-a) mod p,p])
else naive_legendre_symbol0(1)([a mod p,p]);;

let phi_set nn=
let n=abs(nn) in
let temp1=Basics_on_small_primes.multiset_factorization(n) in
let temp2=Multiset.to_list(temp1) in
let temp3=Ennig.ennig(1)(n-1) in
List.filter(function x->List.filter(function p->(x mod p)=0)(temp2)=[])(temp3);;

let find_one_prime_equivalent m a=
let rec tempf=(function
accu->if Basics_on_small_primes.is_prime(accu) then (a,accu) else tempf(accu+m))
in if a<3 then tempf(a+m) else tempf(a);;

let find_many_prime_equivalents m l=
Image.image(find_one_prime_equivalent(m))(l);;

let good_moduli a=
let aa=abs(a) and ea=Gcd.arouez(a) in
let temp1=Basics_on_small_primes.multiset_factorization(aa) in
let temp2=Multiset.filter_odd_multiplicities(temp1) in
let ka=ea*big_product(temp2) in
let big_a=4*abs(ka) in
let temp3=Ennig.ennig(1)(big_a-1) in
let temp4=List.filter(function x->List.filter(function p->(x mod p)=0)(2::temp2)=[])(temp3) in
let temp5=find_many_prime_equivalents(big_a)(temp4) in
let temp6=List.filter(function (z,pz)->naive_legendre_symbol(a)(pz)=1)(temp5) in
(big_a,temp6);;

let gcd_for_three a b c=Arithmetic.gcd(a)(Arithmetic.gcd(b)(c));;

let enumerate_reduced_forms dd=
let r=(dd mod 4) in
if ((r=1)||(r=2)) then ([],[]) else
let temp1=Ennig.ennig(0)(Arithmetic.isqrt(dd/3)) in
let temp2=List.filter(function x->(x mod 2)=r)(temp1) in
let temp3=List.flatten(Image.image(function b->let m=(b*b+dd)/4 in
Image.image(function a->(a,b,m/a) )(Ordered_integer.forget_order(Basics_on_small_primes.naive_list_of_divisors(m))) )(temp2)) in
let temp4=List.filter(function (a,b,c)->(b<=a)&&(a<=c)&&(gcd_for_three(a)(b)(c)=1 ) )(temp3) in
let temp5=List.filter(function (a,b,c)->(0<b)&&(b<a)&&(a<c) )(temp4) in
(temp4,Image.image(function (a,b,c)->(a,-b,c))(temp5));;

let number_of_reduced_forms dd=
let temp=enumerate_reduced_forms(dd) in
List.length(fst(temp))+List.length(snd(temp));;





