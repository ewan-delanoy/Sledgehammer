
(*

#use"basics_on_small_primes.ml";;

*)


type int_multiset = int Multiset.t;;
type ii= (int,int) Memoized.map;;
type ib= (int, bool) Memoized.map;;

let list_of_small_primes=List_of_small_primes.list_of_small_primes;;


let mp=((Memoized.make(fun n->
  match Option.find_it(fun p->n mod p=0)(list_of_small_primes) with
  None->n
  |Some(p)->p
)):ii);;



let is_prime n=(mp(n)=n);;

let next_prime n=
  let rec tempf=(fun x->
    if is_prime x
    then  x
    else tempf(x+1)
  ) in
  tempf(n+1);;

let power=Easy_arithmetic.power;;

let rec naive_list_of_divisors=function
1->Ordered_integer.singleton(1)
|n->
let p=mp(n) in
let m=(n/p) in
let temp1=naive_list_of_divisors(m) in
let temp2=Ordered_integer.image(function x->p*x)(temp1) in 
Ordered_integer.teuzin(temp1)(Ordered_integer.unsafe_set(temp2));;

let naive_factorization n=
let rec sub_f=
(function
(i,accu)->if i=1 then List.rev(accu) else
let p=mp(i) in
sub_f(i/p,p::accu)) in
sub_f(n,[]);;

let p_decomposition p n=
let rec sub_f=(function
(m,j,accu)->if (m mod p)=0 then sub_f(m/p,j+1,p*accu) else (j,accu)
) in sub_f(n,0,1);;

let multiset_factorization n=
let rec sub_f=
(function
(i,accu)->if i=1 then Multiset.M(List.rev(accu)) else
let p=mp(i) in
let temp=p_decomposition(p)(i) in
let a=fst(temp) and b=snd(temp) in
sub_f(i/b,(p,a)::accu)) in
(sub_f(n,[]):int_multiset);;

let is_square_free=((Memoized.make(function n->
match multiset_factorization(n) with
 Multiset.M(l)->List.for_all(function c->snd(c)=1)(l)
)):ib);;

let compute_factorized_term l=
   List.fold_left ( * ) 1 (Image.image (fun (p,a)->power p a) l);;

let list_of_divisors=Memoized.make(fun n->
  let (Multiset.M(temp1))=multiset_factorization n in
  let temp2=Image.image (fun (a,ea)->Ennig.doyle (fun j->(a,j)) 0 ea ) temp1 in
  let temp3=Cartesian.general_product temp2 in
  let temp4=Image.image compute_factorized_term temp3 in
  let temp5=Ordered_integer.diforchan temp4 in
  Ordered_integer.forget_order temp5
);;


let nf=naive_factorization;;
let mf=multiset_factorization;;



