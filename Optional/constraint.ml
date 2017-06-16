type t={whole:Vector.t;pos_part:Vector.t;neg_part:Vector.t};;

let unveil (x:t)=(x.whole,x.pos_part,x.neg_part);;

let of_vector old_v=
   let v=Vector.sign_preserving_gauss_operation(old_v) in
   let sep=List.partition(function (c,anv)->Rational.is_negative c)(Vector.unveil v) in
   let v1=Vector.unsafe_vector(snd sep)
   and v2=(Vector.dot Rational.minus_one   (Vector.unsafe_vector(fst sep))) in
   {whole=v;pos_part=v1;neg_part=v2};;

let to_vector (x:t)=x.whole;;



let to_relation cst=Relation.of_vector (cst.whole);;

let eval cst v=Vector.scalar_product cst.whole v;;

let check phi v=Rational.is_nonnegative(eval phi v);;

let is_in_pointed_cone w l_phi=List.for_all
(function phi->check phi w)(l_phi);;

let test_for_extremality v l_phi=
 let temp1=List.rev_map(function phi->(phi,eval phi v))(l_phi) in
 if List.exists(function (phi,w)->Rational.is_negative w)(temp1)
 then false
 else let temp2=List.filter(function (phi,w)->Rational.is_zero w)(temp1) in
      let temp3=List.rev_map(function (cst,y)->to_relation cst)(temp2) in
      let ds=Descriptive_system.make(temp3) 
      and supp=Ordered.forget_order(Vector.support(v)) in
      Descriptive_system.the_solution_is_a_line ds supp;;

let dual_test_for_extremality phi l_v=
 let temp1=List.rev_map(function v->(v,eval phi v))(l_v) in
 if List.exists(function (v,w)->Rational.is_negative w)(temp1)
 then false
 else let temp2=List.filter(function (v,w)->Rational.is_zero w)(temp1) in
      let temp3=List.rev_map(function (v,w)->Relation.of_vector v)(temp2) in
      let ds=Descriptive_system.make(temp3) 
      and supp=Ordered.forget_order(Vector.support(phi.whole)) in
      Descriptive_system.the_solution_is_a_line ds supp;; 
 

let of_list l=of_vector(Vector.sum_of_variables l);;
let of_dlist l1 l2=of_vector(Vector.of_dlist l1 l2);;

let print cst=
  let s1=Vector.print(cst.pos_part)
  and s2=Vector.print(cst.neg_part) in
   s1^" >= "^s2;;

let print_and_go cst=print_string(print cst);;  

 let print_out (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print x);
   Format.close_box();;
   
let cmp=
 ((fun cst1 cst2->
 let u1=Vector.cmp(cst1.pos_part)(cst2.pos_part) in
 if u1<>Total_ordering.Equal then u1 else
 Vector.cmp(cst1.neg_part)(cst2.neg_part) ): t Total_ordering.t);;
