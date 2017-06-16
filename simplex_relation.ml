(*this is a short module that is used in the simplex method implementation. *)


type t={main_vector:Vector.t; adjuster:Variable.t ; bounding_constant:Rational.t};;

(* {main_vector=v,adjuster=ad,bounding_constant=mm} represents the relation

    v+ad=mm
*)

let unsafe_constructor x y z=
 {main_vector=x;adjuster=y;bounding_constant=z};;
 
let constructor x y z=
  let g=Vector.coeff(x)(Variable.dummy) in
  if Rational.eq(g)(Rational.zero)
  then unsafe_constructor x y z
  else let vg=Vector.of_rational(g) in
       unsafe_constructor (Vector.sub x vg) y (Rational.sub z g);;
 
let vector_part sr=sr.main_vector;; 
 
let adjuster_part sr=sr.adjuster;;

let constant_part sr=sr.bounding_constant;;

let print sr=
 let s1=Variable.unveil(sr.adjuster)
 and s2="+{"^(Vector.print(sr.main_vector))^"}="
 and s3=Rational.print (sr.bounding_constant) in
 s1^s2^s3;;

let eq sr1 sr2=
 if Rational.eq(sr1.bounding_constant)(sr2.bounding_constant)
 then if Variable.eq(sr1.adjuster)(sr2.adjuster)
      then Vector.eq(sr1.main_vector)(sr2.main_vector)
      else false
 else false;;

let subs simplex_rel var vec=
  let temp1=Vector.subs(simplex_rel.main_vector)(var)(vec) in
  let c0=Vector.coeff(temp1)(Variable.dummy) in
  let c1=Vector.lonely(c0)(Variable.dummy) in
  let temp2=Vector.sub(temp1)(c1) in
  let new_m=Rational.sub(simplex_rel.bounding_constant)(c0) in
  if Rational.is_negative(new_m)
  then failwith("You crossed the line! See "^print(simplex_rel))
  else 
  {main_vector=temp2;adjuster=simplex_rel.adjuster;bounding_constant=new_m};;
  
let fold_subs simplex_rel var_vec=
  let temp1=Vector.fold_subs(var_vec)(simplex_rel.main_vector) in
  let c0=Vector.coeff(temp1)(Variable.dummy) in
  let c1=Vector.lonely(c0)(Variable.dummy) in
  let temp2=Vector.sub(temp1)(c1) in
  let new_m=Rational.sub(simplex_rel.bounding_constant)(c0) in
  if Rational.is_negative(new_m)
  then failwith("You crossed the line! See "^print(simplex_rel))
  else 
  {main_vector=temp2;adjuster=simplex_rel.adjuster;bounding_constant=new_m};;
 
  
let is_critical simplex_rel =
(Rational.eq simplex_rel.bounding_constant Rational.zero);;

let ratio simplex_rel var=
 let r1=Vector.coeff(simplex_rel.main_vector)(var) 
 and r2=simplex_rel.bounding_constant in
 if (Rational.eq r1 Rational.zero)||(Rational.eq r2 Rational.zero) 
 then Rational.minus_one (*this is just a convenience *)
 else Rational.div r1 r2;;
  
let coeff sr x=Vector.coeff (sr.main_vector) x;;  
  
let change_adjusters simplex_rel new_adjuster=
  let vec=simplex_rel.main_vector in
  let c0=Vector.coeff(simplex_rel.main_vector)(new_adjuster) in
  if (Rational.is_nonpositive c0)
  then failwith("bad adjuster")
  else 
      let c1=Rational.div(Rational.one)(c0) in
      let vec1=Vector.lonely(c0)(new_adjuster) in
      let vec2=Vector.sub(vec)(vec1) in
      let vec3=Vector.dot(c1)(vec2) in
      let vec4=Vector.lonely(c1)(simplex_rel.adjuster) in
      let vec5=Vector.add(vec3)(vec4) in
      let new_m=Rational.mult(c1)(simplex_rel.bounding_constant) in
      {main_vector=vec5;adjuster=new_adjuster;bounding_constant=new_m}
  
  ;;
  
 let to_vector simplex_rel=
  let vec1=Vector.sum_of_variables([simplex_rel.adjuster]) in
  let vec2=Vector.add(simplex_rel.main_vector)(vec1)
  and vec3=Vector.lonely(simplex_rel.bounding_constant)(Variable.dummy) in
  Vector.sub vec3 vec2;;
  
 let to_relation simplex_rel=Relation.of_vector(to_vector simplex_rel);;  
 
 let is_superfluous simplex_rel=
  let temp1=Vector.unveil(simplex_rel.main_vector) in
  List.for_all(function (lambda,x)->Rational.is_nonpositive lambda)(temp1);;
  
 let is_a_gift simplex_rel=
  if is_critical(simplex_rel)
  then let temp1=Vector.unveil(simplex_rel.main_vector) in
       List.for_all(function (lambda,x)->Rational.is_nonnegative lambda)(temp1)
  else false;;     
  
type var_set=Variable.t Tidel.set;;  
  
 let possible_adjusters x=
  let temp1=Vector.unveil(x.main_vector) in
  let temp2=List.filter(function (lambda,var)->Rational.is_positive lambda)(temp1) in
  let temp3=List.rev_map(snd)(temp2) in
  (Tidel.diforchan(x.adjuster::temp3):var_set);;
  
 let change_adjusters_carefully simplex_rel an_adjuster=
  if an_adjuster=simplex_rel.adjuster
  then simplex_rel
  else change_adjusters simplex_rel an_adjuster;;
  
 
 let print_out (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print x);
   Format.close_box();;
 
