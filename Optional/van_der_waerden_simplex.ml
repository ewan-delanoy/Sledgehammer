let drummer l=
  if l=[] then Constraint.of_vector(Vector.of_variable (Variable.of_string "y")) else
  if List.length(l)=1 
  then let x=Variable.of_string("x"^(string_of_int (List.hd l))) in
       Constraint.of_vector(Vector.of_variable x)
  else
  let temp1=Image.image (function i->Variable.of_string("x"^(string_of_int i)) ) l in
  let v1=Vector.sum_of_variables(temp1) in
  let n=List.length(l)-1 in
  let v2=Vector.lonely(Rational.of_int(n))(Variable.of_string("y")) in
  Constraint.of_vector(Vector.sub(v1)(v2));;
  
 let for_the_uniform_vector n=
  let temp1=Ennig.doyle(function j->Variable.of_string("x"^(string_of_int j)) )(1)(n) in
  (Variable.of_string "y")::temp1;;
  
 let uniform_vector n=
  let temp1=Ennig.doyle(function j->(Rational.one,Variable.of_string("x"^(string_of_int j))) )(1)(n) in
  Vector.safe_vector((Rational.one,Variable.of_string("y"))::temp1);;
   
 let x_vector lx=
  let temp1=Image.image(function (c,i)->(Rational.of_int(c),Variable.of_string("x"^(string_of_int i)))   )(lx) in
  Vector.safe_vector temp1;;
   
let almost_uniform_vector n cu cy lcx=
   let v1=Vector.dot (Rational.of_int(cu)) (uniform_vector n)
   and v2=Vector.dot (Rational.of_int(cy)) (Vector.of_variable (Variable.of_string "y"))
   and v3=x_vector(lcx) in
   let v23=Vector.add(v2)(v3) in
   Vector.add v1 v23;;
   
 let almost_uniform_reconstructor n v=
   let l=Vector.unveil(v) in
   let temp1=Rational.zero::(Tidel.forget_order(Tidel.diforchan(List.rev_map(fst)(l)))) in
   let temp2=Max.maximize_it(function j->
      if j=Rational.zero then (n+1-List.length(l)) else
      List.length(List.filter (function re->fst(re)=j) l) )(temp1) in
   let u_coordinate=(fst temp2) in
   let v1=Vector.dot(u_coordinate)(uniform_vector n)
   and v2=Vector.dot(u_coordinate)(Vector.of_variable (Variable.of_string "u")) in
   let v3=Vector.sub(v)(v1) in
   Vector.add v2 v3;;
   
