(* Rat-ionals are quotients of big ints here      *)
(*The - is to impede the dependencies computer to read
a mention*)


module Big=
struct

   
   let zero=Big_int.zero_big_int;;
   let one=Big_int.unit_big_int;;
   let two=Big_int.big_int_of_int 2;;
   let minus_one=Big_int.sub_big_int zero one;;
   
   let eq=Big_int.eq_big_int;;
   let lt=Big_int.lt_big_int;;
   let gt=Big_int.gt_big_int;;

   let add=Big_int.add_big_int;;
   let sub=Big_int.sub_big_int;;
   let mult=Big_int.mult_big_int;;
   let div=Big_int.div_big_int;;
   let modd=Big_int.mod_big_int;;
   let pow=Big_int.power_big_int_positive_int;;
   let gcd=Big_int.gcd_big_int;;
   let minus=Big_int.minus_big_int;;	   
   let sign=Big_int.sign_big_int;;
   
    let of_string=Big_int.big_int_of_string;;
    let to_string=Big_int.string_of_big_int;;
	   
	let lcm x y=
	  let t1=mult(x)(y)
	  and t2=gcd(x)(y) in
	  div t1 t2;;
	  
	  
    let gcd_for_many=function
    []->zero
    |a::b->List.fold_left gcd a b;;
    
     let lcm_for_many=function
    []->one
    |a::b->List.fold_left lcm a b;;

   let p_decomposition p y=
     let rec tempf=
	 (function (j,u,v)->
	   if eq(modd(v)(p))(zero)
	   then tempf(j+1,mult(u)(p),div(v)(p))
	   else (j,u,v)
	 ) in
	 tempf(0,one,y);;
	
	

end;;

type t=Ratio of Big_int.big_int*Big_int.big_int;;

 
  let direct_frac x y=
   let g=Big.gcd(x)(y) in
   let cx=Big.div(x)(g)
   and cy=Big.div(y)(g) in
    match (Big.sign cy) with
	-1->Ratio(Big.minus cx,Big.minus cy)
	|0->failwith("division by 0")
	|z-> Ratio(cx,cy);;
 
 let string_frac sx sy=
   let x=Big.of_string(sx) and y=Big.of_string(sy) in
   direct_frac x y;;
	
  let frac x y=string_frac (string_of_int x) (string_of_int y);;	
  
 let numerator (Ratio(a,b))=Big.to_string a;;
 let denominator (Ratio(a,b))=Big.to_string b;;
	 
  let add (Ratio(a,b)) (Ratio(c,d))=
    let e1=Big.add(Big.mult(a)(d))(Big.mult(b)(c))
	and e2=Big.mult(b)(d) in
	 direct_frac e1 e2;;

  let sub (Ratio(a,b)) (Ratio(c,d))=
    let e1=Big.sub(Big.mult(a)(d))(Big.mult(b)(c))
	and e2=Big.mult(b)(d) in
	 direct_frac e1 e2;;


   let mult (Ratio(a,b)) (Ratio(c,d))=
    let e1=Big.mult(a)(c)
	and e2=Big.mult(b)(d) in
	 direct_frac e1 e2;;
  
  let div (Ratio(a,b)) (Ratio(c,d))=
    let e1=Big.mult(a)(d)
	and e2=Big.mult(b)(c) in
	 direct_frac e1 e2;;

  let pow (Ratio(a,b)) n=
    Ratio(Big.pow a n,Big.pow b n);;  
	
  let is_an_integer (Ratio(x,y))=(Big.eq y Big.one);;	 	
	
  let is_negative (Ratio(a,b))=((Big.sign a)<0);;
  let is_zero (Ratio(a,b))=(Big.sign a=0);;	
  let is_positive (Ratio(a,b))=((Big.sign a)>0);; 	
  
  
  let sign r=if is_negative(r) then -1 else 
             if is_zero(r) then 0 else 1;; 	
  
  let is_nonnegative (Ratio(a,b))=(Big.sign a>=0);;
  let is_nonpositive (Ratio(a,b))=(Big.sign a<=0);;	
	
  let minus (Ratio(a,b))=Ratio(Big.minus a,b);;
  
  let compare r1 r2=is_positive(sub r2 r1);;
  
  let maximize_snd =function
  []->failwith("nothing to maximize")
  |(x1,y1)::b->let rec tempf=
   (function
     ((x,y),z)->match z with
	 []->(x,y)
	 |(x2,y2)::peurrest->if compare(y)(y2)
	                     then tempf((x2,y2),peurrest)
						 else tempf((x,y),peurrest)
   ) in
   tempf((x1,y1),b);;
  
	
  let lt x y=is_negative(sub x y);;
  let eq x y=is_zero(sub x y);;
  let gt x y=is_positive(sub x y);; 
  let leq x y=is_nonpositive(sub x y);;
  let geq x y=is_nonnegative(sub x y);;
  let cmp=Total_ordering.from_lt lt;;
  
  
  let minus_one=Ratio(Big.minus_one,Big.one);;
  let zero=Ratio(Big.zero,Big.one);;
  let one=Ratio(Big.one,Big.one);;		 

   let opposite x=mult (minus_one) x;;

 let inverse x=div one x;;

  let opposite_of_inverse x=div minus_one x;;
		 
  let big_sum l=List.fold_left add zero l;;	
  
  let mean l=
    let n=List.length(l) in
    if n=0 then failwith("Empty mean undefined") else
    let temp1=big_sum(l) 
    and temp2=frac(1)(n) in
    mult temp1 temp2;;
 
  let to_num (Ratio(a,b))=
     let na=Num.num_of_big_int(a)
	 and nb=Num.num_of_big_int(b) in
	 Num.div_num(na)(nb);;
 
  let approx_fix j r=Num.approx_num_fix j (to_num r);;
  
  let approx r=approx_fix 15 r;;
  
  let floor (Ratio(x,y))=Ratio(Big.div(x)(y),Big.one);;
  let ceil (Ratio(x,y))=
     if Big.eq(y)(Big.one) then Ratio(x,y) else 
     Ratio(Big.add(Big.div(x)(y))(Big.one),Big.one);;
 
  let round (Ratio(a,b))=
     if Big.eq(b)(Big.one) then Ratio(a,b) else 
     let q=(Big.div(a)(b)) in
     let double_q_plus_one=Big.add(Big.one)(Big.mult Big.two q) in
     let temp1=Big.mult(Big.two)(a) 
     and temp2=Big.mult(b)(double_q_plus_one)  in
     if Big.lt(temp1)(temp2)
     then Ratio(q,Big.one)
     else Ratio(Big.add q (Big.one),Big.one);;
   
   
 
  let print_leftmost (Ratio(x,y))=
  if Big.eq y Big.one
  then if Big.eq x Big.one then "" else 
       if Big.eq x Big.minus_one then "-" else (Big.to_string x)
  else "("^(Big.to_string x)^"/"^(Big.to_string y)^")";;
 
  let print_in_between (Ratio(x,y))=
  let sx=Big.to_string(x) and sy=Big.to_string(y) in
  if Big.eq y Big.one
  then if Big.eq x Big.one then "+" else if Big.eq x Big.minus_one then "-" else
       if String.get(sx)(0)='-' then sx else "+"^sx
  else if String.get(sx)(0)='-' 
	   then "-("^(String.sub sx 1 ((String.length sx)-1))^"/"^sy^")"
	   else "+("^sx^"/"^sy^")";;
 
	   
  let print (Ratio(x,y))=
  let sx=Big.to_string(x) and sy=Big.to_string(y) in
  if sy="1"
  then sx
  else sx^"/"^sy;;
  
  let print_out (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print x);
   Format.close_box();;
	    
  let ocaml_name (Ratio(x,y))=
    "Rat"^"ional.string_frac("^(Big.to_string x)^")("^(Big.to_string y)^")";;	    
	    
	    
  let of_int x=Ratio(Big_int.big_int_of_int(x),Big.one);;
  
   let gauss_multiplier l=
     if l=[] then one else
     let l_up=List.rev_map(function Ratio(x,y)->x)(l) 
     and l_down=List.rev_map(function Ratio(x,y)->y)(l) in
     let g_up=Big.gcd_for_many(l_up)
     and g_down=Big.lcm_for_many(l_down) in
     let g=direct_frac(g_down)(g_up) in
	 g;; 
	 
	 
  
  let gauss_operation l=
     if l=[] then [] else
     let l_up=List.rev_map(function (Ratio(x,y),anv)->x)(l) 
     and l_down=List.rev_map(function (Ratio(x,y),anv)->y)(l) in
     let g_up=Big.gcd_for_many(l_up)
     and g_down=Big.lcm_for_many(l_down) in
     let g=direct_frac(g_down)(g_up) in
	 Image.image (function (r,anv)->(mult g r,anv) )(l);;  
	 
let sign_preserving_gauss_operation l=
 if l=[] then [] else
 let n=List.length(l) in
 match Option.find_it(function j->not(is_zero(fst(List.nth l j))))(Ennig.ennig 0 (n-1)) with
 None->l
 |Some(j)->let temp1=gauss_operation(l) in
              let x1=fst(List.nth(l)(j)) and y1=fst(List.nth(temp1)(j)) in
              if sign(x1)=sign(y1)
              then temp1
              else Image.image (function (x,anv)->(opposite x,anv))(temp1);;	 
	    
 let big_product l=List.fold_left mult one l;;

let sum_of_products ll=
  let temp1=List.rev_map big_product ll in
  big_sum temp1;;	    
	    
let latex_name x=
   let (a,b)=(numerator x,denominator x) in
   if b="1"
   then a
   else "\\frac{"^a^"}{"^b^"}";;
  
   
	    
	    
	    
	    
	    
	    
