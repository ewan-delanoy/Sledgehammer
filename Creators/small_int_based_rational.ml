(* Rat-ionals are quotients of ordinary ints here *)
(*The - is to impede the dependencies computer to read
a mention*)

let sign_of_int x=
  if (x<0)
  then (-1)
  else if (x=0)
       then 0
       else 1;;

type t=Ratio of int*int;;
 
 let numerator (Ratio(a,b))=string_of_int a;;
 let denominator (Ratio(a,b))=string_of_int b;;
 
 let frac x y=
   let g=Gcd.gcd(x)(y) in
   let cx=(x/g)
   and cy=(y/g) in
    match (sign_of_int cy) with
	-1->Ratio(-cx,-cy)
	|0->failwith("division by 0")
	|z-> Ratio(cx,cy);;
	 
 let string_frac x y=frac (int_of_string x) (int_of_string y);;	 
	 
  let add (Ratio(a,b)) (Ratio(c,d))=
    let e1=(a*d)+(b*c)
	and e2=(b*d) in
	 frac e1 e2;;

  let sub (Ratio(a,b)) (Ratio(c,d))=
    let e1=(a*d)-(b*c)
	and e2=(b*d) in
	 frac e1 e2;;


   let mult (Ratio(a,b)) (Ratio(c,d))=
    let e1=(a*c)
	and e2=(b*d) in
	 frac e1 e2;;
  
  let div (Ratio(a,b)) (Ratio(c,d))=
    let e1=(a*d)
	and e2=(b*c) in
	 frac e1 e2;;

  let pow (Ratio(a,b)) n=
    Ratio(Easy_arithmetic.power a n,Easy_arithmetic.power b n);;  
	
  let is_negative (Ratio(a,b))=(a<0);;
  let is_zero (Ratio(a,b))=(a=0);;	
  let is_positive (Ratio(a,b))=(a>0);; 	
	
  let sign r=if is_negative(r) then -1 else 
             if is_zero(r) then 0 else 1;; 	
	
  let is_nonnegative (Ratio(a,b))=(a>=0);;	
  let is_nonpositive (Ratio(a,b))=(a<=0);;	
	
  let minus (Ratio(a,b))=Ratio(-a,b);;
  
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
  
  let is_an_integer (Ratio(x,y))=(y=1);;	 	
	
  let lt x y=is_negative(sub x y);;
  let eq x y=is_zero(sub x y);;
  let gt x y=is_positive(sub x y);; 
  let leq x y=is_nonpositive(sub x y);;
  let geq x y=is_nonnegative(sub x y);;
  let cmp=Total_ordering.from_lt lt;;
  
  let minus_one=Ratio(-1,1);;
  let zero=Ratio(0,1);;
  let one=Ratio(1,1);;		 

  let opposite (Ratio(a,b))=Ratio(-a,b);;
  
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
     let na=Num.num_of_int(a)
	 and nb=Num.num_of_int(b) in
	 Num.div_num(na)(nb);;
 
  let approx_fix j r=Num.approx_num_fix j (to_num r);;
  
  let approx r=approx_fix 15 r;;
 
 let true_quotient a b= (* we know that b>0 in our fractions *)
    if (a>0)||((a mod b)=0) then (a/b) else 
    let m=((-a)/b)+1 in -m;;


  let floor (Ratio(a,b))=Ratio(true_quotient a b,1);;
  let ceil (Ratio(a,b))=
    if b=1
    then Ratio(a,1)
    else Ratio((true_quotient a b)+1,1);;
  
  let round (Ratio(a,b))=
    if b=1
    then (Ratio(a,1))
    else let q=(true_quotient a b) in
         if (b*(2*q+1)>(2*a))
         then Ratio(q,1)
         else Ratio(q+1,1);;
 
 let to_int rat=
    match round(rat) with
    Ratio(a,b)->a;;
 
  let print_leftmost (Ratio(x,y))=
  if y=1
  then if x=1 then "" else 
       if x=(-1) then "-" else string_of_int(x)
  else "("^string_of_int(x)^"/"^string_of_int(y)^")";;
 

  let print_in_between (Ratio(x,y))=
  if y=1
  then if x=1 then "+" else if x=(-1) then "-" else
       if (x<0) then string_of_int(x) else "+"^string_of_int(x)
  else if (x<0)
	   then "-("^(string_of_int(-x))^"/"^string_of_int(y)^")"
	   else "+("^string_of_int(x)^"/"^string_of_int(y)^")";;
	   
  let print (Ratio(x,y))=
   if y=1
   then string_of_int(x)
   else string_of_int(x)^"/"^string_of_int(y);;
   
   let print_out (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print x);
   Format.close_box();;
   
   let of_int x=Ratio(x,1);;
   
  let ocaml_name (Ratio(x,y))=
    let sx=string_of_int(x)
    and sy=string_of_int(y) in
    "Rat"^"ional.frac("^sx^")("^sy^")";;	    
   
  let gauss_multiplier l= 
     if l=[] then one else
     let l_up=List.rev_map(function Ratio(x,y)->x)(l) 
     and l_down=List.rev_map(function Ratio(x,y)->y)(l) in
     let g_up=Gcd.gcd_for_many(l_up)
     and g_down=Gcd.lcm_for_many(l_down) in
     let g=frac(g_down)(g_up) in
     g;;
   
   
   let gauss_operation l=
     if l=[] then [] else
     let l_up=List.rev_map(function (Ratio(x,y),anv)->x)(l) 
     and l_down=List.rev_map(function (Ratio(x,y),anv)->y)(l) in
     let g_up=Gcd.gcd_for_many(l_up)
     and g_down=Gcd.lcm_for_many(l_down) in
     let g=frac(g_down)(g_up) in
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
  
   
	    	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
