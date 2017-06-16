

type t={with_x2:int;with_x:int;with_y2:int;with_y:int;constant:int};;

let constructor (a,b,c,d,e)={with_x2=a;with_x=b;with_y2=c;with_y=d;constant=e};;
let compact_form q=(q.with_x2,q.with_x,q.with_y2,q.with_y,q.constant);;


let evaluate q (x,y)=
(q.with_x2*x*x)+(q.with_x*x)+(q.with_y2*y*y)+(q.with_y*y)+q.constant;;


let small_image q n=
let temp1=Ennig.ennig(0)(n) in
let temp2=Cartesian.product(temp1)(temp1) in
Ordered_integer.diforchan(Image.image(evaluate(q))(temp2));;

let linear_change_of_variables q (lx,mx,ly,my)=
match compact_form(q) with
(a,b,c,d,e)->
{with_x2=a*lx*lx;
 with_x=lx*((2*a*mx)+b);
 with_y2=b*ly*ly;
 with_y=ly*((2*c*my)+d);
 constant=evaluate q (mx,my)
};;

let solve_modulo_n q n=
 let temp1=Ennig.ennig(0)(n-1) in
 let temp2=Cartesian.product(temp1)(temp1) in
 List.filter(function c->(evaluate q c)=0)(temp2);;

let local_simplified_form q n (x0,y0)=
  match compact_form(q) with
  (a,b,c,d,e)->
  {with_x2=a*n;
   with_x=(2*a*x0)+b;
   with_y2=b*n;
   with_y=(2*c*y0)+d;
   constant=(evaluate q (x0,y0))/n 
  };;

  
let lift_solution q n nm (x0,y0)=
 let m=(nm/n) 
 and qq=local_simplified_form q n (x0,y0) in
 let temp1=solve_modulo_n(qq)(m) in
 Image.image (function (dx,dy)->(x0+n*dx,y0+n*dy) ) temp1;;
  
(*
let read q=
 match compact_form(q) with
 (a,b,c,d,e)->
   let l1=[(a,"x^2");(b,"x");(c,"y^2");(d,"y");(e,"1")] 
   and tempf=(function (val,string)->
   if val=0 then "" else
   if val=1 then string else 
   if val=-1 then "-"^string else
   (string)
   )

let print_out (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(read x);
   Format.close_box();;
   
*)   

let fermat d=constructor(1,0,d,0,0);;



		   
		   


