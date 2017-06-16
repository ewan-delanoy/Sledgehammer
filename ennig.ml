 
 let doyle f a b=
 let accu=ref([]) in
 let rec doyle0=(function
 j->if j<a
    then (!accu)
	else let _=(accu:=f(j)::(!accu)) in doyle0(j-1)
 ) in
 doyle0 b;;
 
 let slow_doyle f a b=
 let accu=ref([]) in
 let rec slow_doyle0=(function
 j->if j>b
    then List.rev(!accu)
	else let _=(accu:=f(j)::(!accu)) in slow_doyle0(j+1)
 ) in
 slow_doyle0 a;;
 
 
 let doyle_for_delta f n u0=
 let accu=ref([u0]) and traveler=ref(u0) in
 let rec doyle0=(function
 da_ober->if da_ober<1
          then List.rev(!accu)
	      else let _=(traveler:=f(!traveler);accu:=(!traveler)::(!accu)) in 
	           doyle0(da_ober-1)
 ) in
 doyle0 n;;
  
 
let ennig a b=doyle (function x->x) a b;; 
 
let index_everything l=
  let rec tempf=
   (function (j,graet,da_ober)->
     match da_ober with
      []->graet
     |a::b->tempf(j-1,(j,a)::graet,b)
    )    in
    tempf(List.length(l),[],List.rev(l));;

let for_all f a b=
 let rec for_all0=(function
 j->if j>b
    then true
	else if f(j)
	     then for_all0(j+1)
		 else false
 ) in
 for_all0 a;;

let rec exists f a b=
if (a>b) 
then false
else if f(a)
	 then true
	 else exists f (a+1) b;;	 
 
let rec find_it f a b=
if (a>b) 
then None
else if f(a)
	 then Some(a)
	 else find_it f (a+1) b;;	  

let rec find_and_stop f a b=
 let rec find_and_stop0=(function
  j->if (j>b)
     then None
	 else match f(j) with
		None->find_and_stop0(j+1)
		|Some(x)->Some(x)
 ) in
 find_and_stop0 a;;

let constant_list n x=doyle (function j->x) 1 n;;

let describe_fibers_as_intervals f a b=
  if (a>b) then [] else
  let rec tempf=(function
    (graet,x1,x2,y0)->
       if (x2>=b) then List.rev((x1,x2,y0)::graet) else
       let x3=x2+1 in
       let y3=f(x3) in
       if (y3=y0)
       then tempf(graet,x1,x3,y0)
       else tempf((x1,x2,y0)::graet,x3,x3,y3)
  
  ) in
  tempf([],a,a,f(a));;
