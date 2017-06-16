(*

#use"detect_arithmetic_progressions.ml";;

*)


 

  
  		type arith_prog=
   			{
     			starting_point:int; 
     			length:int;
     			step: int;
    		};;  
    
		type specified_set=
   			Arith_Prog of arith_prog
   			|Nothing_Special of int list;;
  
		let unveil x=(x.starting_point,x.length,x.step);;
  
		let rec arith_prog_catcher (x0,d,j,x,peurrest)=
   			match peurrest with
     		[]->({starting_point=x0;length=j;step=d},[])
     		|y::peurrest2->
        		if y=(x+d)
        		then arith_prog_catcher(x0,d,j+1,y,peurrest2)
        		else ({starting_point=x0;length=j;step=d},peurrest);;
    
		let rec nothing_special_catcher (graet,x,y,peurrest)=
  			match peurrest with
     		[]->(List.rev(y::x::graet),[])
     		|z::peurrest3->
        		if z-y=y-x
        		then (List.rev(graet),x::y::peurrest)
        		else nothing_special_catcher(x::graet,y,z,peurrest3);;
 
    
  
		let rec catcher l=
  			if List.length(l)<3 then (Nothing_Special(l),[]) else
  			let a1=List.hd(l) and t1=List.tl(l) in
  			let a2=List.hd(t1) and t2=List.tl(t1) in
  			let a3=List.hd(t2) and t3=List.tl(t2) in
  			let d=a2-a1 in
  			if a3=a2+d
  			then let (tamm1,peurrest)=arith_prog_catcher(a1,d,3,a3,t3) in
       		(Arith_Prog(tamm1),peurrest)
  		else let (tamm1,peurrest)=nothing_special_catcher([a1],a2,a3,t3) in
            (Nothing_Special(tamm1),peurrest);;
  
  
		let decompose l=
   			let rec tempf=(fun
     		(graet,da_ober)->match da_ober with
     		[]->List.rev graet
     		|ket_goullo->
        		let (tamm1,peurrest)=catcher(da_ober) in
        		tempf(tamm1::graet,peurrest)
   			) in
   		tempf([],l);;
   
  
 
 let string_of_specified_set (x:specified_set)=
   match x with
   Arith_Prog(ap)->
      let (start,length,step)=unveil(ap) in
      let s="["^(string_of_int start)^".."^(string_of_int (start+(length-1)*step))^"]" in
      if step>1
      then s^"(by "^(string_of_int step)^")"
      else s
   |Nothing_Special(l)->
    "["^(String.concat(";")(Image.image string_of_int l))^"]";;      
                
 
 let pretty_print l=
   let temp1=decompose(l) in
   let temp2=Image.image string_of_specified_set temp1 in
   String.concat "" temp2;;
   
   
 
 




   
