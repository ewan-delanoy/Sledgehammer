let filter f=function
[]->[]
|l->
 let rec filter_easily0=(function
 (graet,da_ober)->match da_ober with
   []->List.rev(graet)
   |a::peurrest->if f(a) 
				 then filter_easily0(a::graet,peurrest)
                 else List.rev(graet)
 ) in
 filter_easily0([],l);; 

let partition f=function
[]->[]
|x::y->
 let rec partition_easily0=(function
 (graet,y,la,da_ober)->match da_ober with
   []->List.rev(List.rev(la)::graet)
   |b::peurrest->let z=f(b) in
                 if z=y
				 then partition_easily0(graet,y,b::la,peurrest)
                 else partition_easily0(List.rev(la)::graet,z,[b],peurrest)
 ) in
 partition_easily0([],f(x),[x],y);;  
 
let partition_in_two_parts f l=
   let rec tempf=(fun
    (graet,da_ober)->match da_ober with
      []->(List.rev graet,[])
      |a::peurrest->
         if f(a)
         then tempf(a::graet,peurrest)
         else (List.rev graet,da_ober)
   ) in
   tempf([],l);; 
 
 let write_interval (i,j)=match (j-i) with
  0->string_of_int(i)
  |1->(string_of_int i)^","^(string_of_int j)
  |arall->"["^((string_of_int i)^".."^(string_of_int j))^"]";;
 
 let doyle f a b=
  if (b<a) then [] else
   let rec tempf=(function
  (graet,y,i0,current_i)->
     if (current_i>b)
     then List.rev( (write_interval(i0,current_i-1),y) ::graet)
     else let z=f(current_i) in
          if z=y
          then tempf(graet,y,i0,current_i+1)
          else tempf( (write_interval(i0,current_i-1),y) ::graet, z,current_i,current_i+1)
   ) in
   tempf([],f(a),a,a+1);;
 
 let partition_according_to_fst=function
[]->[]
|(x1,y1)::lost->
 let rec partition_easily0=(function
 (graet,xi,ly,da_ober)->match da_ober with
   []->List.rev((xi,List.rev(ly))::graet)
   |(x,y)::peurrest->
                 if x=xi
				 then partition_easily0(graet,xi,y::ly,peurrest)
                 else partition_easily0((xi,List.rev(ly))::graet,x,[y],peurrest)
 ) in
 partition_easily0([],x1,[y1],lost);;  
