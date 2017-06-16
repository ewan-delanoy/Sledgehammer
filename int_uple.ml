
let translate_pair a (x1,x2)=(a+x1,a+x2);;
let translate_triple a (x1,x2,x3)=(a+x1,a+x2,a+x3);;
let translate_fourtuple a (x1,x2,x3,x4)=(a+x1,a+x2,a+x3,a+x4);;
let translate_fiftuple a (x1,x2,x3,x4,x5)=(a+x1,a+x2,a+x3,a+x4,a+x5);;
let translate_sixtuple a (x1,x2,x3,x4,x5,x6)=(a+x1,a+x2,a+x3,a+x4,a+x5,a+x6);;

let next_pair (i,j)=
if i+1<j then (i+1,j) else
(1,i+2);;

let next_triple (i,j,k)=
if i+1<j then (i+1,j,k) else
if i+2<k then (1,i+2,k) else
(1,2,i+3);;

let next_fourtuple (x1,x2,x3,x4)=
if x1+1<x2 then (x1+1,x2,x3,x4) else
if x2+1<x3 then (1, x2+1,x3,x4) else
if x3+1<x4 then (1,2,  x3+1,x4) else
(1,2,3,x4+1);;

let next_fiftuple (x1,x2,x3,x4,x5)=
if x1+1<x2 then (x1+1,x2,x3,x4,x5) else
if x2+1<x3 then (1, x2+1,x3,x4,x5) else
if x3+1<x4 then (1,2,  x3+1,x4,x5) else
if x4+1<x5 then (1,2,3,   x4+1,x5) else
(1,2,3,4,x5+1);;

let next_sixtuple (x1,x2,x3,x4,x5,x6)=
if x1+1<x2 then (x1+1,x2,x3,x4,x5,x6) else
if x2+1<x3 then (1, x2+1,x3,x4,x5,x6) else
if x3+1<x4 then (1,2,  x3+1,x4,x5,x6) else
if x4+1<x5 then (1,2,3,   x4+1,x5,x6) else
if x5+1<x6 then (1,2,3,4,    x5+1,x6) else
(1,2,3,4,5,x6+1);;

let list_of_pairs=Memoized.make(function n->
if n<2 then [] else
let accu=ref([],(1,2))
and number_of_iterations=(n*(n-1))/2 
and iterator=(function (l,c)->(c::l,next_pair(c)) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
List.rev(fst (!accu)));;

let list_of_triples=Memoized.make(function n->
if n<3 then [] else
let accu=ref([],(1,2,3))
and number_of_iterations=(n*(n-1)*(n-2))/6 
and iterator=(function (l,c)->(c::l,next_triple(c)) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
List.rev(fst (!accu)));;

let list_of_fourtuples=Memoized.make(function n->
if n<4 then [] else
let accu=ref([],(1,2,3,4))
and number_of_iterations=(n*(n-1)*(n-2)*(n-3))/24 
and iterator=(function (l,c)->(c::l,next_fourtuple(c)) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
List.rev(fst (!accu)));;


let list_of_fiftuples=Memoized.make(function n->
if n<5 then [] else
let accu=ref([],(1,2,3,4,5))
and number_of_iterations=(n*(n-1)*(n-2)*(n-3)*(n-4))/120 
and iterator=(function (l,c)->(c::l,next_fiftuple(c)) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
List.rev(fst (!accu)));;


let list_of_sixtuples=Memoized.make(function n->
if n<6 then [] else
let accu=ref([],(1,2,3,4,5,6))
and number_of_iterations=(n*(n-1)*(n-2)*(n-3)*(n-4)*(n-5))/720 
and iterator=(function (l,c)->(c::l,next_sixtuple(c)) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
List.rev(fst (!accu)));;

let maximize_on_pairs f a b=
let n=(b-a)+1 and aa=(a-1) in
let accu=ref(f(a,a+1),[],(1,2))
and number_of_iterations=(n*(n-1))/2 
and iterator=(function (y0,lx,c)->
   let tc=translate_pair(aa)(c) and nc=next_pair(c) in
   let y=f(tc) in
   if y<y0 
   then (y0,lx,nc)
   else if y=y0
        then (y0,tc::lx,nc)
        else (y,[],nc) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
let last_term=(!accu) in
let final_y=(function (y,lx,c)->y)(last_term)
and final_list=(function (y,lx,c)->List.rev lx)(last_term) in
(final_y,final_list);;
  
let maximize_on_triples f a b=
let n=(b-a)+1 and aa=(a-1) in
let accu=ref(f(a,a+1,a+2),[],(1,2,3))
and number_of_iterations=(n*(n-1)*(n-2))/6 
and iterator=(function (y0,lx,c)->
   let tc=translate_triple(aa)(c) and nc=next_triple(c) in
   let y=f(tc) in
   if y<y0 
   then (y0,lx,nc)
   else if y=y0
        then (y0,tc::lx,nc)
        else (y,[],nc) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
let last_term=(!accu) in
let final_y=(function (y,lx,c)->y)(last_term)
and final_list=(function (y,lx,c)->List.rev lx)(last_term) in
(final_y,final_list);;  
  
let maximize_on_fourtuples f a b=
let n=(b-a)+1 and aa=(a-1) in
let accu=ref(f(a,a+1,a+2,a+3),[],(1,2,3,4))
and number_of_iterations=(n*(n-1)*(n-2)*(n-3))/24 
and iterator=(function (y0,lx,c)->
   let tc=translate_fourtuple(aa)(c) and nc=next_fourtuple(c) in
   let y=f(tc) in
   if y<y0 
   then (y0,lx,nc)
   else if y=y0
        then (y0,tc::lx,nc)
        else (y,[],nc) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
let last_term=(!accu) in
let final_y=(function (y,lx,c)->y)(last_term)
and final_list=(function (y,lx,c)->List.rev lx)(last_term) in
(final_y,final_list);;

let maximize_on_fiftuples f a b=
let n=(b-a)+1 and aa=(a-1) in
let accu=ref(f(a,a+1,a+2,a+3,a+4),[],(1,2,3,4,5))
and number_of_iterations=(n*(n-1)*(n-2)*(n-3)*(n-4))/120
and iterator=(function (y0,lx,c)->
   let tc=translate_fiftuple(aa)(c) and nc=next_fiftuple(c) in
   let y=f(tc) in
   if y<y0 
   then (y0,lx,nc)
   else if y=y0
        then (y0,tc::lx,nc)
        else (y,[],nc) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
let last_term=(!accu) in
let final_y=(function (y,lx,c)->y)(last_term)
and final_list=(function (y,lx,c)->List.rev lx)(last_term) in
(final_y,final_list);;
  
let maximize_on_sixtuples f a b=
let n=(b-a)+1 and aa=(a-1) in
let accu=ref(f(a,a+1,a+2,a+3,a+4,a+5),[],(1,2,3,4,5,6))
and number_of_iterations=(n*(n-1)*(n-2)*(n-3)*(n-4)*(n-5))/720
and iterator=(function (y0,lx,c)->
   let tc=translate_sixtuple(aa)(c) and nc=next_sixtuple(c) in
   let y=f(tc) in
   if y<y0 
   then (y0,lx,nc)
   else if y=y0
        then (y0,tc::lx,nc)
        else (y,[],nc) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
let last_term=(!accu) in
let final_y=(function (y,lx,c)->y)(last_term)
and final_list=(function (y,lx,c)->List.rev lx)(last_term) in
(final_y,final_list);;
    
  
  
