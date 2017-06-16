let power x n=
let var=ref(1) in
for i=1 to n
do
var:=x*(!var)
done;
!var;;


