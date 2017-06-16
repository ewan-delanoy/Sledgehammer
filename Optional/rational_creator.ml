
let description_of_small_int_mode="(* Rat-ionals are quotients of ordinary ints here *)";;
let description_of_big_int_mode=  "(* Rat-ionals are quotients of big ints here      *)";;


let current_description=
  (*If no indication is given, we assume that we are in small_int mode *)
  let janet=open_in("rational.ml") in
  let buf=String.make(52)(' ') in
  let _=really_input janet buf 0 52 in
  if buf=description_of_big_int_mode
  then description_of_big_int_mode
  else description_of_small_int_mode;;


let big_int_active=ref(current_description=description_of_big_int_mode);;


let small_int_mode ()=
   let cmd="cp Creators/small_int_based_rational.ml rational.ml" in
   let _=Unix_command.uc(cmd) in
   ();;
   
let big_int_mode ()=
   let cmd="cp Creators/big_int_based_rational.ml rational.ml" in
   let _=Unix_command.uc(cmd) in
   ();;
   
   
let implement()=
  if (!big_int_active)
  then big_int_mode()
  else small_int_mode();;
  
let switch()=(big_int_active:=not(!big_int_active);implement());;  

