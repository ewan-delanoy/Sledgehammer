(*

#use"current_date.ml";;

*)

let current_date ()=
  let temp=Unix.gmtime(Unix.time()) in
  let year=string_of_int(temp.Unix.tm_year+1900)
  and month1=string_of_int(temp.Unix.tm_mon+1)
  and day1=string_of_int(temp.Unix.tm_mday) in
  let month=Cull_string.resize_from_right month1 2 '0'
  and day=Cull_string.resize_from_right day1 2 '0' in
  year^"_"^month^"_"^day;;

