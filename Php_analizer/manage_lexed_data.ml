(*

#use"Php_analizer/manage_lexed_data.ml";;

*)

module Private=struct

let data_list=ref (
   Image.image (
     fun (x,y)->
      (x,Directory_name.of_string y)
   ) [
     "symblog","/Users/ewandelanoy/Documents/Sites/Symblog/symblogproject/";
     "phpbb","/Users/ewandelanoy/Documents/Sites/Mikeal/public_html/";
   ]
);;

let recompute_lexed_data_from_scratch dir=
  let temp1=More_unix.all_files_with_endings dir [".php"] in
  let temp2=Image.image (
    fun t->
   Positioned_php_token_list.filter (fun ptok->
    let tok=Positioned_php_token.fst ptok in
    not(Php_token.is_a_comment tok)
    )(Php_lexer.parse_file t)
  ) temp1 in
  temp2;;
  
let marshaled_file_for_item s=
  (Directory_name.connectable_to_subpath German_constant.root)^
  "Remembered/Marshaled_data/"^s^".mshl";;  

  
let retrieve_lexed_data_from_marshaled_file s=
  let chan=open_in (marshaled_file_for_item s) in
  let answer=((Marshal.from_channel chan):Positioned_php_token_list.t list) in
  let _=close_in chan in
  answer;;
  
let persist_lexed_data s=
  let dir=List.assoc  s (!data_list) in
  let answer=recompute_lexed_data_from_scratch dir in
  let chang=open_out (marshaled_file_for_item s) in
  let _=(Marshal.to_channel chang answer [];close_out chang) in
  answer;;
  
let get_individual_data s=
   if Sys.file_exists(marshaled_file_for_item s)
   then retrieve_lexed_data_from_marshaled_file s 
   else persist_lexed_data s;;
   
 let get_data l=
    List.flatten(Image.image get_individual_data l);;  
  
end;;  
  	 
let get_data=Private.get_data;;  
    