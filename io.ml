
let make_filename_complete s=
  let home=Sys.getenv("HOME") in
  if s="" then Absolute_path.of_string(home) else
  let c=String.get s 0 in
  if c='/' then Absolute_path.of_string(s) else
  if c='~' then Absolute_path.of_string(home^(String.sub s 1 (String.length(s)-1))) else
  Absolute_path.of_string((Sys.getcwd ())^"/"^s);;

let open_in_locally x=try open_in(x) with 
_->failwith("File "^x^" cannot be opened in");;

let open_out_locally x=try open_out(x) with 
_->failwith("File "^x^" cannot be opened out");;  

let put_whole_content_of_file_in_buffer s=
  let x=Absolute_path.to_string(make_filename_complete(s)) in
  let janet=open_in_locally(x) in
  let n=in_channel_length(janet) in
  let b=Buffer.create(n) in
  let _=Buffer.add_channel(b)(janet)(n) in
  let _=close_in janet in
  b;;
  
type filename=string;;
  
let erase_file_and_fill_it_with_contents_of_buffer (fn:filename) b=
   let x=Absolute_path.to_string(make_filename_complete(fn)) in
  let john=open_out_locally(x) in
  (Buffer.output_buffer(john)(b);close_out john);;
  
let erase_file_and_fill_it_with_string ap s=
   let fn=Absolute_path.to_string ap in
   let n=String.length(s) in
   let b=Buffer.create(n) in
   let _=Buffer.add_string b s in
   erase_file_and_fill_it_with_contents_of_buffer fn b;;
   
let read_whole_file ap=   
   let s=Absolute_path.to_string ap in
   let b=put_whole_content_of_file_in_buffer(s) in
   Buffer.contents b;;

let append_string_to_file s ap=
  let new_content=(read_whole_file ap)^s in
  erase_file_and_fill_it_with_string ap new_content;; 

     
   
   
  
