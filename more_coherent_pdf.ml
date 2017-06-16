(*

#use"more_coherent_pdf.ml";;

Utility to cut a large PDF into easily printable chunks.
Each chunk is at most 20 pages long, and separated into
odd/even parts to allow for recto-verso printing. 

*)


type source={
   root_directory :string;
   pdffile_name   :string;
   page_interval  :int*int;
   chunk_size     :int;
};;

let initialize (dir1,fname,i,j,opt)={
   root_directory =(if Substring.ends_with dir1 "/" then dir1 else dir1^"/");
   pdffile_name   =(if Substring.ends_with fname ".pdf" then fname else fname^".pdf");
   page_interval  =(i,j);
   chunk_size    =(if opt=None then 20 else Option.unpack opt);
};;    

let individual_act x (act_description,act_output)=
    let cmd1="/Applications/cpdf "^x.root_directory^"Coherent_PDF/"^act_description^
              " -o "^act_output^".pdf" in
    let cmd2="mv "^act_output^".pdf "^x.root_directory^"Coherent_PDF/" in
    [cmd1;cmd2];;

let individual_command x (i,j)=
    let si=string_of_int i and sj=string_of_int j in
    List.flatten(
    Image.image (fun (a,b)->individual_act x (a,b))
    [
      (x.pdffile_name^" "^si^"-"^sj,"from_"^si^"_to_"^sj);
      ("from_"^si^"_to_"^sj^".pdf even","from_"^si^"_to_"^sj^"_even");
      ("from_"^si^"_to_"^sj^".pdf odd","from_"^si^"_to_"^sj^"_odd");
    ]
    );;

let prepare_premises x=
   ["mkdir -p "^x.root_directory^"Coherent_PDF/";
    "cp "^x.root_directory^x.pdffile_name^" "^x.root_directory^"Coherent_PDF/"];;

let list_of_commands x=
   let (i,j)=x.page_interval 
   and d=x.chunk_size in
   let r=(j-i)/d in
   let intervals=Ennig.doyle (fun k->(i+(k-1)*d,min(i+k*d-1)(j))) 1 (r+1) in
   List.flatten(
    (prepare_premises x)::(Image.image (individual_command x) intervals)
    );; 


(*

let example=initialize
   (
     "/Users/ewandelanoy/Documents/html_files/Ricciotti/",
     "plain_pages.pdf",
     1,511
     ,
     None
   );;

let example=initialize
   (
     "/Users/ewandelanoy/Documents/html_files/Wilhelm/",
     "ww.pdf",
     1,431
     ,
     None
   );;

let result1=list_of_commands example;;   
let result2=Explicit.image Unix_command.uc result1;;

*)

