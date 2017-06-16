(*

#use"unix_compliant_filename.ml";;

*)



    
 let uppercase_letters= 
   ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';
    'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';
    'U';'V';'W';'X';'Y';'Z'];;
    
 let lowercase_identifier_elements=    
    ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
     'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
     'u';'v';'w';'x';'y';'z';'_';'+';'-';'*';
     '0';'1';'2';'3';'4';'5';'6';'7';'8';'9']@uppercase_letters;;
     
 let strictly_alphanumeric_characters =
  [
   'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
   'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
   'u';'v';'w';'x';'y';'z';
   'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';
   'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';
   'U';'V';'W';'X';'Y';'Z';
   '0';'1';'2';'3';'4';'5';'6';'7';'8';'9';
   '_';
   ];;   

let alphanumeric_characters =
  strictly_alphanumeric_characters @
  [
   '.';'\''
  ];;    

let unix_filename_admissible_characters =
  strictly_alphanumeric_characters @
  [
   '.';'/';'!';
  ];;        
    
 let look_for_capitalized_identifiers s=
   let n=String.length s in
   let rec tempf=(fun (graet,j,j0)->
       if (j>=(n-1)) then List.rev(graet) else
       let c=String.get s (j+1) in
       if (j0>=0)
       then if List.mem c lowercase_identifier_elements
            then tempf(graet,j+1,j0)
            else let s1=String.lowercase_ascii(String.sub s j0 (j-j0+1)) in
                 if List.mem s1 graet
                 then tempf(graet,j+1,-1)
                 else tempf(s1::graet,j+1,-1)
       else
            if List.mem c uppercase_letters
            then tempf(graet,j+1,j+1)
            else tempf(graet,j+1,(-1))
   ) in
   tempf([],-1,-1);;
    
    
let is_a_lowercase c=let i=int_of_char c in (97<=i)&&(i<=122);;
let is_an_uppercase c=let i=int_of_char c in (65<=i)&&(i<=90);;
let character_is_alphanumeric c=List.mem c alphanumeric_characters;;
let character_is_strictly_alphanumeric c=List.mem c strictly_alphanumeric_characters;;
let is_an_uppercase_letter c=List.mem c uppercase_letters;;   
  
let string_is_alphanumeric s=
   List.for_all (fun j->
     character_is_alphanumeric(String.get s j)
   ) (Ennig.ennig 0 (String.length(s)-1));;  
  
let is_unix_filename_admissible s=
   List.for_all (fun j->
     List.mem (String.get s j) unix_filename_admissible_characters
   ) (Ennig.ennig 0 (String.length(s)-1));;  

exception Unix_rewrite_exn of char;;

let list_for_unix_rewriting=
   [
      ' ',"_";
      '-',"_";
      '\'',"_single_quote_";
      '"',"_single_quote_";
      '&',"_and_";
      '(',"_left_parenthesis_";
      ')',"_right_parenthesis_";
      '?',"_question_mark_";
      '|',"_vertical_bar_";
      '<',"_lower_than_";
      '>',"_greater_than_";
      '=',"_equals_";
      ',',"_comma_";
      ';',"_semicolon_";
    ];;
  
let unix_rewrite_char c=
    if List.mem c unix_filename_admissible_characters
    then String.make 1 c
    else
    List.assoc c
    list_for_unix_rewriting;;
  
exception Unix_unknown of char*string;;  
  
let make_unix_compliant s=
   try String.concat "" (Image.image (fun j->
     unix_rewrite_char(String.get s j)
   ) (Ennig.ennig 0 (String.length(s)-1))) with
   Unix_rewrite_exn(c)->raise(Unix_unknown(c,s));;  
  
  
let starry_from l s i=
   let n=String.length s in
   let rec tempf=(fun (k0,k)->
    if k>n
    then String.sub s (k0-1) (k-k0)
    else 
    if List.mem(String.get s (k-1)) l 
    then tempf(k0,k+1)
    else String.sub s (k0-1) (k-k0)
   ) in
   tempf(i,i);;
     
  

  
  