(*

#use"Generic_syntax_types/token_category.ml";;

Used in the PHP analizer modules.
   
*)



type t=
      Keyword
     |Punctuator
     |Operator
     |Variable
     |Identifier
     |Comment
     |Single_quoted_string
     |Double_quoted_string
     |Heredoc_string
     |Nowdoc_string
     |Namespacer
     |External_item
     |Integer
     |Floating_number
     |Character
     |End_of_text;;
     
let all_tokens=
   [
     Keyword;
     Punctuator;
     Operator;
     Variable;
     Identifier;
     Comment;
     Single_quoted_string;
     Double_quoted_string;
     Heredoc_string;
     Nowdoc_string;
     Namespacer;
     External_item;
     Integer;
     Floating_number;
     Character;
     End_of_text
   ];;     
     
let string_tokens=
   [
     
     Variable;
     Identifier;
     Comment;
     Single_quoted_string;
     Double_quoted_string;
     Heredoc_string;
     Nowdoc_string
     
   ];;     
     
let harmless_tokens=string_tokens@[Integer;Floating_number];;
   
let to_string=function
      Keyword->"kwd"
     |Punctuator->"punct"
     |Operator->"op"
     |Variable->"variable"
     |Identifier->"id"
     |Comment->"cmt"
     |Single_quoted_string->"sqs"
     |Double_quoted_string->"dqs"
     |Heredoc_string->"heredoc"
     |Nowdoc_string->"nowdoc"
     |Namespacer->"nmspc"
     |External_item->"ext"
     |Integer->"integer"
     |Floating_number->"float"
     |Character->"chr"
     |End_of_text->"eot";;
        
   
   
   