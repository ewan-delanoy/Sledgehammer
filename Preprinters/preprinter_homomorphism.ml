(*

#use"Preprinters/preprinter_homomorphism.ml";;


*)


let list (initial_preprinter: 'a Preprinter.t)=
  let tempf=(function 
    []->["[]"]
    |a::peurrest->
       let temp1=Image.image (fun x->" ; "::(initial_preprinter x)) peurrest in
       let temp2=List.flatten temp1 in
       "[ "::(temp2@[" ]"])
  ) in
  (tempf: ('a list) Preprinter.t);;


