
(* 

#use"Country/Alaska/alaskan_try_to_register.ml";;

*)


let mlx_file mdata mlx_file=
    try(Some(Alaskan_register_mlx_file.on_monitored_modules 
        mdata mlx_file)) with _->None;;  

module Private=struct

exception Pusher_exn;;

let pusher  (vdata,failures,yet_untreated)=
     match yet_untreated with
      []->raise(Pusher_exn)
      |mlx::others->
      (
        match mlx_file vdata mlx with
        None->(vdata,mlx::failures,others)
        |Some(nfs)->(nfs,failures,others)
      );; 

let rec iterator x=
   let (vdata,failures,yet_untreated)=x in
   match yet_untreated with
      []->(failures,vdata)
      |mlx::others->
      (
        match mlx_file vdata mlx with
        None->iterator(pusher x)
        |Some(nfs)->iterator(pusher x)
      );;   

end;;

let mlx_files mdata mlx_files=
   Private.iterator(mdata,[],mlx_files);;
 
  
   