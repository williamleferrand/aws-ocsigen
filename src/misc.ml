(*
 * aws-ocaml 0.2
 *
 * (c) 2011 William Le Ferrand
 *)

open Lwt 

let current_timestamp () = 
  let tm = Unix.gmtime (Unix.time ()) in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (1900 + tm.Unix.tm_year) (1 + tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
  
let expiration delay = 
    let tm = Unix.gmtime (Unix.time () +. delay) in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (1900 + tm.Unix.tm_year) (1 + tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
  

let extract_string frame = 
   match frame.Ocsigen_http_frame.frame_content with
       None -> failwith "Server down (raised from misc.ml)"
     | Some data -> Ocsigen_stream.string_of_stream (Ocsigen_stream.get data) 

let extract_stream frame = 
   match frame.Ocsigen_http_frame.frame_content with
       None -> failwith "Server down (raised from misc.ml)"
     | Some data -> return (Ocsigen_stream.get data) 
		    

(* Quick and dirty, but aws seems to have an issue with the wildcard *)
let encode str = 
  let strlist = ref [] in 
  for i = 0 to String.length str - 1 do 
    let c = Char.code (str.[i]) in 
      if (65 <= c && c <= 90) || (48 <= c && c <= 57 ) || (97 <= c && c <= 122) || (c = 126) || (c = 95) || (c = 46) || (c = 45) then  
	strlist := Printf.sprintf "%c" str.[i] :: !strlist 
      else 
	strlist :=  Printf.sprintf "%%%X" c :: !strlist 
  done ;
    String.concat "" (List.rev !strlist) 
