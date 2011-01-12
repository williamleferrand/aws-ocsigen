(*
 * aws-ocaml 0.2
 *
 * (c) 2011 William Le Ferrand
 *)

let current_timestamp () = 
  let tm = Unix.gmtime (Unix.time ()) in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (1900 + tm.Unix.tm_year) (1 + tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
  
let extract_string frame = 
   match frame.Ocsigen_http_frame.frame_content with
       None -> failwith "Server down"
     | Some data -> Ocsigen_stream.string_of_stream (Ocsigen_stream.get data) 
		    

let encodeplus = 
  Netencoding.Url.encode 

let decodeplus = 
  Netencoding.Url.decode 
