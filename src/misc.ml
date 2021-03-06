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
     | Some data -> Ocsigen_stream.string_of_stream 1000000 (Ocsigen_stream.get data)
		    >>= fun s -> 
                             catch (fun () -> Ocsigen_stream.finalize data `Success >>= fun _ -> return s)
			           (fun _ -> return s)

let extract_nothing frame = 
  match frame.Ocsigen_http_frame.frame_content with
      None -> return () 
    | Some data -> catch (fun () -> Ocsigen_stream.finalize data `Success) (fun _ -> return ()) 

let extract_stream frame = 
   match frame.Ocsigen_http_frame.frame_content with
       None -> failwith "Server down (raised from misc.ml)"
     | Some data -> return data
		    

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

(*
let encode = Netencoding.Url.encode ~plus:false 
*)
(* It is not even better, it fails for utf8 *)
let encode_cpl = Netencoding.Url.mk_url_encoded_parameters
let encode_cpl l = 
  List.fold_left (fun acc (k,v) -> 
    if acc = "" then 
      Printf.sprintf "%s=%s" (encode k)  (encode v) 
    else 
      Printf.sprintf "%s&%s=%s" acc (encode k) (encode v)) "" l
  
