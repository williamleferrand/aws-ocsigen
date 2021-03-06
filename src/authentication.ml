(*
 * aws-ocaml 0.2
 *
 * (c) 2011 William Le Ferrand
 *)

open CalendarLib

open Connection
open Misc

(* For SDB, EC2 *)
let sign sec2 request = 
  let sorted_attributes = List.sort (fun (k1, v1)  (k2, v2) -> String.compare k1 k2) request in
  (* let encoded_attributes = List.map (fun (k, v) -> encode k, encode v) sorted_attributes in
     let raw_request = List.fold_left (fun acc (k,v) -> 
     if acc = "" then 
     Printf.sprintf "%s=%s" k v 
     else 
     Printf.sprintf "%s&%s=%s" acc k v) "" sorted_attributes in *)
  let raw_request = encode_cpl sorted_attributes in 
  
  let string_to_sign = Printf.sprintf "%s\n%s\n%s\n%s" sec2.http_method sec2.http_host sec2.http_uri raw_request in
  let hasher = Cryptokit.MAC.hmac_sha1 sec2.secret_access_key in
  let hashed = Cryptokit.hash_string hasher string_to_sign in
  let encoder = Cryptokit.Base64.encode_multiline () in 
  let r = Cryptokit.transform_string encoder hashed in 
  String.sub r 0 (String.length r - 1)
      
	    
(* For S3 *)
let get_date () = 
  let date = Date.today () in
  let time = Time.now () in
  let date_s = Printer.Date.sprint "%a, %d %b %Y" date in    
  let time_s = Printer.Time.sprint " %H:%M:%S GMT" time in 
   date_s ^ time_s 
  
let s3headers connection verb ?(content_md5="") ?(content_type="") ?(amzheaders="") resource = 
  let date = get_date () in 
  let string_to_sign = Printf.sprintf "%s\n%s\n%s\n%s\n%s%s" verb content_md5 content_type date amzheaders resource in
  let hasher = Cryptokit.MAC.hmac_sha1 connection.secret_access_key in
  let hashed = Cryptokit.hash_string hasher string_to_sign in
  let encoder = Cryptokit.Base64.encode_multiline () in 
  let signature = Cryptokit.transform_string encoder hashed in
  let signature = String.sub signature 0 (String.length signature - 1) in
  
  let authorization = Printf.sprintf "AWS %s:%s" connection.access_key_id signature in
  
  Http_headers.add (Http_headers.name "Authorization") authorization
    (Http_headers.add (Http_headers.name "Date") date 
       (Http_headers.add (Http_headers.name "Transfer-Encoding") "" (match content_type with
	 | "" -> Http_headers.empty
	 | _ as c -> Http_headers.add (Http_headers.content_type) c Http_headers.empty
	)))
   

(* 
   http://localhost:1024/backend/users?token=rO0ABXNyACdjb20uYW1hem9uLnNkcy5RdWVyeVByb2Nlc3Nvci5Nb3JlVG9rZW7racXLnINNqwMA%0AC0kAFGluaXRpYWxDb25qdW5jdEluZGV4WgAOaXNQYWdlQm91bmRhcnlKAAxsYXN0RW50aXR5SURa%0AAApscnFFbmFibGVkSQAPcXVlcnlDb21wbGV4aXR5SgATcXVlcnlTdHJpbmdDaGVja3N1bUkACnVu%0AaW9uSW5kZXhaAA11c2VRdWVyeUluZGV4TAANY29uc2lzdGVudExTTnQAEkxqYXZhL2xhbmcvU3Ry%0AaW5nO0wAEmxhc3RBdHRyaWJ1dGVWYWx1ZXEAfgABTAAJc29ydE9yZGVydAAvTGNvbS9hbWF6b24v%0Ac2RzL1F1ZXJ5UHJvY2Vzc29yL1F1ZXJ5JFNvcnRPcmRlcjt4cAAAAAAAAAAAAAAAAHsAAAAAAAAA%0AAAAAAAAAAAAAAABwcHB4 
   http://localhost:1024/backend/users?token=rO0ABXNyACdjb20uYW1hem9uLnNkcy5RdWVyeVByb2Nlc3Nvci5Nb3JlVG9rZW7racXLnINNqwMAC0kAFGluaXRpYWxDb25qdW5jdEluZGV4WgAOaXNQYWdlQm91bmRhcnlKAAxsYXN0RW50aXR5SURaAApscnFFbmFibGVkSQAPcXVlcnlDb21wbGV4aXR5SgATcXVlcnlTdHJpbmdDaGVja3N1bUkACnVuaW9uSW5kZXhaAA11c2VRdWVyeUluZGV4TAANY29uc2lzdGVudExTTnQAEkxqYXZhL2xhbmcvU3RyaW5nO0wAEmxhc3RBdHRyaWJ1dGVWYWx1ZXEAfgABTAAJc29ydE9yZGVydAAvTGNvbS9hbWF6b24vc2RzL1F1ZXJ5UHJvY2Vzc29yL1F1ZXJ5JFNvcnRPcmRlcjt4cAAAAAAAAAAAAAAAAHsAAAAAAAAAAAAAAAAAAAAAAABwcHB4 *)
