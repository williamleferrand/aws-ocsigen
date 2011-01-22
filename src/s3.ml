(*
 * aws-ocaml 0.2
 *
 * (c) 2011 William Le Ferrand
 *)

open Lwt
open Misc
open Connection

(* List contents from a bucket *)

(* GET /?prefix=photos&max-keys=50&marker=puppy HTTP/1.1 *)
let list connection ?(prefix="") ?(max_keys=50) ?(marker=None) bucket = 
  let headers = Authentication.s3headers connection "GET" ("/"^bucket^"/") in 
  let uri = (Printf.sprintf "/?prefix=%s&max-keys=%d" prefix max_keys)^(match marker with Some n -> "&marker="^n | None -> "") in
  Http_client.get ~headers ~host:(bucket^".s3.amazonaws.com") ~uri ()
  >>= extract_string
  

let get connection bucket key = 
  let headers = Authentication.s3headers connection "GET" ("/"^bucket^"/"^key) in 
  let uri = "/" ^ key in
  Http_client.get ~headers ~host:(bucket^".s3.amazonaws.com") ~uri ()
  >>= extract_stream

(* Special URI with credentials for upload in a s3 bucket *)
let string_request connection verb content_md5 content_type amz_headers ressource = 
  let expires = Int64.of_float (Unix.time () +. 36.0 *. 3600.0) in
  let string_to_sign = 
    Printf.sprintf "%s\n%s\n%s\n%Ld\n%s%s" 
      (match verb with `POST -> "POST" | `GET -> "GET" | `PUT -> "PUT")
      content_md5 
      content_type 
      expires 
      amz_headers ressource in
  Printf.printf "%s\n" string_to_sign ;
  let hasher = Cryptokit.MAC.hmac_sha1 connection.secret_access_key in
  let hashed = Cryptokit.hash_string hasher string_to_sign in
  let encoder = Cryptokit.Base64.encode_multiline () in 
  let signature = Cryptokit.transform_string encoder hashed in
  let signature = String.sub signature 0 (String.length signature - 1) in
  let request = Printf.sprintf "AWSAccessKeyId=%s&Signature=%s&Expires=%Ld" connection.access_key_id (Netencoding.Url.encode signature) expires in  
  request 


(* Initiate multipart *)
let initiate_multipart ?(content_type="text/html") connection bucket key =
  
  let headers = Authentication.s3headers ~content_type connection "POST" ("/" ^bucket^"/"^key^"?uploads") in
  let uri = "/" ^ key ^ "?uploads" in 
  Http_client.get ~headers ~host:(bucket^".s3.amazonaws.com") ~uri ()
  >>= extract_string
  
