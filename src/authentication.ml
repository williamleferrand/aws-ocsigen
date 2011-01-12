(*
 * aws-ocaml 0.2
 *
 * (c) 2011 William Le Ferrand
 *)

open Connection
open Misc

let sign sec2 request = 
  let sorted_attributes = List.sort (fun (k1, v1)  (k2, v2) -> String.compare k1 k2) request in
  let encoded_attributes = List.map (fun (k, v) -> encodeplus k, encodeplus v) sorted_attributes in
  let raw_request = List.fold_left (fun acc (k,v) -> 
    if acc = "" then 
      Printf.sprintf "%s=%s" k v 
    else 
      Printf.sprintf "%s&%s=%s" acc k v) "" encoded_attributes in
  let string_to_sign = Printf.sprintf "%s\n%s\n%s\n%s" sec2.http_method sec2.http_host sec2.http_uri raw_request in
  let hasher = Cryptokit.MAC.hmac_sha1 sec2.secret_access_key in
  let hashed = Cryptokit.hash_string hasher string_to_sign in
  let encoder = Cryptokit.Base64.encode_multiline () in 
  let r = Cryptokit.transform_string encoder hashed in 
  String.sub r 0 (String.length r - 1)
      
	    
  
 



