(*
 * aws-ocaml 0.2
 *
 * (c) 2011 William Le Ferrand
 *)

open Lwt
open Misc

(* List contents from a bucket *)

(* GET /?prefix=photos&max-keys=50&marker=puppy HTTP/1.1 *)
let list connection ?(prefix="") ?(max_keys=50) ?(marker=None) bucket = 
  let headers = Authentication.s3headers connection "GET" ("/"^bucket^"/") in 
  let uri = (Printf.sprintf "/?prefix=%s&max-keys=%d" prefix max_keys)^(match marker with Some n -> "&marker="^n | None -> "") in
  Http_client.get ~headers ~host:(bucket^".s3.amazonaws.com") ~uri ()
  >>= extract_string
  

let get connection bucket key = 
  let headers = Authentication.s3headers connection "GET" ("/"^bucket^"/"^key) in 
  let uri = key in
  Http_client.get ~headers ~host:(bucket^".s3.amazonaws.com") ~uri ()
  >>= extract_string
