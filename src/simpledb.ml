(*
 * aws-ocaml 0.2
 *
 * (c) 2011 William Le Ferrand
 *)

open Lwt 

open Connection
open Misc 

let create_domain connection domain =
  let request = 
    ("AWSAccessKeyId", connection.access_key_id) :: 
      ("Action", "CreateDomain") ::
      ("DomainName", domain) ::
      ("SignatureMethod", "HmacSHA1") ::
      ("SignatureVersion", "2") ::
      ("Timestamp", current_timestamp ()) :: 
      ("Version", "2009-04-15") :: [] in 
  
  let signature = Authentication.sign connection request in 
  let content = ("Signature", signature) :: request in 
  Http_client.post_urlencoded ~host:connection.http_host ~uri:connection.http_uri ~content () 
  >>= extract_string 
 

  
