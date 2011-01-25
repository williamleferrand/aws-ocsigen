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
  Printf.printf "Signature is %s\n" signature; flush stdout;
  let content = ("Signature", signature) :: request in 

  Http_client.post_urlencoded ~host:connection.http_host ~uri:connection.http_uri ~content () 
  >>= extract_string 
 
let delete_domain connection domain = 
    let request = 
    ("AWSAccessKeyId", connection.access_key_id) :: 
      ("Action", "DeleteDomain") ::
      ("DomainName", domain) ::
      ("SignatureMethod", "HmacSHA1") ::
      ("SignatureVersion", "2") ::
      ("Timestamp", current_timestamp ()) :: 
      ("Version", "2009-04-15") :: [] in 
  
  let signature = Authentication.sign connection request in
  Printf.printf "Signature is %s\n" signature; flush stdout;
  let content = ("Signature", signature) :: request in 
  Http_client.post_urlencoded ~host:connection.http_host ~uri:connection.http_uri ~content () 
  >>= extract_string 

let list_domain connection max_domain = 
    let request = 
    ("AWSAccessKeyId", connection.access_key_id) :: 
      ("Action", "ListDomains") ::
      ("MaxNumberOfDomains", (string_of_int max_domain)) ::
      ("SignatureMethod", "HmacSHA1") ::
      ("SignatureVersion", "2") ::
      ("Timestamp", current_timestamp ()) :: 
      ("Version", "2009-04-15") :: [] in 
  
  let signature = Authentication.sign connection request in
  Printf.printf "Signature is %s\n" signature; flush stdout;
  let content = ("Signature", signature) :: request in 
  Http_client.post_urlencoded ~host:connection.http_host ~uri:connection.http_uri ~content () 
  >>= extract_string 
  

let put_attributes connection ?(replace=false) domain item attributes = 
    let _, atts = List.fold_left
      (fun (c, acc) (name, value) -> 
	(c+1), 
	((Printf.sprintf "Attribute.%d.Name" c), name) :: 
	  ((Printf.sprintf "Attribute.%d.Value" c), value) :: (if replace then ((Printf.sprintf "Attribute.%d.Replace" c), "true") :: acc else acc)) (1, []) attributes in
    
    let request = 
      ("AWSAccessKeyId", connection.access_key_id) :: 
	("Action", "PutAttributes") ::
	("DomainName", domain) ::
	("ItemName", item) ::
	("SignatureMethod", "HmacSHA1") ::
	("SignatureVersion", "2") ::
	("Timestamp", current_timestamp ()) :: 
	("Version", "2009-04-15") :: atts in 
    
  let signature = Authentication.sign connection request in
  let content = ("Signature", signature) :: request in 
  Http_client.post_urlencoded ~host:connection.http_host ~uri:connection.http_uri ~content () 
  >>= extract_string 
  
																					  
							
let delete_attributes connection domain item attributes = 
  let _, atts = List.fold_left (fun (c, acc) (name, value) -> 
    (c+1), 
    ((Printf.sprintf "Attribute.%d.Name" c), name) :: 
      ((Printf.sprintf "Attribute.%d.Value" c), value) :: acc) (1, []) attributes in 
  
    let request = 
      ("AWSAccessKeyId", connection.access_key_id) :: 
	("Action", "DeleteAttributes") ::
	("DomainName", domain) ::
	("ItemName", item) ::
	("SignatureMethod", "HmacSHA1") ::
	("SignatureVersion", "2") ::
	("Timestamp", current_timestamp ()) :: 
	("Version", "2009-04-15") :: atts in 
    
  let signature = Authentication.sign connection request in
  Printf.printf "Signature is %s\n" signature; flush stdout;
  let content = ("Signature", signature) :: request in 
  Http_client.post_urlencoded ~host:connection.http_host ~uri:connection.http_uri ~content () 
  >>= extract_string 


let get_attributes connection domain item = 
    let request = 
      ("AWSAccessKeyId", connection.access_key_id) :: 
	("Action", "GetAttributes") ::
	("DomainName", domain) ::
	("ItemName", item) ::
	("SignatureMethod", "HmacSHA1") ::
	("SignatureVersion", "2") ::
	("Timestamp", current_timestamp ()) :: 
	("Version", "2009-04-15") :: [] in 
    
    let signature = Authentication.sign connection request in
    Printf.printf "Signature is %s\n" signature; flush stdout;
    let content = ("Signature", signature) :: request in 
    Http_client.post_urlencoded ~host:connection.http_host ~uri:connection.http_uri ~content () 
    >>= extract_string 
      
let select connection ?(token=None) request = 
   let request = 
     ("AWSAccessKeyId", connection.access_key_id) :: 
       ("Action", "Select") ::
       ("SelectExpression", request) ::
       ("SignatureMethod", "HmacSHA1") ::
       ("SignatureVersion", "2") ::
       ("Timestamp", (current_timestamp ())) ::
       ("Version", "2009-04-15") :: (match token with None -> [] | Some id -> [ "NextToken", id ]) in
   
   let signature = Authentication.sign connection request in
   Printf.printf "Signature is %s\n" signature; flush stdout;
   let content = ("Signature", signature) :: request in 
   Http_client.post_urlencoded ~host:connection.http_host ~uri:connection.http_uri ~content () 
   >>= extract_string 
