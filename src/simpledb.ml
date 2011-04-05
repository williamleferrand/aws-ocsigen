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
  

let put_attributes connection ?(replace=false) ?(encode=false) domain item attributes = 
    let _, atts = List.fold_left
      (fun (c, acc) (name, value) -> 
	(c+1), 
	((Printf.sprintf "Attribute.%d.Name" c), name) :: 
	  ((Printf.sprintf "Attribute.%d.Value" c), (if encode then Netencoding.Base64.encode value else value)) :: 
	  (if replace then ((Printf.sprintf "Attribute.%d.Replace" c), "true") :: acc else acc)) (1, []) attributes in
    
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
	("ConsistentRead", "true") :: 
	("Timestamp", current_timestamp ()) :: 
	("Version", "2009-04-15") :: [] in 
    
    let signature = Authentication.sign connection request in
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
       ("ConsistentRead", "true") :: 
       ("Timestamp", (current_timestamp ())) ::
       ("Version", "2009-04-15") :: (match token with None -> [] | Some id -> [ "NextToken", id ]) in
   
   let signature = Authentication.sign connection request in
   let content = ("Signature", signature) :: request in 
   Http_client.post_urlencoded ~host:connection.http_host ~uri:connection.http_uri ~content () 
   >>= extract_string 

(* 
rO0ABXNyACdjb20uYW1hem9uLnNkcy5RdWVyeVByb2Nlc3Nvci5Nb3JlVG9rZW7racXLnINNqwMAC0kAFGluaXRpYWxDb25qdW5jdEluZGV4WgAOaXNQYWdlQm91bmRhcnlKAAxsYXN0RW50aXR5SURa
AApscnFFbmFibGVkSQAPcXVlcnlDb21wbGV4aXR5SgATcXVlcnlTdHJpbmdDaGVja3N1bUkACnVu
aW9uSW5kZXhaAA11c2VRdWVyeUluZGV4TAANY29uc2lzdGVudExTTnQAEkxqYXZhL2xhbmcvU3Ry
aW5nO0wAEmxhc3RBdHRyaWJ1dGVWYWx1ZXEAfgABTAAJc29ydE9yZGVydAAvTGNvbS9hbWF6b24v
c2RzL1F1ZXJ5UHJvY2Vzc29yL1F1ZXJ5JFNvcnRPcmRlcjt4cAAAAAAAAAAAAAAAAIMAAAAAAQAA
AAAePUXOAAAAAAFwdAALMTI3OTA1MDg0Ni5+cgAtY29tLmFtYXpvbi5zZHMuUXVlcnlQcm9jZXNz
b3IuUXVlcnkkU29ydE9yZGVyAAAAAAAAAAASAAB4cgAOamF2YS5sYW5nLkVudW0AAAAAAAAAABIA
AHhwdAAKREVTQ0VORElOR3g=

*)
