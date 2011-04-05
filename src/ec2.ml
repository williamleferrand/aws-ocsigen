(*
 * aws-ocaml 0.2
 *
 * (c) 2011 William Le Ferrand
 *)

open Lwt
open Connection
open Misc

(* DESCRIBE IMAGES ****************************************************************)

let describe_images connection image_ids = 
  let rec image_list n = function 
    | [] -> []  
    | t::q -> ((Printf.sprintf "ImageId.%d" n), t) :: (image_list (n+1) q) in
  let request = 
    ("AWSAccessKeyId", connection.access_key_id) :: 
      ("Action", "DescribeImages") ::
      ("SignatureMethod", "HmacSHA1") ::
      ("SignatureVersion", "2") ::
      ("Expires", expiration 180.0) :: 
      ("Version", "2009-11-30") :: (image_list 1 image_ids) in 
  let signature = Authentication.sign connection request in
  let content = ("Signature", signature) :: request in
  Http_client.post_urlencoded ~host:connection.http_host ~uri:connection.http_uri ~content () 
  >>= extract_string 
  
(* DESCRIBE INSTANCES *************************************************************)

let describe_instances connection instance_ids = 
   let rec instance_list n = function 
	  [] -> [] 
	| t::q -> ((Printf.sprintf "InstanceId.%d" n), t) :: (instance_list (n+1) q) in
   let request = 
     ("AWSAccessKeyId", connection.access_key_id) :: 
       ("Action", "DescribeInstances") ::
       ("SignatureMethod", "HmacSHA1") ::
       ("SignatureVersion", "2") ::
       ("Expires", expiration 180.0) :: 
       ("Version", "2009-11-30") :: (instance_list 1 instance_ids) in 
   let signature = Authentication.sign connection request in
   let content = ("Signature", signature) :: request in
   Http_client.post_urlencoded ~host:connection.http_host ~uri:connection.http_uri ~content () 
   >>= extract_string 

(* RUN INSTANCES ******************************************************************)

let run_instances connection image_id min_count max_count instance_type key_name user_data = 
  let encoder = Cryptokit.Base64.encode_compact () in 
  let user_data_encoded = Cryptokit.transform_string encoder (user_data^"   ") in
  let request = 
    ("AWSAccessKeyId", connection.access_key_id) :: 
      ("Action", "RunInstances") ::
      ("DisableApiTermination", "false") ::
      ("SignatureMethod", "HmacSHA1") ::
      ("SignatureVersion", "2") ::
      ("Expires", expiration 60.0) :: 
      ("InstanceType", instance_type) :: 
      ("MaxCount", (string_of_int max_count)) :: 
      ("MinCount", (string_of_int min_count)) :: 
      ("ImageId", image_id) ::
      ("UserData", user_data_encoded) ::
      ("KeyName", key_name) ::
      ("Version", "2009-11-30") :: [] in 
  let signature = Authentication.sign connection request in
  let content = ("Signature", signature) :: request in
  Http_client.post_urlencoded ~host:connection.http_host ~uri:connection.http_uri ~content () 
  >>= extract_string 
    
(* TERMINATE INSTANCES ************************************************************)

let terminate_instances connection instance_ids =
  let rec instance_list n = function 
    | [] -> [] 
    | t::q -> ((Printf.sprintf "InstanceId.%d" n), t) :: (instance_list (n+1) q) in
  let request = 
    ("AWSAccessKeyId", connection.access_key_id) :: 
      ("Action", "TerminateInstances") ::
      ("SignatureMethod", "HmacSHA1") ::
      ("SignatureVersion", "2") ::
      ("Expires", expiration 60.0) :: 
      ("Version", "2009-11-30") :: (instance_list 1 instance_ids) in
  let signature = Authentication.sign connection request in
  let content = ("Signature", signature) :: request in
  Http_client.post_urlencoded ~host:connection.http_host ~uri:connection.http_uri ~content () 
  >>= extract_string 
    
(* LOCAL DETAILS ******************************************************************)
  
let local_details_ipv4 () = 
  Ocsigen_http_client.get ~host:"169.254.169.254" ~uri:"/latest/meta-data/local-ipv4" ()
  >>= extract_string 

let local_details_instance_id () =
  Ocsigen_http_client.get ~host:"169.254.169.254" ~uri:"/latest/meta-data/instance-id" ()
  >>= extract_string 
 
(* GET http://169.254.169.254/latest/user-data *)
let local_details_user_data () = 
  Ocsigen_http_client.get ~host:"169.254.169.254" ~uri:"/latest/user_data" ()
  >>= extract_string 
  
