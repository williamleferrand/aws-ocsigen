(*
 * aws-ocaml 0.2
 *
 * (c) 2011 William Le Ferrand
 *)

type t =
    { 
      http_method : string ; 
      http_host : string ; 
      http_uri : string ;
      secret_access_key : string ; 
      access_key_id : string; 
    } 

let create_sdb key secret = 
  {
    http_method = "POST" ; 
    http_host = "sdb.amazonaws.com" ; 
    http_uri = "/" ;
    secret_access_key = secret ;
    access_key_id = key ;
  }

let create_s3 key secret = 
  {
    http_method = "GET" ; 
    http_host = "" ; 
    http_uri = "/" ;
    secret_access_key = secret ;
    access_key_id = key ;
  }

let create_ec2 key secret = 
  {
    http_method = "GET" ; 
    http_host = "" ; 
    http_uri = "/" ;
    secret_access_key = secret ;
    access_key_id = key ;
  }

