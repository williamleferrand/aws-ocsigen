(*
 * aws-ocaml 0.2
 *
 * (c) 2011 William Le Ferrand
 *)

type connection =
    { 
      http_method : string ; 
      http_host : string ; 
      http_uri : string ;
      secret_access_key : string ; 
      access_key_id : string; 
    } 
