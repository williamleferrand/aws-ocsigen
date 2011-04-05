(*
 * aws-ocaml 0.2
 *
 * (c) 2011 William Le Ferrand
 *)

open Lwt
open Misc


(*****************************************************************************)
let post_urlencoded ?https ?port ?headers ~host ~uri ~content () =
  let raw_request = encode_cpl content in 
  (* List.map (fun (k, v) -> encode k, encode v) content in
  let raw_request = List.fold_left (fun acc (k,v) -> 
    if acc = "" then 
      Printf.sprintf "%s=%s" k v 
    else 
      Printf.sprintf "%s&%s=%s" acc k v) "" encoded_attributes in *)
    Ocsigen_http_client.post_string ?https ?port ?headers
    ~host ~uri
    ~content:raw_request
    ~content_type:("application","x-www-form-urlencoded")
    ()


let put ?(headers = Http_headers.empty) ~host ~uri ~content ~content_length () = 
  Ocsigen_pervasives.Ip_address.get_inet_addr host >>= fun inet_addr ->
   Ocsigen_http_client.raw_request
     ~http_method:Ocsigen_http_frame.Http_header.PUT
     ~content:(Some content)
     ~content_length:(Int64.of_int content_length)
     ~headers
     ~host
     ~inet_addr
     ~uri
     () ()
