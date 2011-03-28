(*
 * aws-ocaml 0.2
 *
 * (c) 2011 William Le Ferrand
 *)

open Lwt 
open Misc

let rec trim s =
  let l = String.length s in 
  if l=0 then s
  else if s.[0]=' ' || s.[0]='\t' || s.[0]='\n' || s.[0]='\r' then
    trim (String.sub s 1 (l-1))
  else if s.[l-1]=' ' || s.[l-1]='\t' || s.[l-1]='\n' || s.[l-1]='\r' then
    trim (String.sub s 0 (l-1))
  else
    s

let clean s = 
  Str.global_replace (Str.regexp_string "\n") "" s 

exception NonStringContent
exception Error

(* Extract one value *)
let extract_string_of_tag tag s = 
  let i = Xmlm.make_input (`String (0, s)) in 

  let rec internal_loop i = 
    lwt __input = Xmlm.input i in
    match __input with 
	`Dtd _ | `Data _ | `El_end -> internal_loop i
      | `El_start ((_, local), _) -> 
	if local = tag then 
	  get_contents i 
	else internal_loop i 
  and get_contents i = 
  lwt __input = Xmlm.input i in 
  match __input with 
      `Data s -> return s 
    |  _ -> fail NonStringContent in 
  internal_loop i

(* Extract all values *)
let extract_string_of_tag_all tag s = 
  let i = Xmlm.make_input (`String (0, s)) in 
  
  let rec internal_loop acc i = 
    catch 
      ( fun () -> lwt __input = Xmlm.input i in
      match __input with 
	  `Dtd _ | `Data _ | `El_end -> internal_loop acc i
	| `El_start ((_, local), _) ->
	  if local = tag then 
	    get_contents acc i 
	  else internal_loop acc i)
      (fun _ -> return acc)

  and get_contents acc i = 
    catch 
      (fun () -> 
	lwt __input = Xmlm.input i in 
          match __input with 
	      `Data s -> internal_loop (s::acc) i
	    | `Dtd _ | `El_start _ | `El_end -> internal_loop acc i)
      (fun  _ -> return acc) in
  
  internal_loop [] i


(* Get an element *)

let get_attribute name s = 
  let i = Xmlm.make_input (`String (0, s)) in
  catch 
    (fun () -> 
      let rec goto_name i = 
	lwt __input = Xmlm.input i in 
        match __input with 
	  | `El_start ((_, local), _) when local = "Name" -> check_name i
	  | _ -> goto_name i 
     and check_name i = 
      lwt __input = Xmlm.input i in 
      match __input with 
	| `Data d when d = name -> goto_value i
	| _ -> goto_name i  
     and goto_value i = 
     lwt __input = Xmlm.input i in 
     match __input with
       | `El_start ((_, local), _) when local = "Value" -> extract_value i 
       | _ -> goto_value i 
     and extract_value i =
    lwt __input = Xmlm.input i in 
     match __input with 
       | `Data d -> return (Some d) 
       | _ -> extract_value i in
   goto_name i)
    (fun _ -> return None)


let get_attributes_to_list s =
  let i = Xmlm.make_input (`String (0, s)) in 
  let p = Hashtbl.create 0 in
  let rec goto_name  i = 
      lwt __input = Xmlm.input i in 
      match __input with
	 | `El_start ((_, local), _) when local = "Name" -> read_name  i 
	 | _ -> goto_name  i
  and read_name  i = 
      lwt __input = Xmlm.input i in 
      match __input with 
	| `Data d -> goto_value  d i
	| _ -> goto_name  i
  and goto_value  l i =
     lwt __input = Xmlm.input i in 
     match __input with
       | `El_start ((_, local), _) when local = "Value" -> extract_value  l i 
       | _ -> goto_value  l i 
  and extract_value  l i =
     lwt __input = Xmlm.input i in 
     match __input with 
       | `Data d -> Hashtbl.add p l d; goto_name i  
       | _ -> extract_value l i in
  catch 
    (fun () -> lwt _ = goto_name i in return p)
    (fun _ -> return p)
  
  

(* Mem a tag *)
exception Missing 

let fail_if_tag tag s = 
  let i = Xmlm.make_input (`String (0, s)) in
  catch  
    (fun () -> 
      let rec loop_over_xml i = 
	lwt __input = Xmlm.input i in 
        match __input with 
	  | `El_start ((_, local), _) when local = tag -> fail Missing
	  | _ -> loop_over_xml i in 
        loop_over_xml i >>= fun _ -> return s)
    (function Missing -> fail Missing | _ -> return s)
  
let mem_tag tag s = 
  let i = Xmlm.make_input (`String (0, s)) in
   catch  
    (fun () -> 
      let rec loop_over_xml i = 
	lwt __input = Xmlm.input i in
        match __input with 
	  | `El_start ((_, local), _) when local = tag -> return true
	  | _ -> loop_over_xml i in 
        loop_over_xml i)
    (function e -> return false)
  
(* List all sdb results for select *)

let list_attributes s = 
  let i = Xmlm.make_input (`String (0, s)) in
  let rec goto_start i = 
    lwt __input = Xmlm.input i in 
    match __input with 
      | `El_start ((_, "SelectResult"), _) -> get_item [] i 
      | _ -> goto_start i 
  and get_item acc i = 
     lwt __input = Xmlm.input i in 
     match __input with 
       | `El_start ((_, "Item"), _) -> get_name acc i
       | `El_start ((_, "NextToken"), _) -> get_token acc i
       | `El_end -> return (acc, None)
       | _ -> fail Error
  and get_name acc i = 
    lwt __input = Xmlm.input i in 
    match __input with 
      | `El_start ((_, "Name"), _) -> get_name_data acc i 
      | _ -> fail Error
  and get_name_data acc i = 
    lwt __input = Xmlm.input i in 
    match __input with 
      | `Data d -> close_name_data acc d i 
      | _ -> fail Error
  and close_name_data acc name i = 
    lwt __input = Xmlm.input i in 
    match __input with 
      | `El_end -> get_attribute acc name [] i
      | _ -> fail Error
  and get_attribute acc name lacc i =
    lwt __input = Xmlm.input i in 
    match __input with 
      | `El_start ((_, "Attribute"), _) -> get_attribute_name acc name lacc i 
      | `El_end -> get_item ((name, lacc)::acc) i
      | _ -> fail Error
  and get_attribute_name acc name lacc i = 
    lwt __input = Xmlm.input i in 
    match __input with 
      | `El_start ((_, "Name"), _ ) -> get_attribute_name_data acc name lacc i
      | _ -> fail Error
  and get_attribute_name_data acc name lacc i = 
    lwt __input = Xmlm.input i in 
    match __input with 
      | `Data d -> close_attribute_name acc name lacc d i
      | _ -> fail Error
  and close_attribute_name acc name lacc k i =
    lwt __input = Xmlm.input i in 
    match __input with 
      | `El_end -> get_attribute_value acc name lacc k i
      | _ -> fail Error
  and get_attribute_value acc name lacc k i = 
    lwt __input = Xmlm.input i in 
    match __input with 
      | `El_start ((_, "Value"), _) -> get_attribute_value_data acc name lacc k i
      | _ -> fail Error
  and get_attribute_value_data acc name lacc k i = 
    lwt __input = Xmlm.input i in 
    match __input with
      | `Data d -> close_attribute_value_data acc name ((k,d)::lacc) i 
      | `El_end -> close_attribute acc name lacc i
      | _ -> fail Error
  and close_attribute_value_data acc name lacc i = 
    lwt __input = Xmlm.input i in 
    match __input with 
      | `El_end -> close_attribute acc name lacc i
      | _ -> fail Error
  and close_attribute acc name lacc i = 
    lwt __input = Xmlm.input i in 
    match __input with 
      | `El_end -> get_attribute acc name lacc i
      | _ -> fail Error
  and get_token acc i  =
    lwt __input = Xmlm.input i in 
    match __input with 
      | `Data d -> return (acc, Some d)
      | _ -> return (acc, None)
  in

  goto_start i 
