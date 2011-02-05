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
      | `El_start ((_, local), _) when local = "SelectResult" -> list_items [] i 
      | _ -> goto_start i 
and list_items acc i = 
    lwt __input = Xmlm.input i in 
    match __input with 
      | `El_start ((_, local), _) when local = "Item" -> read_item acc "" [] i 
      | `El_start ((_, local), _) when local = "NextToken" -> read_token acc i
      | `El_end -> return (acc, None)
    | _ -> list_items acc i 
and read_item acc cname cacc i = 
    lwt __input = Xmlm.input i in 
    match __input with 
    | `El_end -> list_items ((cname, cacc)::acc) i 
    | `El_start ((_, local), _) when local = "Name" -> read_name acc cname cacc i 
    | `El_start ((_, local), _) when local = "Attribute" -> read_attribute acc cname cacc i 
    | _ -> read_item acc cname cacc i 
and read_name acc cname cacc i = 
    lwt __input = Xmlm.input i in 
    match __input with 
    | `Data d -> read_name acc d cacc i
    | `El_end -> read_item acc cname cacc i 
    | _ -> read_name acc cname cacc i 
and read_attribute acc cname cacc i = 
    lwt __input = Xmlm.input i in 
    match __input with 
    | `El_start ((_, local), _) when local = "Name" -> read_name2 acc cname cacc i 
    | `El_end -> read_item acc cname cacc i
    | _ -> read_attribute acc cname cacc i 
and read_name2 acc cname cacc i = 
    lwt __input = Xmlm.input i in 
    match __input with 
    | `Data d -> read_value acc cname cacc d i 
    | _ -> read_name2 acc cname cacc i
and read_value acc cname cacc name i =
      lwt __input = Xmlm.input i in 
    match __input with 
    | `El_start ((_, local),_) when local = "Value" -> extract_value acc cname cacc name i  
    | _ -> read_value acc cname cacc name i 
and extract_value acc cname cacc name i = 
      lwt __input = Xmlm.input i in 
    match __input with 
    | `Data d -> close_value acc cname ((name, d) :: cacc) i 
    | _ -> extract_value acc cname cacc name i 

 and close_value acc cname cacc i = 
    lwt __input = Xmlm.input i in 
match __input with 
  | `El_end -> read_attribute acc cname cacc i
  | _ -> close_value acc cname cacc i
 
 and read_token acc i = 
     lwt __input = Xmlm.input i in 
match __input with 
    | `Data d -> return (List.rev acc, Some (trim (clean d)))
    | _ -> return (acc, None)

 in
goto_start i
      
      
