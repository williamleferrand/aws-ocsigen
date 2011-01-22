(*
 * aws-ocaml 0.2
 *
 * (c) 2011 William Le Ferrand
 *)

open Lwt 

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
  let i = Xmlm.make_input (`String s) in 
  
  let rec internal_loop (token, acc) i = 
    catch 
      ( fun () -> lwt __input = Xmlm.input i in
      match __input with 
	  `Dtd _ | `Data _ | `El_end -> internal_loop (token, acc) i
	| `El_start ((_, local), _) -> 
	  if local = tag then 
	    get_contents (token, acc) i 
	  else internal_loop (token, acc) i)
      (fun _ -> return (token, acc))

  and get_contents (token, acc) i = 
    catch 
      (fun () -> 
	lwt __input = Xmlm.input i in 
          match __input with 
	      `Data s -> internal_loop (token, s::acc) i
	    | `Dtd _ | `El_start _ | `El_end -> internal_loop (token, acc) i)
      (fun  _ -> return (token, acc)) in
  
  internal_loop (None, []) i
