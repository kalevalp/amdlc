open Dlog_types
open Printf
open Utils
open Ast
       
module IntMap = Map.Make(struct type t = int let compare = compare end)

let extract_port_num channel = 
    let r = Str.regexp "port_\\([0-9]+\\)" in
    Str.replace_first r "\\1" channel |> int_of_string

let collect_mem_cond_map condl =
  List.fold_left
    (fun m cond ->
      match cond with
      | Mem (_, _, i) ->
         IntMap.add i true m
      | Neg Mem (_, _, i) ->
         IntMap.add i false m
      | _ ->
         m
    )
    IntMap.empty
    condl

let deconstruct_action_block action_block = 
    match action_block.action_list,
          action_block.mbox_type_name,
          action_block.rec_channel_name,
          action_block.query_list,
          action_block.query_count,
          action_block.cond_list with
    | None, _, _, _, _, _ 
      | _, None, _, _, _, _
      | _, _, None, _, _, _
      | _, _, _, None, _, _
      | _, _, _, _, None, _
      | _, _, _, _, _, None -> 
       failwith "Error 010"
    
    | Some l, Some mboxt, Some rec_cname, Some qlist, Some qcount, Some clist -> 
       l, mboxt, rec_cname, qlist, qcount, clist

let to_value_match_condition negate update_vec query_vec = 
  let match_pairs = zip update_vec query_vec
  in
  let field_names = [ "src"; "dst"; "tpe" ]
  in
  let field_types = [ "src", "host"; "dst", "host"; "tpe", "type" ]
  in
  ( List.map 
    (fun pair -> 
      match pair with
      | u_val, Variable q_val ->
         if not (List.mem q_val field_names) then
           failwith "Error 030: Unexpected variable name: " ^ q_val
         else if List.mem u_val field_names then (* Variable in both query and update *)
           if u_val <> q_val && (u_val = "tpe" || q_val = "tpe") then 
             failwith ("Error 028: Type mismatch between query value (" ^ q_val ^ ") and update value (" ^ u_val ^ ")")
           else
             sprintf "%s %s= %s_o"
                     u_val
                     (if negate then "!" else "")
                     q_val
         else              (* Variable in query, constant in update *)
           let _ = 
             try ignore (int_of_string u_val)
             with Failure _ -> 
               failwith ("Error 031: Unexpected value in update: " ^ u_val)
           in
           sprintf "%sId[%s] %s= %s_o"
                   (List.assoc q_val field_types)
                   u_val
                   (if negate then "!" else "")
                   q_val
      | u_val, Value q_val ->               (* Value in query *)
         if List.mem u_val field_names then (* Variable in update *)
           sprintf "%sId[%d] %s= %s, %sId[%d] %s= %s_o"
                   (List.assoc u_val field_types)
                   q_val
                   (if negate then "!" else "")
                   u_val
                   (List.assoc u_val field_types)
                   q_val
                   (if negate then "!" else "")
                   u_val
         else                   (* Value in both query and update *)                               
           let u_v =              
             try int_of_string u_val
             with Failure _ -> 
               failwith ("Error 032: Unexpected value in update: " ^ u_val)
           in
           string_of_bool (u_v = q_val) (* No conditions on the packets *)
           
            
    )
    match_pairs |> 
    String.concat ", " ) ^ ","

let to_value_match_condition_range update_vec query_vec = 
  let match_pairs = zip update_vec query_vec
  in
  let field_names = [ "src"; "dst"; "tpe" ]
  in
  let field_types = [ "src", "host"; "dst", "host"; "tpe", "type" ]
  in
  ( List.map 
    (fun pair -> 
      match pair with
      | (r_from, r_to), Variable q_val ->
         if not (List.mem q_val field_names) then
           failwith "Error 030: Unexpected variable name: " ^ q_val
         else 
           sprintf "int:range(%d,%d,1,x), %sId[x] = %s"
                   r_from r_to
                   (List.assoc q_val field_types)
                   q_val
      | (r_from, r_to), Value q_val ->               (* Value in query *)
         string_of_bool (r_from <= q_val && r_to >= q_val) (* No conditions on the packets *)
    )
    match_pairs |> 
    String.concat ", " ) ^ ","


let get_atom node =
  match node with
  | Id id ->
     Variable id
  | Num n ->
     Value n
  | Mbox _ | Pblock _ | Receive _ | Simp _ | Concat _ | Ifblock _ | Bool _ | Not _ | And _ | Or _ | Equality _ | Membership _ | Abort | Send _ | Update _ ->
     failwith "Error 003: Expected atom ast node (Id or Num)"
                            
let rec collect_queries node =
  match node with
  | Mbox (id, pblocks) -> 
     pblocks |> List.map collect_queries |> List.concat
  | Pblock (Receive (Id id,_ ), gc) ->     
     collect_queries gc
  | Simp (c1, c2) | And (c1, c2)   | Concat (c1, c2) | Or (c1, c2) ->
     (collect_queries c1) @ (collect_queries c2)
  | Ifblock gcs ->
     gcs |> List.map (fun gc -> collect_queries gc) |> List.concat
  | Membership (packet, relation, idx) ->
     [Mem (get_atom relation, List.map (fun node -> get_atom node) packet, idx)]
  | Not Membership (packet, relation, idx) ->
     [Neg (Mem (get_atom relation, List.map (fun node -> get_atom node) packet, idx))]
  | Update _ | Not _ | Send _ | Num _ | Receive _ | Abort | Id _ | Bool _ | Pblock _ | Equality _->
     []

let negate_range (lower_bound, upper_bound) r = 
  let r_from = fst r in
  let r_to   = snd r in
  let range_at_upper = upper_bound = r_to in
  let range_at_lower = lower_bound = r_from in
  if range_at_lower && range_at_upper then []
  else if range_at_upper then [(lower_bound, r_from - 1)]
  else if range_at_lower then [(r_to + 1, upper_bound)]
  else [(lower_bound, r_from - 1);(r_to + 1, upper_bound)]
  

let negate_range_vector (range_vector : range list) (host_range : range) (type_range : range) (type_vector : string list) =
  List.map2
    (fun r t -> 
      let bounds = if t = "host" then host_range else type_range in
      negate_range bounds r)   
    range_vector
    type_vector 
  |> List.mapi
       ( fun i rl -> 
         List.map
           ( fun r -> List.mapi 
                        (fun j t -> if i = j then r else 
                                      if t = "host" then host_range
                                      else if t = "type" then type_range
                                      else failwith "Error 038: unexpected type") 
                        type_vector)
           rl
       )
  |> List.flatten
