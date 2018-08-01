open Dlog_types
open Dlog
open Printf
open Utils
open Yojson.Basic.Util

let cond_map_to_string dummy_list cond_map =
  String.concat 
    "," 
    ( List.mapi
        (fun i c -> 
          if IntMap.mem i cond_map then
            IntMap.find i cond_map |> string_of_bool
          else "_")
        dummy_list )

let to_send_dlog_code (s: send) = 
  let head = sprintf "pending(mbox, %s, %s, %s, other_port) <-" 
                     s.src s.dst s.tpe
  in
  let ext_port_tail = sprintf "portId[%d]=ext_port," (extract_port_num s.cname) in
  sprintf "
        %s
          %s
          %s
          %s
          link(this, ext_port, mbox, other_port),
          pending(this,src,dst,tpe,int_port)."
          head
          ext_port_tail
          
let to_abort_dlog_code = 
  sprintf "
        abort(this) <-
          %s
          %s
          pending(this,src,dst,tpe,int_port)."


let to_lblox_send_abort_stateful action_block =
  let action_list, mbox_type_name, rec_channel_name, query_list, query_count, cond_list =
    deconstruct_action_block action_block
  in
  let dummyql = query_count |> dummyl in
  action_list |>
    List.map
      (fun action ->
        match action with
        | Send send ->
           (sprintf
              "
        pending(mbox, %s, %s, %s, other_port) <-
            portId[%d]=ext_port,
            link(this, ext_port, mbox, other_port),
            portId[%d]=int_port,
            pending(this,src,dst,tpe,int_port)%s"
              send.src
              send.dst
              send.tpe
              (extract_port_num send.cname)
              (extract_port_num rec_channel_name)
              (if 0 = List.length cond_list then ".\n"
               else
                 sprintf 
                   ",
            abstract_state_%s(this, src, dst, tpe, %s).\n"
                   mbox_type_name
                   (collect_mem_cond_map cond_list |> cond_map_to_string dummyql) ))
             
        | Abort ->
           (sprintf
              "
        abort(this) <-
            pending(this,src,dst,tpe,int_port),
            portId[%d]=int_port%s"
              (extract_port_num rec_channel_name)
              (if 0 = List.length cond_list then ".\n"
               else
                 sprintf 
                   ",
            abstract_state_%s(this, src, dst, tpe, %s).\n"
                   mbox_type_name
                   (collect_mem_cond_map cond_list |> cond_map_to_string dummyql) ))
        | Update _ -> ""
      )
                     

let to_lblox_send_abort_stateless topo_json action_block =
  let action_list, mbox_type_name, rec_channel_name, _, _, cond_list =
    deconstruct_action_block action_block
  in
  let json_topo = if Sys.file_exists !topo_json 
                  then (Some (Yojson.Basic.from_file !topo_json)) 
                  else None in 
  let typeCount = 
    match json_topo with 
    | Some json -> json |> member "typeCount" |> to_int 
    | None -> -1
  in
  let hostMaxId = 
    match json_topo with 
    |Some json -> json |> member "connectedHosts" |> to_list
                  |> List.map (fun conn -> conn |> member "toHost" |> to_int)
                  |> List.fold_left max 0 
    | None -> -1
  in 
  let init_updates = 
    match json_topo with
    | Some json -> 
       let open Yojson.Basic.Util in
       let inits = json |> member "initialState" |> to_list 
                  |> List.filter (fun upd -> mbox_type_name = (upd |> member "mboxKind" |> to_string))
      in    
      inits |> 
        List.map 
          (fun init -> 
            init |> member "content" |> to_list 
            |> List.map (fun json_entry -> json_entry |> to_list 
                                           |> List.map 
                                                (fun r -> ( r |> member "from" |> to_int ), ( r |> member "to" |> to_int)))
            |> List.map (fun entry -> {rel_name = init |> member "relationName" |> to_string; 
                                       content = entry; 
                                       types = init |> member "type" |> to_list |> List.map to_string;
                                       mbox_id = init |> member "mboxId" |> to_int })
                        
          ) |> List.concat 
    | None -> 
       []
  in
  let int_port_tail = sprintf "portId[%d]=int_port," (extract_port_num rec_channel_name) in
  let filtered_init_l = 
    cond_list |> List.map (fun cond -> List.filter 
                                         (fun init_q -> match init_q, cond with
                                                        | {rel_name = r; _}, Mem (Variable q_rel,_,_) | {rel_name = r; _}, Neg Mem (Variable q_rel,_,_) ->
                                                           r = q_rel
                                                        | _ -> false)
                                         init_updates)
  in
  let negated_init_l = 
    List.map2 (fun init_l cond -> match cond with 
                                  | Neg Mem _ -> init_l |> List.map 
                                                             (fun init -> 
                                                               let negated_ranges = negate_range_vector init.content (0,hostMaxId) (0,typeCount) init.types in
                                                               negated_ranges |> List.map (fun r -> {init with content=r}))
                                                 |> List.flatten
                                  | _ -> init_l)
              filtered_init_l
              cond_list
  in
  let normalized_init_list = negated_init_l |> cartesian in
  let var_counter = ref 0 in 
  let condition_l = 
    List.map
      (fun init_l ->
        List.map2 
          (fun init cond -> 
            match cond with
            | Mem (Variable r, mem_vec,_) | Neg Mem (Variable r, mem_vec, _) -> 
               List.mapi
                 (fun i r -> 
                   let curr_var_count = var_counter := !var_counter + 1 ; !var_counter in
                   let range_string = sprintf "int:range(%d,%d,1,x%d), " (fst r) (snd r) curr_var_count in
                   let ith_field = match (List.nth mem_vec i) with | Variable field -> field | _ -> failwith "Error 043" in
                   let type_eq = sprintf "%sId[x%d]= %s" (List.nth init.types i) curr_var_count ith_field in
                   range_string ^ type_eq)
                 init.content
            |> String.concat ", "
            | Eq _ | Neg Eq _ -> failwith "TODO: Equality condition in stateless firewall"
            | Neg Neg _ -> failwith "Error 040: Unexpected double negation in cond (should've been normalized earlier)."
            | _ -> failwith "Error 041" )
          init_l 
          cond_list
      |> String.concat ",\n          ")
      normalized_init_list
  in
  let all_negative_conditions = cond_list |> List.for_all (fun c -> match c with 
                                                                    | Neg Mem _ -> true
                                                                    | _ -> false)
  in
  let no_inits = condition_l = [] in 
  if all_negative_conditions && no_inits
  then 
    action_list 
    |> List.map
         (fun action -> match action with
                        | Send s ->
                           to_send_dlog_code s int_port_tail ""
                        | Abort ->
                           to_abort_dlog_code int_port_tail ""
                        | Update _ -> failwith "Error 042: No update actions should appear in *stateless* middleboxes"
         )
  else
    begin
      action_list 
      |> List.map 
           (fun action -> 
             condition_l
             |> List.map
                  (fun condition ->
                    let c = if condition = "" then "" else (condition ^ ",") in
                    match action with
                    | Send s ->
                       to_send_dlog_code s int_port_tail c
                    | Abort ->
                       to_abort_dlog_code int_port_tail c
                    | Update _ -> failwith "Error 039: No update actions should appear in *stateless* middleboxes"                                        
                  )
             |> String.concat "\n"
           )
    end
  
