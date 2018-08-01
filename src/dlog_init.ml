open Ast
open Printf
open Dlog_types
open Dlog
open Utils
open Yojson.Basic.Util
       
let to_state_init ast mboxt topo_json = 
  let updates = 
    if Sys.file_exists !topo_json then
      let topo = Yojson.Basic.from_file !topo_json
      in
      
      let open Yojson.Basic.Util in
      let inits = topo |> member "initialState" |> to_list 
                  |> List.filter (fun upd -> mboxt = (upd |> member "mboxKind" |> to_string))
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
    else
      []
  in
  let queryl = collect_queries ast in
  let init_query_pairs = 
    product updates queryl |> 
      List.filter 
        (fun (upd, query) -> 
          match upd, query with
          | {rel_name = r; _}, Mem (Variable q_rel,_,_) | {rel_name = r; _}, Neg Mem (Variable q_rel,_,_) ->
             r = q_rel
          | _ -> false
        )
  in
  let dummyql = 1 + max_query_idx ast |> dummyl in
  let head_no_init = sprintf "abstract_state_%s(this, src, dst, tpe, %s) <-" mboxt in
  let state_init = sprintf "abstract_state_%s_init_%d(this, src, dst, tpe, %s)" mboxt in
  let tail_packet_type = "host(src), host(dst), type(tpe)," in
  let tail_middlebox_id = sprintf "middleboxId[%d] = this."  in 
  let tail_neg_middlebox_id = sprintf "middleboxId[%d] != this."  in 
  let tail_middlebox_type = sprintf "%s(this)." mboxt  in 
  match queryl, init_query_pairs with
  | [], _ ->
     ""
  | _, [] ->
     sprintf "
          %s
            %s
            %s"
             (head_no_init ( dummyql |> 
                               List.map (fun _ -> "false") |>
                               String.concat ", " ))
             tail_packet_type
             tail_middlebox_type
  | _, _ ->
     sprintf "
          %s <-
            %s
            %s"
             (state_init 0
                        ( dummyql |> 
                            List.map (fun _ -> "false") |>
                            String.concat ", " ))
             tail_packet_type
             tail_middlebox_type
     ^
       ( sprintf "
          %s
            %s."
               ( head_no_init 
                   ( dummyql |> 
                       List.mapi (fun i _ -> "prev_" ^ string_of_int i) |>
                       String.concat ", " ) )
               ( state_init ( List.length init_query_pairs ) 
                            ( dummyql |> 
                                List.mapi (fun i _ -> "prev_" ^ string_of_int i) |>
                                String.concat ", " ) ) )
                            
     ^
       ( init_query_pairs |>
           List.mapi
             (fun idx (upd, query) ->
               let q_vec, q_id =
                 match query with
                 | Mem (_,q_vec,q_id) | Neg Mem (_,q_vec,q_id) ->
                    q_vec, q_id
                 | _ -> failwith "Error 033"
                                 
               in
               let pos_update = 
                 let values_tuple value = (String.concat ", " (List.mapi (fun i c -> if i = q_id then value else "prev_" ^ string_of_int i) dummyql))  in
                 sprintf "
          %s <-
            %s,
            %s
            %s"
                         ( state_init (idx + 1) ( values_tuple "true" ) )
                         ( state_init idx ( values_tuple "_" ) )
                         ( to_value_match_condition_range upd.content q_vec )
                         ( tail_middlebox_id upd.mbox_id )
               in
               let prev_tuple = (String.concat ", " (List.mapi (fun i c -> "prev_" ^ string_of_int i) dummyql)) in
               let neg_update = 
                 let json_topo = Yojson.Basic.from_file !topo_json in
                 let typeCount = json_topo |> member "typeCount" |> to_int in
                 let hostMaxId = json_topo |> member "connectedHosts" |> to_list
                                 |> List.map (fun conn -> conn |> member "toHost" |> to_int)
                                 |> List.fold_left max 0 in
                 
                 let negated_ranges = negate_range_vector upd.content (0,hostMaxId) (0,typeCount) upd.types in
                 List.map
                   ( fun neg_range -> 
                 sprintf "
          %s <-
            %s,
            %s
            %s"
                         ( state_init (idx + 1) prev_tuple ) 
                         ( state_init idx prev_tuple )
                         ( to_value_match_condition_range neg_range q_vec )
                         ( tail_middlebox_id upd.mbox_id ) )
                   negated_ranges
               |> String. concat "\n"
               in
               let other_mboxes_update = 
          (* abstract_state_acl_lb_dst_init_1(this, src, dst, tpe, prev_0) <- *)
          (*   abstract_state_acl_lb_dst_init_0(this, src, dst, tpe, prev_0), *)
          (*   middleboxId[7] != this. *)
                 sprintf "
          %s <-
            %s,
            %s"
                         ( state_init (idx + 1) prev_tuple ) 
                         ( state_init idx prev_tuple )
                         ( tail_neg_middlebox_id upd.mbox_id )
                         
                         
               in
               pos_update ^ neg_update ^ other_mboxes_update
             )
         |> String.concat "\n\n" )
