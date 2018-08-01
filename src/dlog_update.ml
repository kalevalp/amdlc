open Ast
open Printf
open Dlog_types
open Dlog
open Utils
       
let to_lblox_update_rule mbox_type_name action_block_idx idx rec_channel_name cond_list query_list (u:update) q_idx q_vec upd_vals neg_cond = 
  let mem_cond_list = List.filter (fun c -> match c with | Mem _ | Neg Mem _ -> true | _ -> false ) cond_list
  in
  let eq_cond_list = List.filter (fun c -> match c with | Eq _ | Neg Eq _ -> true | _ -> false ) cond_list
  in
  let next_idx_string = if action_block_idx = -1 then ""
                        else sprintf "_%d_%d" action_block_idx ( idx + 1 )
  in
  let curr_idx_string = if action_block_idx = -1 then ""
                        else sprintf "_%d_%d" action_block_idx idx
  in
  let next_state_rel = sprintf "abstract_state_%s%s" mbox_type_name next_idx_string in
  let curr_state_rel = sprintf "abstract_state_%s%s" mbox_type_name curr_idx_string in
  let rule_head = sprintf "%s(this, src_o, dst_o, tpe_o, %s ) <-" next_state_rel upd_vals in
  let rule_tail_port = sprintf "portId[%d]=int_port," (extract_port_num rec_channel_name) in
  let rule_tail_pending = "pending(this,src,dst,tpe,int_port)," in
  let rule_tail_memb_cond = if mem_cond_list = [] then ""
                            else sprintf "%s(this, src, dst, tpe, %s ),"
                                         curr_state_rel
                                         (let cond_map = collect_mem_cond_map mem_cond_list
                                          in
                                          String.concat
                                            ","
                                            ( List.mapi (fun i c -> if IntMap.mem i cond_map then
                                                                      IntMap.find i cond_map |> string_of_bool
                                                                    else
                                                                      "_") query_list )) (* Condition on incoming packet *)
  in
  let rule_tail_eq_cond = 
    if eq_cond_list = [] then ""
    else (String.concat ", "
                        (List.map
                           (fun c ->
                             let host_fields = ["src"; "dst"] in
                             let type_fields = ["tpe"] in
                             let c_op = match c with | Neg _ -> false | _ -> true in
                             let op = if c_op <> neg_cond then " = " else " != " in
                             match c with
                             | Eq (Variable v, Value vl) | Neg Eq (Variable v, Value vl) ->
                                if List.mem v host_fields then sprintf "hostId[%d]%s%s" vl op v 
                                else if List.mem v type_fields then sprintf "typeId[%d]%s%s" vl op v 
                                else failwith ("Error 038: Illegal variable name in equality expression. Variable must be one of: " ^ (String.concat ", " (host_fields @ type_fields)) ^ ".")
                             | _ ->
                                failwith "Error 037: Unexpected condition type. Expected equality of the form <Variable> = <Value>."
                           ) eq_cond_list)
         ) ^ ","
  in
  let rule_tail_value_match_cond = to_value_match_condition neg_cond u.tuple q_vec  in
  let rule_tail_curr_other_packet_state = sprintf "%s(this, src_o, dst_o, tpe_o, %s)."
                                                  curr_state_rel
                                                  (String.concat "," (List.mapi
                                                                        (fun i c -> if i = q_idx then "_"
                                                                                    else "prev_" ^ string_of_int i )
                                                                        query_list))
  in

  sprintf "
        %s
            %s
            %s
            %s
            %s
            %s
            %s"
          rule_head
          rule_tail_port
          rule_tail_pending
          rule_tail_memb_cond
          rule_tail_eq_cond
          rule_tail_value_match_cond
          rule_tail_curr_other_packet_state


(* *************************************************************************************** *)
(* *************************************************************************************** *)

let to_lblox_update action_block_idx action_block = 
  let action_list, mbox_type_name, rec_channel_name, query_list, query_count, cond_list =
    deconstruct_action_block action_block
  in
  let update_pairs = 
    product action_list query_list |>
        List.filter
          (fun (a, c) ->
            match a, c with
            | Update u, Mem (Variable rel_name,_,_) | Update u, Neg Mem (Variable rel_name,_,_)  ->
               u.rel_name = rel_name
            | _ -> false)
  in
  let dummyql = query_count |> dummyl in
  match update_pairs with
  | [] ->                       (* No updates *)
     []
  | (Update u, query) :: [] -> (* Single update *)
     [ 
       let neg, q_vec, q_idx = match query with | Mem (_, q_vec,q_idx)  -> 
                                                   false, q_vec, q_idx
                                                | Neg Mem (_,q_vec,q_idx) ->
                                                   true, q_vec, q_idx
                                                | _ -> 
                                                   failwith "Error 034"
       in

       to_lblox_update_rule 
         mbox_type_name 
         (-1) (-1) 
         rec_channel_name 
         cond_list 
         dummyql 
         u 
         q_idx
         q_vec 
         (String.concat "," (List.mapi
                               (fun i c ->
                                 if i = q_idx
                                 then string_of_bool ( neg <> u.value )
                                 else "prev_" ^ string_of_int i)
                               dummyql)) (* Updated values *)
         false        
     ]
  | _ :: _ :: _ ->                        (* Multiple updates *)
     ( sprintf 
         "
         abstract_state_%s_%d_0(this, src, dst, tpe, %s ) <-
         abstract_state_%s(this, src, dst, tpe, %s ).
         "
         mbox_type_name
         action_block_idx
         (String.concat "," (List.mapi (fun i c -> "prev_" ^ string_of_int i) dummyql))
         mbox_type_name
         (String.concat "," (List.mapi (fun i c -> "prev_" ^ string_of_int i) dummyql))
     )
     ::
       ( sprintf 
           "
           abstract_state_%s(this, src, dst, tpe, %s ) <-
           abstract_state_%s_%d_%d(this, src, dst, tpe, %s ).
           "
           mbox_type_name
           (String.concat "," (List.mapi (fun i c -> "prev_" ^ string_of_int i) dummyql))
           mbox_type_name
           action_block_idx
           (List.length update_pairs)
           (String.concat "," (List.mapi (fun i c -> "prev_" ^ string_of_int i) dummyql))
       )
     ::
       ( List.mapi
           (fun idx act -> 
             let u, q_idx, q_vec, neg = 
               match act with
               | Update u, Mem (Variable rel_name,q_vec,q_idx) ->
                  u, q_idx, q_vec, false
               | Update u, Neg Mem (Variable rel_name,q_vec,q_idx)  ->
                  u, q_idx, q_vec, true
               | _ -> failwith "Error 027"
             in
             let wrapper = 
               to_lblox_update_rule mbox_type_name action_block_idx idx rec_channel_name cond_list dummyql u q_idx q_vec
             in
             ( wrapper
                 (String.concat "," (List.mapi 
                                       (fun i c -> 
                                         if i = q_idx 
                                         then string_of_bool ( neg <> u.value )
                                         else "prev_" ^ string_of_int i) 
                                       dummyql)) (* Updated values *)
                 false 
             ) 
             ^ ( wrapper
                   (String.concat "," (List.mapi 
                                         (fun i c -> 
                                           "prev_" ^ string_of_int i) 
                                         dummyql)) (* Updated values *)
                   true)
           )
           update_pairs )
  | _ -> 
     failwith "Error 035"

          
