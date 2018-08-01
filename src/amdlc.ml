(* file: main.ml *)
open Ast
open Amdl_parser
open Amdl_tokens
open Printf
open Utils
open Project
open Dlog_types
open Dlog
open Dlog_init
open Dlog_update
open Dlog_send
       
(* Error message max id: 043 *)

let version = "0.5"

let print_ast = ref false
let print_version = ref false
let topo_json = ref "###"
let files = ref []

let specs = [
    ("-p", Arg.Set print_ast, "Prints the AST");
    ("--print_ast", Arg.Set print_ast, "Same as -p");
    ("-j", Arg.Set_string topo_json, "Set the json topology file");
    ("--json", Arg.Set_string topo_json, "Same as -j");
    ("-v", Arg.Set print_version, "Prints andl version and exists");
    ("--version", Arg.Set print_version, "Same as -v");
  ]
    
let usage = "Usage: amdlc [options] file."

let _ =
  Arg.parse 
    specs 
    (fun fname -> files := fname :: !files) 
    usage

(* ************************************************************************** *)
(* ************************************************************************** *)

let conj c1 c2 =
  match c1.action_list,
        c1.mbox_type_name,
        c1.query_list,
        c1.query_count,
        c1.rec_channel_name,
        c1.cond_list,
        c2.cond_list with
  | None, None, None, None, None, Some c1cl, Some c2cl ->
     {c2 with cond_list = Some (c1cl @ c2cl)}
  | None, None, None, None, None, Some c1cl, None ->
     {c2 with cond_list = Some (c1cl)}
  | None, None, None, None, None, None , _ ->
     c2 
  | _ -> failwith "Error 001"


(* ************************************************************************** *)

let rec to_action_block_list node =
  match node with
  | Mbox (id, pblocks) -> 
     pblocks |> List.map to_action_block_list |> List.concat |>
       List.map
         (fun block ->
           match block.mbox_type_name, block.query_list, block.query_count with
           | None, None, None ->
              {block with
                mbox_type_name = Some (ast_to_string id);
                query_list = Some (collect_queries node);
                query_count = Some (1 + max_query_idx node)}
           | _ -> failwith "Error 004")
  | Pblock (Receive (Id id,_ ), gc) ->     
     to_action_block_list gc |>
       List.map (fun block -> match block.rec_channel_name with
                               | None -> {block with rec_channel_name = Some id}
                               | _ -> failwith "Error 005")
  | Simp (c1, c2)   | And (c1, c2) -> 
     product (to_action_block_list c1) (to_action_block_list c2) |>
       List.map (fun (cc1, cc2) -> conj cc1 cc2)
  | Concat (c1, c2) | Or (c1, c2) ->
     (to_action_block_list c1) @ (to_action_block_list c2)
  | Ifblock gcs ->
     gcs |> List.map (fun gc -> to_action_block_list gc) |> List.concat
  | Send (Id id, packet) ->
     (match packet with
      |  Id s ::  Id d ::  Id t :: [] ->
          [{ action_list = Some [Send {cname = id;src = s; dst = d; tpe = t}];
             mbox_type_name = None;
             query_list= None;
             query_count= None;
             cond_list= None;
             rec_channel_name= None }]
      | _ ->
         failwith "Error 006: Illegal packet"
     )
  | Update (Id rname, tuple, Bool cond) ->
     [{ action_list = Some [ Update {rel_name = rname;
                                     tuple = List.map (fun n -> match n with 
                                                                | Id d -> d
                                                                | Num n -> string_of_int n
                                                                | _ -> "" ) 
                                                      tuple;
                                     value = cond }];
        mbox_type_name = None;
        query_list= None;
        query_count= None;
        cond_list= None;
        rec_channel_name= None }]
  | Abort ->
     [{ action_list = Some [ Abort ];
        mbox_type_name = None;
        query_list= None;
        query_count= None;
        cond_list= None;
        rec_channel_name= None }]

  | Equality (c1, c2) ->
     [{ action_list = None;
        mbox_type_name = None;
        query_list= None;
        query_count= None;
        cond_list= Some [Eq (get_atom c1, get_atom c2)];
        rec_channel_name= None }]
  | Not Equality (c1, c2) ->
     [{ action_list = None;
        mbox_type_name = None;
        query_list= None;
        query_count= None;
        cond_list= Some [Neg (Eq (get_atom c1, get_atom c2))];
        rec_channel_name= None }]
  | Membership (packet, relation, idx) ->
     [{ action_list = None;
        mbox_type_name = None;
        query_list= None;
        query_count= None;
        cond_list= Some [Mem (get_atom relation, List.map (fun node -> get_atom node) packet, idx)];
        rec_channel_name= None }]       
  | Not Membership (packet, relation, idx) ->
     [{ action_list = None;
        mbox_type_name = None;
        query_list= None;
        query_count= None;
        cond_list= Some [Neg (Mem (get_atom relation, List.map (fun node -> get_atom node) packet, idx))];
        rec_channel_name= None }]       
  | Bool true ->
     [{ action_list = None;
        mbox_type_name = None;
        query_list= None;
        query_count= None;
        cond_list= Some [];
        rec_channel_name= None }]       
  | Update _ ->
     failwith "Error 009: Unexpected update expression"
  | Not _ | Send _ | Num _ | Receive _ | Id _ | Bool _ | Pblock _ ->
     []
             


let has_updates action_block = 
  let action_list, _, _, _, _, _ =
    deconstruct_action_block action_block
  in
  List.exists (fun act -> match act with | Update _ -> true | _ -> false) action_list

              
let compile fname =
  let ast = fname |> open_in |>
              Lexing.from_channel |> Amdl_parser.mbox Amdl_tokens.token |> (* Run parser *)
              push_neg false |> minimize_queries
  in 
  let mboxt = match ast with 
    | Mbox (Id name,_) -> name 
    | _ -> failwith "Error 020: Unexpected AST node."
  in  
  let mboxf =
    if not (Sys.file_exists mboxt)
    then
      Unix.mkdir mboxt 0o777
    else 
      if not (Sys.is_directory mboxt)
      then
        failwith ("Error 021: Failed trying to create directory " ^ mboxt ^ " file exists instead.");
    open_out (sprintf "%s/%s_mbox.logic" mboxt mboxt)
  in
  begin
    if (!print_ast) then
      ast |> 
        ast_to_string |> 
        print_endline;
    let action_blocks = ast |> to_action_block_list in
    let stateful = action_blocks 
                    |> List.exists (fun a -> match a.action_list with 
                                             | Some l -> l |> List.exists 
                                                                (fun act -> match act with
                                                                            | Update _ -> true
                                                                            | _ -> false)
                                             | _ -> false ) 
    in
    let to_lblox_send_abort = if stateful 
                              then to_lblox_send_abort_stateful 
                              else to_lblox_send_abort_stateless topo_json 
    in
    fprintf 
      mboxf
      "
block(`%s_mbox) {

    alias_all(`core:network),

    export(`{
        %s(mbox) ->
            middlebox(mbox).
    }),

    clauses(`{
        // State initialization 
        %s

        // Pending rules
%s

        // State rules
%s

        })
} <-- .
"
      mboxt
      mboxt
      ( if stateful then to_state_init ast mboxt topo_json
        else "" )
      (action_blocks |>
         List.map to_lblox_send_abort |>
         List.concat |>
         String.concat "\n" )
      ( action_blocks |> 
          List.filter has_updates |>
          List.mapi to_lblox_update |>
          List.concat |>
          String.concat "\n" )
  end

let _ = 
  if !print_version then
    print_endline ("amdlc version " ^ version)
  else
    begin
      begin
        match !files with
        | [] -> fprintf stderr "No amdl files were passed.\n%s" (Arg.usage_string specs usage); exit(1)
        | file_list ->
           List.iter compile file_list
      end;
      begin 
        match !topo_json with
        | "###" -> ()
        | topo -> 
           Yojson.Basic.from_file topo |> create_project !files
      end
    end
