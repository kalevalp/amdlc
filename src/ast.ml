type t =
  | Mbox of t * (t list)
  | Pblock of t * t
  | Receive of t * (t list)
  | Id of string
  | Num of int
  (* guarded-command *)
  | Simp of t * t
  | Ifblock of t list
  (* condition *)
  | Bool of bool
  | Not of t
  | And of t * t
  | Or of t * t
  | Equality of t * t
  | Membership of (t list) * t * int
  (* action *)
  | Abort
  | Send of t * (t list)
  | Update of t * (t list) * t
  | Concat of t * t

let rec max_query_idx node =
  match node with
  | Mbox (_, l) | Ifblock l ->
     List.fold_left
       (fun x node ->
         max x (max_query_idx node)
       )
       (-1)
       l
  | Pblock (_, n) | Not n | Update (_, _, n) ->
     max_query_idx n
  | Simp (n1, n2) | And (n1, n2) | Or (n1, n2)->
     max (max_query_idx n1) (max_query_idx n2)
  | Membership (_, _, idx) ->
     idx
  | Bool _ | Equality _ | Abort | Send _ | Num _ | Id _ | Receive _ | Concat _ ->
     -1

let rec ast_to_string node =
  match node with
  | Mbox (id, pblocks) ->
     ast_to_string id ^ ":=> \n  "
     ^ (pblocks |> List.map ast_to_string |> String.concat "\n[]\n  ")
     ^ "\n\nWith " ^ (1 + max_query_idx node |> string_of_int) ^ " queries.\n"
  | Id id ->
     id
  | Pblock (receive, gc) ->     
     "  " ^ ast_to_string receive ^ " -> \n" ^ "    " ^ ast_to_string gc
  | Receive (channel, packet) ->
     ast_to_string channel ^ " ? "
     ^ "(" ^ (packet |> List.map ast_to_string |> String.concat ",") ^ ")"
  | Simp (cond, act) ->
     ast_to_string cond ^ " -> " ^ ast_to_string act
  | Ifblock gcs ->
     "IF"
     ^ (gcs |> List.map ast_to_string |> String.concat "\n[]\n")
     ^ "FI"
  | Bool b ->
     string_of_bool b
  | Not cond ->
     "! (" ^ ast_to_string cond  ^ ")"
  | And (c1, c2) ->
     "(" ^ ast_to_string c1 ^ "&&" ^ ast_to_string c2 ^ ")"
  | Or (c1, c2) ->
     "(" ^ ast_to_string c1 ^ "||" ^ ast_to_string c2 ^ ")"
  | Equality (c1, c2) ->
     "(" ^ ast_to_string c1 ^ " == " ^ ast_to_string c2 ^ ")"
  | Membership (packet, relation, idx) ->
     "(" ^ (packet |> List.map ast_to_string |> String.concat ",") ^ ")"
     ^ " ∈ " ^ ast_to_string relation ^ "(* Query #" ^ string_of_int idx ^ "*)"
  | Abort ->
     "abort"
  | Send (channel, packet) ->
     ast_to_string channel ^ " ! "
     ^ "(" ^ (packet |> List.map ast_to_string |> String.concat ",") ^ ")"
  | Update (relation, entry, cond) ->
     ast_to_string relation
     ^ "(" ^ (entry |> List.map ast_to_string |> String.concat ",") ^ ")"
     ^ " := " ^ ast_to_string cond
  | Concat (act1, act2) ->
     ast_to_string act1 ^ ";" ^ ast_to_string act2
  | Num n ->
     string_of_int n

let rec push_neg negate node =
  match node with
  | Mbox (id, pblocks) ->
     Mbox (id, List.map (fun n -> push_neg false n) pblocks)
  | Pblock (receive, gc) ->
     Pblock (receive, push_neg negate gc)
  | Simp (cond, act) ->
     Simp (push_neg negate cond, push_neg negate act)
  | Concat (gc1, gc2) ->
     Concat (push_neg negate gc1, push_neg negate gc2)
  | Ifblock gcs ->
     Ifblock (List.map (fun n -> push_neg negate n) gcs)
  | Bool b ->
     Bool (b <> negate)
  | Equality _ | Membership _ ->
     if negate then
       Not node
     else
       node
  | And (c1, c2) ->
     if negate then 
       Or (push_neg negate c1, push_neg negate c2)
     else
       node
  | Or (c1, c2) ->
     if negate then
       And (push_neg negate c1, push_neg negate c2)
     else
       node
  | Not c ->
     push_neg (not negate) c
  | Update (relation, entry, cond) ->
     Update (relation, entry, push_neg negate cond)
  | Abort | Send _ | Num _ | Id _ | Receive _ -> 
     node 

let rec collect_raw_queries node =
  match node with
  | Mbox (id, pblocks) -> 
     pblocks |> List.map collect_raw_queries |> List.concat
  | Pblock (Receive (Id id,_ ), gc) ->     
     collect_raw_queries gc
  | Simp (c1, c2) | And (c1, c2)   | Concat (c1, c2) | Or (c1, c2) ->
     (collect_raw_queries c1) @ (collect_raw_queries c2)                               
  | Ifblock gcs ->
     gcs |> List.map (fun gc -> collect_raw_queries gc) |> List.concat
  | Membership _ ->
     [node]
  | Not c ->
     collect_raw_queries c
  | Update _ | Send _ | Num _ | Receive _ | Abort | Id _ | Bool _ | Pblock _ | Equality _->
     []

let rec reduce_ast_queries queries node =
  match node with
  | Mbox (id, pblocks) -> 
     Mbox(id, List.map (reduce_ast_queries queries) pblocks)
  | Pblock (receive, gc) ->     
     Pblock(receive, reduce_ast_queries queries gc)
  | Simp (c1, c2) ->
     Simp (reduce_ast_queries queries c1, reduce_ast_queries queries c2)
  | And (c1, c2) ->
     And (reduce_ast_queries queries c1, reduce_ast_queries queries c2)
  | Concat (c1, c2) ->
     Concat (reduce_ast_queries queries c1, reduce_ast_queries queries c2)
  | Or (c1, c2) ->
     Or (reduce_ast_queries queries c1, reduce_ast_queries queries c2)
  | Not c ->
     Not (reduce_ast_queries queries c)
  | Ifblock gcs ->
     Ifblock (List.map (reduce_ast_queries queries) gcs)
  | Membership (packet, relation, _) ->
     Membership (packet, relation, List.assoc node queries)
  | Update _ | Send _ | Num _ | Receive _ | Abort | Id _ | Bool _ | Equality _->
     node

   
let minimize_queries node =
  let queries = collect_raw_queries node 
                |> List.fold_left (fun l q -> if List.mem q l then l else q :: l) [] 
                |> List.rev
                |> List.mapi (fun idx q -> (q, idx))
  in
  reduce_ast_queries queries node

