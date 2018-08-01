module IntMap : sig
  type key = int
  type +'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val remove : key -> 'a t -> 'a t
  val merge :
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal : 'a t -> int
  val bindings : 'a t -> (key * 'a) list
  val min_binding : 'a t -> key * 'a
  val max_binding : 'a t -> key * 'a
  val choose : 'a t -> key * 'a
  val split : key -> 'a t -> 'a t * 'a option * 'a t
  val find : key -> 'a t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
end
                       
val extract_port_num : string -> int
val collect_mem_cond_map : Dlog_types.cond list -> bool IntMap.t                                 
val deconstruct_action_block : Dlog_types.action_block ->
                               Dlog_types.action list * string * string * Dlog_types.cond list * int * Dlog_types.cond list
val to_value_match_condition : bool -> string list -> Dlog_types.atom list -> string
val to_value_match_condition_range : Dlog_types.range list -> Dlog_types.atom list -> string
val get_atom : Ast.t -> Dlog_types.atom
val collect_queries : Ast.t -> Dlog_types.cond list
val negate_range : Dlog_types.range -> Dlog_types.range -> Dlog_types.range list
val negate_range_vector : Dlog_types.range list ->
                          Dlog_types.range ->
                          Dlog_types.range -> string list -> Dlog_types.range list list
