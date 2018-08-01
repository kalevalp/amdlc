val to_lblox_update_rule : string ->
                           int ->
                           int ->
                           string ->
                           Dlog_types.cond list ->
                           'a list ->
                           Dlog_types.update -> int -> Dlog_types.atom list -> string -> bool -> string
                                                                                           
val to_lblox_update : int -> Dlog_types.action_block -> string list

