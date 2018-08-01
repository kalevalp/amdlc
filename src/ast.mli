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

val max_query_idx    : t -> int
val ast_to_string    : t -> string
val push_neg         : bool -> t -> t
val minimize_queries : t -> t
