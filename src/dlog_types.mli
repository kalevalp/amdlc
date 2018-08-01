type atom = 
  | Variable of string
  | Value of int

type cond =
  | Eq of atom * atom
  | Mem of atom * atom list * int
  | Neg of cond
             
type action =
  | Send of send
  | Update of update
  | Abort
 and send =
   { cname: string; src: string; dst: string; tpe: string; }
 and update =
   { rel_name: string; tuple: string list; value: bool; }

type range = int * int
     
type init_update = 
  { rel_name: string; content: range list; types: string list; mbox_id: int}
   
type action_block = {
    action_list: action list option;
    mbox_type_name: string option;
    query_list: cond list option;
    query_count: int option;
    cond_list: cond list option;
    rec_channel_name: string option}
