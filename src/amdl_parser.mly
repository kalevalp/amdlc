/* */
/* */

%{

open Ast
open Printf
open Lexing

let parse_error str = 
    let start = rhs_start_pos 1 in
    let finish = rhs_end_pos 1 in
    (Printf.sprintf 
       "(line %d: char %d..%d): parsing error" 
       start.pos_lnum 
       (start.pos_cnum - start.pos_bol) 
       (finish.pos_cnum - finish.pos_bol)) |>
    print_endline
%}

%token <bool> BOOL
%token <string> ID
%token <int> NUM
%token DO OD IF FI ABORT QMARK EMARK RIGHTARROW SQUARE AND NOT IN EQUALS SEMICOLON COMMA LPAREN RPAREN ASSIGN EOF 

%left SEMICOLON

%start mbox
%type <Ast.t> mbox

%%

mbox    : ID EQUALS DO pblocks OD EOF { Mbox ( Id $1, List.rev $4 ) }
        ;

pblocks : pblock { [$1] }
        | pblocks SQUARE pblock { $3 :: $1 }
        ;

pblock  : ID QMARK vector RIGHTARROW gc { Pblock ( Receive ( Id $1, List.rev $3 ), $5 ) }
        ;

gc      : cond RIGHTARROW action { Simp ( $1, $3 ) }
        | IF gcblock FI { Ifblock (List.rev $2) }
        ;

gcblock : gc { [$1] }
        | gcblock SQUARE gc { $3 :: $1 }
        ; 

cond    : BOOL { Bool $1 }
	| LPAREN cond AND cond RPAREN { And ( $2, $4 ) }
	| NOT cond { Not $2 }
	| atom EQUALS atom { Equality ( $1, $3 ) }
	| vector IN ID { Membership ( List.rev $1, Id $3, -1 ) }
        ;

action  : ID EMARK vector { Send ( Id $1, List.rev $3 ) }
        | ID LPAREN vector RPAREN ASSIGN cond { Update ( Id $1, List.rev $3, $6 ) }
	| ABORT { Abort }
	| action SEMICOLON action { Concat ( $1, $3 ) }
	;

vector  : atom { [$1] }
        | vector COMMA atom { $3 :: $1 }
        ;

atom    : ID { Id $1 }
        | NUM { Num $1 }
        ;

%%
