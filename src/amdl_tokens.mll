{
open Amdl_parser
open Ast
}

let space = [' ' '\t' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
           | "//"      { comment lexbuf }
           | space+    { token lexbuf }
           | "true"    { BOOL(true) }
           | "false"   { BOOL(false) }
           | "do"      { DO }
           | "od"      { OD }
           | "if"      { IF }
           | "fi"      { FI }
           | "abort"   { ABORT }
           | '?'       { QMARK }
           | '!'       { EMARK }
           | "=>"      { RIGHTARROW }
           | "[]"      { SQUARE }
           | "and"     { AND }
           | "not"     { NOT }
           | "in"      { IN }
           | '='       { EQUALS }
           | ';'       { SEMICOLON }
           | ','       { COMMA }
           | '('       { LPAREN }
           | ')'       { RPAREN }
           | ":="      { ASSIGN }
           | eof       { EOF }
           | '\n'      { Lexing.new_line lexbuf; token lexbuf }

           | (lower|upper) (digit|lower|upper|'_'|'-')*
                { ID(Lexing.lexeme lexbuf) }

           | (digit)*
                { NUM(int_of_string (Lexing.lexeme lexbuf)) }

           | _
                { failwith
                (Printf.sprintf "unknown token %s near characters %d-%d"
                   (Lexing.lexeme lexbuf)
                   (Lexing.lexeme_start lexbuf)
                   (Lexing.lexeme_end lexbuf)) }
and comment = parse
            | '\n'      { Lexing.new_line lexbuf; token lexbuf }
	    | _         { comment lexbuf }