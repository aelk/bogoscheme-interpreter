(*
 * lexer.mll
 *
 *     bogoscheme Lexer.
 *
 *)

{
open Parser
}

(* Some useful definitions. *)
let whitespace = [' ' '\t' '\n']
let integer    = '-'? ['0' - '9']+
let id_chars   = ['a' - 'z' '+' '-' '*' '/' '=' '<' '>' '!']

(* The lexer definition itself. *)
rule lex = parse
  | ';' [^ '\n']*  { lex lexbuf }
  | whitespace     { lex lexbuf }
  | '('            { TOK_LPAREN }
  | ')'            { TOK_RPAREN }
  | eof            { TOK_EOF }
  | "#u"           { TOK_UNIT }
  | "#t"           { TOK_BOOL true }
  | "#f"           { TOK_BOOL false }
  | integer as i   { TOK_INT (int_of_string i) }
  | id_chars+ as id { TOK_ID id }

  (* lexer error -- this should never happen *)
  | _               { 
      raise (Failure ("unrecognized token: " ^ (Lexing.lexeme lexbuf))) 
    }

{
(* Nothing. *)
}

      
