/*
 * parser.mly
 *
 *     bogoscheme parser.
 *
 */

%{

(* header *)

%}
  
/* declarations */

%token TOK_LPAREN TOK_RPAREN
%token          TOK_UNIT
%token <bool>   TOK_BOOL
%token <int>    TOK_INT
%token <string> TOK_ID
%token          TOK_EOF

%start parse
%type <Sexpr.expr option> parse
%type <Sexpr.expr>        sexpr
%type <Sexpr.atom>        atom
%type <Sexpr.expr list>   slist
%type <Sexpr.expr list>   sexpr_list

%%

/* rules */

parse:
  | TOK_EOF { None }
  | sexpr   { Some $1 }

sexpr:
  | atom  { Sexpr.Expr_atom $1 }
  | slist { Sexpr.Expr_list $1 }

atom:  
  | TOK_UNIT { Sexpr.Atom_unit }
  | TOK_BOOL { Sexpr.Atom_bool $1 }
  | TOK_INT  { Sexpr.Atom_int $1 } 
  | TOK_ID   { Sexpr.Atom_id $1 }

slist:
  | TOK_LPAREN sexpr_list TOK_RPAREN { $2 }

sexpr_list:
  | sexpr sexpr_list { $1::$2 }
  | sexpr            { $1::[] }

%%

(* trailer *)



