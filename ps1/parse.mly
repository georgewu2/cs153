/* Parser for Fish --- TODO */

%{
open Ast
open Lexing
(* use this to get the line number for the n'th token *)
let rhs n =
  let pos = Parsing.rhs_start_pos n in
  pos.pos_lnum
let parse_error s =
  let pos = Parsing.symbol_end_pos () in
  let l = pos.pos_lnum in
  print_string ("line "^(string_of_int l)^": "^s^"\n") 
%}

/* Tells us which non-terminal to start the grammar with. */
%start program

/* This specifies the non-terminals of the grammar and specifies the
 * types of the values they build. Don't forget to add any new non-
 * terminals here.
 */
%type <Ast.program> program
%type <Ast.stmt> stmt
%type <Ast.rstmt> rstmt
%type <Ast.exp> exp
%type <Ast.rexp> rexp

/* The %token directive gives a definition of all of the terminals
 * (i.e., tokens) in the grammar. This will be used to generate the
 * tokens definition used by the lexer. So this is effectively the
 * interface between the lexer and the parser --- the lexer must
 * build values using this datatype constructor to pass to the parser.
 * You will need to augment this with your own tokens...
 */
%token <int> INT 
%token <string> VAR
%token EOF
%token SEMI
%token RETURN
%token PLUS MINUS
%token STAR SLASH
%token LPAREN RPAREN LBRACE RBRACE
%token EQUAL NEQUAL LT LTE GT GTE
%token NOT AND OR
%token ASSIGN

%left PLUS MINUS
%left STAR SLASH
%left UMINUS
%left AND OR
%left NOT

/* Here's where the real grammar starts -- you'll need to add 
 * more rules here... Do not remove the 2%'s!! */
%%

program:
  stmt EOF { $1 }

stmt :
  /* empty */ { (Ast.skip, 0) } 
  | LBRACE stmt RBRACE { $2 }
  | rstmt { ($1, rhs 1) }

rstmt :
  | exp { Exp($1) }
  | stmt SEMI stmt { Seq($1,$3) }
  | RETURN exp SEMI { Return($2) }

exp :
  | rexp { ($1, rhs 1) }

rexp :
  | INT { Int($1) }
  | VAR { Var($1) }
  | LPAREN rexp RPAREN { $2 }
  | VAR ASSIGN exp { Assign($1, $3) }
  | NOT exp { Not($2) }
  | binop { $1 }

binop :
  | exp AND exp { And($1,$3) }
  | exp OR exp { Or($1,$3) }
  | exp EQUAL exp { Binop($1, Eq, $3) }
  | exp NEQUAL exp { Binop($1, Neq, $3) }
  | exp LT exp { Binop($1, Lt, $3) }
  | exp LTE exp { Binop($1, Lte, $3) }
  | exp GT exp { Binop($1, Gt, $3) }
  | exp GTE exp { Binop($1, Gte, $3) }
  | exp PLUS exp { Binop($1, Plus, $3) }
  | exp MINUS exp { Binop($1, Minus, $3) }
  | MINUS exp %prec UMINUS { Binop((Int(0), rhs 1), Minus, $2) }
  | exp STAR exp { Binop($1, Times, $3) }
  | exp SLASH exp { Binop($1, Div, $3) }
