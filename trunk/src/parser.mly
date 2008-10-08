%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE EOF LINECOMMENT EOL
%token OPENBLOCKCOMMENT CLOSEBLOCKCOMMENT OPENPAREN CLOSEPAREN
%token OPENBLOCK CLOSEBLOCK AT DOT
%token SEMICOLON PIPE COMMA
%token <string> SYMBOL VARIABLE STRING TEXT
%token <int> NUMBER

(* Comparison tokens *)
%token EQUALS GT LT GTE LTE NEQUALS

%nonassoc EQUALS
%left PLUS MINUS
%left TIMES DIVIDE


%start main
%type < Ast.expr> expr

%%

main:
  top EOF
| top top

top:
  comment
| culogRule

comment:
  LINECOMMENT TEXT EOL
| OPENBLOCKCOMMENT TEXT CLOSEBLOCKCOMMENT

culogRule:
  name OPENPAREN params CLOSEPAREN SEMICOLON
| name OPENPAREN params CLOSEPAREN block

params:
  param
| param COMMA param

param:
  VARIABLE
| SYMBOL
| NUMBER
| STRING

block:
  OPENBLOCK stmtList CLOSEBLOCK

stmtList:
  statement SEMICOLON statement
| statement PIPE      statement

statement:
  block
| ruleEval
| expr EQUALS expr
| expr NEQUALS expr
| expr GT expr
| expr GL expr
| expr GTE expr
| expr LTE expr

ruleEval:
  SYMBOL OPENPAREN params CLOSEPAREN
| VARIABLE DOT SYMBOL OPENPAREN params CLOSEPAREN


expr:
  expr PLUS   expr { Binop($1, Add, $3) }
| expr MINUS  expr { Binop($1, Sub, $3) }
| expr TIMES  expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| NUMBER           { Lit($1) }
| VARIABLE         { Var($1) }

