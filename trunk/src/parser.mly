%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE ASSIGN EOF COMMENT 
%token LBRACE RBRACE LPAREN RPAREN
%token ARROPEN ARRCLOSE AT DOT
%token SEMICOLON OR AND COMMA
%token <string> ID VARIABLE STRING TEXT
%token <int> DIGIT

/* Comparison tokens */
%token EQ GT LT GEQ LEQ NEQ

%nonassoc EQ
%left ASSIGN NEQ
%left LT GT LEQ GEQ
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

/*comment:
  COMMENT TEXT SEMICOLON
| OPENBLOCKCOMMENT TEXT CLOSEBLOCKCOMMENT*/

culogRule:
  ID LPAREN params RPAREN SEMICOLON
| ID LPAREN params RPAREN block


params:
  param
| param COMMA param

param:
  VARIABLE
| ID
| DIGIT
| STRING

block:
  LBRACE stmtList RBRACE
 |OR stmtList RBRACE
 |AND stmtList RBRACE	 	

stmtList:
  statement SEMICOLON statement

statement:
  block
| ruleEval
| expr EQ expr
| expr NEQ expr
| expr GT expr
| expr GL expr
| expr GEQ expr
| expr LEQ expr

ruleEval:
  ID LPAREN params RPAREN
| VARIABLE DOT ID LPAREN params RPAREN



expr:
  expr PLUS   expr { Binop($1, Add, $3) }
| expr MINUS  expr { Binop($1, Sub, $3) }
| expr TIMES  expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| DIGIT            { Lit($1) }
| VARIABLE         { Var($1) }

