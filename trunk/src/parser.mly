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
%left NEQ
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
culogRule


culogRule:
  /* nothing */ { [] }
  ID LPAREN param_list RPAREN SEMICOLON	{ $1 ,List.rev($3) }
| ID LPAREN param_list RPAREN block	   	{   $1, List.rev($3),List.rev($4) } 


param_list:
  param		      		{[$1]}
| param_list COMMA param     {$3::$1}

param:
  VARIABLE		{variable($1)}
| DIGIT			{digits($1)}
| STRING                {string($1)} 
| ARRAY			{array($1)}

block:
  LBRACE stmt_list RBRACE {
 |OR stmt_list RBRACE
 |AND stmt_list RBRACE	 	

stmt_list:
  statement SEMICOLON statement

statement:
  block
| ruleEval
| expr EQ expr 
| expr NEQ expr
| expr GT expr
| expr LT expr
| expr GEQ expr
| expr LEQ expr

ruleEval:
  ID LPAREN params RPAREN
| VARIABLE DOT ID LPAREN params RPAREN



expr:
  | expr PLUS   expr { Binop($1, add,   $3) }
  | expr MINUS  expr { Binop($1, sub,   $3) }
  | expr TIMES  expr { Binop($1, mult,  $3) }
  | expr DIVIDE expr { Binop($1, div,   $3) }
  | expr EQ     expr { Binop($1, equal, $3) }
  | expr NEQ    expr { Binop($1, neq,   $3) }
  | expr LT     expr { Binop($1, less,  $3) }
  | expr LEQ    expr { Binop($1, leq,   $3) }
  | expr GT     expr { Binop($1, greater,  $3) }
  | expr GEQ    expr { Binop($1, geq,   $3) }
| DIGIT            { Lit($1) }
| VARIABLE         { variable($1) }

