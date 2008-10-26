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
  VARIABLE		{Var($1)}
| DIGIT			{Lit($1)}
| STRING                {Str($1)} 
| ARRAY			{Arr($1)}

array:
ARROPEN param_list ARRCLOSE {Arr(List.rev $2)}

block:
  LBRACE stmt_list RBRACE {Default(List.rev $2)}
 |LBRACE ID stmt_list RBRACE{ Blk ($2, List.rev $3)}
 	 	
	
stmt_list:
  statement SEMICOLON statement

statement:
  block
| ruleEval		
| expr EQ expr		{Comp($1,eq,$3)}
| expr NEQ expr		{Comp($1,neq,$3)}
| expr GT expr		{Comp($1,gt,$3)}
| expr LT expr		{Comp($1,lt,$3)}
| expr GEQ expr		{Comp($1,geq,$3)}
| expr LEQ expr		{Comp($1,leq,$3)}

ruleEval:
  ID LPAREN params RPAREN	{Eval($1, $3)}
| VARIABLE DOT ID LPAREN params RPAREN 



expr:
  | expr PLUS   expr 	{ Binop($1, add,   $3) }
  | expr MINUS  expr 	{ Binop($1, sub,   $3) }
  | expr TIMES  expr 	{ Binop($1, mult,  $3) }
  | expr DIVIDE expr 	{ Binop($1, div,   $3) }
  | DIGIT            	{ Lit($1) } 
  | VARIABLE         	{ Var($1) }
  | STRING           	{Str($1)}

