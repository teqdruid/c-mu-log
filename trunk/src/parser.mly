%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE ASSIGN EOF COMMENT 
%token LBRACE RBRACE LPAREN RPAREN
%token ARROPEN ARRCLOSE AT DOT
%token SEMICOLON OR AND COMMA COLON QUOTE
%token <string> ID VARIABLE STRING1 
%token <int> DIGIT

/* Comparison tokens */
%token EQ GT LT GEQ LEQ NEQ

%nonassoc EQ
%left NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE


%start program
%type < Ast.program> program

%%

program:
  main { Program($1) }
  	
main:
  EOF { [] }
| top main { $1 :: $2 }

top:
  culogRule { $1 }
| culogFact { $1 }
| culogDrective {$1}


culogDrective:
   AT ID LPAREN param_list RPAREN SEMICOLON    { GlobalDirective($2, Params($4)) }
culogFact:
  ID LPAREN param_list RPAREN SEMICOLON	{ Fact($1, Params(List.rev $3) ) }

culogRule:
  ID LPAREN param_list RPAREN block	{ Rule($1, Params(List.rev $3), $5 ) }

param_list:
  {[]}
| param		      		{[$1]}
| param_list COMMA param     {$3::$1}

param:
  VARIABLE		{ Var($1) }
| ID                    { Sym($1) }
| DIGIT			{ Lit($1) }
| STRING                { Str($1) } 
| array			{ Arr($1) }

STRING: 
  QUOTE ID QUOTE	{ $2 } 
  |STRING1	{ $1 } 

 

array:
  ARROPEN param_list ARRCLOSE { Array( List.rev $2 ) }

block:
  LBRACE stmt_list RBRACE { Block("AND", Stmts( $2 ) ) } 
| LBRACE ID COLON stmt_list RBRACE{ Block($2, Stmts( $4 )) } 
 	 	
	
stmt_list:
  /*nothing*/   	{ [] }
 | statement stmt_list  { $1 :: $2 } 

statement:
  block { $1 }
| ID LPAREN param_list RPAREN SEMICOLON   { Eval($1, Params($3)) }
/* | VARIABLE DOT ID LPAREN param_list RPAREN  */
| expr EQ expr  SEMICOLON       {Comp($1,Eq,$3)}
| expr NEQ expr SEMICOLON	{Comp($1,Neq,$3)}
| expr GT expr  SEMICOLON	{Comp($1,Gt,$3)}
| expr LT expr	SEMICOLON	{Comp($1,Lt,$3)}
| expr GEQ expr	SEMICOLON	{Comp($1,Geq,$3)}
| expr LEQ expr	SEMICOLON	{Comp($1,Leq,$3)}
| AT ID LPAREN stmt_list RPAREN SEMICOLON    { DirectiveStudy($2, Stmts($4)) }
| AT ID LPAREN param_list RPAREN SEMICOLON   { Directive($2,Params($4)) }
| VARIABLE DOT AT ID LPAREN stmt_list RPAREN SEMICOLON {Dot1($1,$4,Stmts($6))}
| ID DOT ID LPAREN param_list RPAREN SEMICOLON {Dot2($1,$3, Params($5))}


expr:
  | expr PLUS   expr 	{ Binop($1, Plus,   $3) }
  | expr MINUS  expr 	{ Binop($1, Minus,  $3) }
  | expr TIMES  expr 	{ Binop($1, Mult,   $3) }
  | expr DIVIDE expr 	{ Binop($1, Divide, $3) }
  | DIGIT            	{ ELit($1) } 
  | VARIABLE         	{ EVar($1) }
  | STRING           	{ EStr($1)}

