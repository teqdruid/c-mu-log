/* Original author:Devesh Dedhia*/

%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE NOT ASSIGN EOF COMMENT 
%token LBRACE RBRACE LPAREN RPAREN
%token ARROPEN ARRCLOSE AT DOT
%token SEMICOLON OR AND COMMA COLON QUOTE QUESTION
%token <string> ID VARIABLE STRING 
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
  culogRule 		{ $1 }	/* the program consists of facts rules and directives*/
| culogFact 		{ $1 }
| culogDirective   	{ $1 }

culogFact:			/*wall(2,2);*/ 
  ID LPAREN param_list RPAREN SEMICOLON		{ Fact($1, Params(List.rev $3) ) }


culogRule:			/* wall(){}*/	
  ID LPAREN param_list RPAREN block		{ Rule($1, Params(List.rev $3), $5 ) }

culogDirective:			/*@print(" these are global directives");*/
 AT ID LPAREN param_list RPAREN SEMICOLON 	{ GlobalDirective($2, Params(List.rev $4)) }


param_list:			
  {[]}
| param		      		{[$1]}
| param_list COMMA param	{$3::$1} /* Params seprated by Commas*/

param:
  VARIABLE		{ Var($1) }	/* $x,$agent*/
| ID                    { Sym($1) }	/* symbA */
| DIGIT 		{ Lit($1) }	/* 0...9*/
| PLUS DIGIT		{ Lit($2) }	/* +0...+9*/
| MINUS DIGIT		{ Lit(-1*$2) }	/* -0...-9*/
| STRING                { Str($1) } 	/* "STRINGS"*/
| array			{ Arr($1) }	/* [$x,$y]*/
| QUESTION		{ Ques }	/* ?- to indicate Anonymous variables */

array:
  ARROPEN param_list ARRCLOSE { Array( List.rev $2 ) }

block:					
  LBRACE stmt_list RBRACE { Block("AND", Stmts( $2 ) ) } /* Default reduction operator is AND*/ 
| LBRACE ID COLON stmt_list RBRACE{ Block($2, Stmts( $4 )) } /* any operator can be used*/
 	 	
stmt_list:
  /*nothing*/   	{ [] }
 | statement stmt_list  { $1 :: $2 } 		

statement:		/* statements can be sub-blocks, facts,comparison statements */
  			/* directives statements or dot operator statements*/	
  block {$1}
| ID LPAREN param_list RPAREN SEMICOLON                	{Eval($1, Params(List.rev $3)) }
| NOT ID LPAREN param_list RPAREN SEMICOLON             {NEval($2, Params(List.rev $4)) }
| VARIABLE DOT ID LPAREN param_list RPAREN SEMICOLON 	{Dot2($1,$3,Params(List.rev $5))}
| NOT VARIABLE DOT ID LPAREN param_list RPAREN SEMICOLON{NDot2($2,$4,Params(List.rev $6))}
| VARIABLE DOT AT ID LPAREN direc_list RPAREN SEMICOLON {Dot1($1,$4,(List.rev $6))}
| expr EQ expr  SEMICOLON     				{Comp($1,Eq,$3)}
| expr NEQ expr SEMICOLON				{Comp($1,Neq,$3)}
| expr GT expr  SEMICOLON				{Comp($1,Gt,$3)}
| expr LT expr	SEMICOLON				{Comp($1,Lt,$3)}
| expr GEQ expr	SEMICOLON				{Comp($1,Geq,$3)}
| expr LEQ expr	SEMICOLON				{Comp($1,Leq,$3)}
| AT ID LPAREN param_list RPAREN SEMICOLON		{Directive($2, Params(List.rev $4))}
| AT ID LPAREN direc_list_first RPAREN SEMICOLON	{DirectiveStudy($2,(List.rev $4))}

direc_list_first:
 directive SEMICOLON direc_list			 	{ $1 :: $3 }

direc_list:
  {[]}
| directive SEMICOLON direc_list		   	{ $1 :: $3 }

directive:
 ID LPAREN param_list RPAREN 				{($1,Params(List.rev $3))}
 	

expr:
    expr PLUS   expr 	{ Binop($1, Plus,   $3) }	/* $X+4*/
  | expr MINUS  expr 	{ Binop($1, Minus,  $3) }	/*3-4*/
  | expr TIMES  expr 	{ Binop($1, Mult,   $3) }	/* $x*4 */
  | expr DIVIDE expr 	{ Binop($1, Divide, $3) }	/* $x/$y */	
  | DIGIT            	{ ELit($1) } 			
  | MINUS DIGIT         { ELit(-1*$2) } 
  | PLUS DIGIT          { ELit($2) } 
  | VARIABLE         	{ EVar($1) }
  | STRING           	{ EStr($1) }
  | ID			{ EId($1) } 

