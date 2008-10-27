type operator = PLUS | MINUS | MULTIPLY | DIVIDE 
type compoperator= LT |LEQ |GT | GEQ | EQ | NEQ

type study = @learn | @forget

type stmts=Stmts of stmt list  /* statment1;statment2;statement3; */ 
type Block = Blk of id*stmt list
	    |Default of stmt list	
type stmt=
	 Blk of stmts                 /* {.....} */
	|Comp of expr*compoperator*expr		$5+5<$4					/* $a=5,$b=6; */
	|Fct of id*params								/* wall(4,5) */
	|Direc of id*params       /*@attach("dfsfsa")*/ /*@print("ddafafa")*/
	|Modif of study*stmts          /*@learn(wall(4,5);wall(5,6))*/
	|Dot1 of variable*stmt              /*$agent.@learn() */
	|Dot2 of id*stmt                   / *env.view()*/


type ruleEval = Eval of id*params	/* same as block foo(4,5){bar(6);bar(7);} */
	
type expr=
	 Binop of expr*operator*expr   /* 0>$X>=5  $X==$Y 5!=4*/
	|Lit of digits
	|Var of variable
	|Str of string
	
type params = Params of param list	
type param = 
	 Lit of digits
	|Var of variable
	|Str of string 
	|Arr of array
	
type array = Arr of param list

	 
/*digits, variable and id (symbol) are defined in grammar written by Devesh*/
