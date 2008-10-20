type operator = + | - | * | / 
type compoperator= < | <= | > | >= | == | !=

type study = @learn | @forget

type stmts=Stmts of stmt list  /* statment1;statment2;statement3; */ 
type Block = string * stmt list
type stmt=
	 Block of stmts                 /* {.....} */
	|Comp of expr*compoperator*expr		$5+5<$4					/* $a=5,$b=6; */
	|Fct of id*params								/* wall(4,5) */
	|Direc of id*params       /*@attach("dfsfsa")*/ /*@print("ddafafa")*/
	|Modif of study*stmts          /*@learn(wall(4,5);wall(5,6))*/
	|Dot1 of variable*stmt              /*$agent.@learn() */
	|Dot2 of id*stmt                   / *env.view()*/

type Rule = id*params*stmts					/* foo(4,5){bar(6);bar(7);} */
	
type expr=
	 Binop of expr*operator*expr   /* 0>$X>=5  $X==$Y 5!=4*/
	|digits
	|variable
	|string
	
type params = Params of param list	
type param = 
	 digits
	|variable
	|string
	|array
	
type array = Array of param list

	 
/*digits, variable and id (symbol) are defined in grammar written by Devesh*/
