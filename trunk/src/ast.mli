(*Original author: Cheng Cheng
  Edited	 : Devesh Dedhia
  support added to include directives *)
		
type operator = Plus | Minus | Mult | Divide 
type compoperator = Lt | Leq | Gt | Geq | Eq | Neq

(* type study = learn | forget *)

type param = 
    Lit  of int				(* 0...9*)
  | Sym  of string			(* sym1*)
  | Var  of string			(* $X *)
  | Str  of string 			(*"asdf"*)
  | Arr  of params			(* [2,$x,symb1]*)	
  | Ques

and params = 
    Params of param list		
  | Array of param list

type expr =
    Binop of expr*operator*expr   	(* 0>$X>=5  $X==$Y 5!=4*)
  | ELit  of int		  	(* 0...9*)	
  | EVar  of string		 	(* $X *)		 	
  | EStr  of string			(*"asdf"*)
  | EId   of string			(* sym1*)

type eval = string*params

type stmt =
    Block of string*stmts                	(* {.....} *)
  | Comp of expr*compoperator*expr		(* $5+5<$4  $a=5,$b=6; *)
  | NEval of string*params		   	(*!wall(4,5) *)
  | Eval of eval			   	(*wall(4,5) *)
  | DirectiveStudy of string*(eval list)	(*@learn(wall(4,5);)*)
  | Directive of string*params       		(*@print("dfdsf");*)
  | Dot1 of string*string*(eval list)          	(*$agent.@learn(wall(4,5);) *)
  | Dot2 of string*string*params          	(* $env.view($X,$Y,$Obj)*)
  | NDot2 of string*string*params          	(* !env.view($X,$Y,$Obj)*)

and stmts=Stmts of stmt list  (* statment1;statment2;statement3; *)


type ruleFact = 
    Rule of string * params * stmt	(* wall(3,4){AND: ....}*)
  | Fact of string * params		(* wall(2,2);*)
  | GlobalDirective of string*params (*@attach("dfsfsa")*) (*@print("ddafafa")*)


type program = Program of ruleFact list
	
