type operator = Plus | Minus | Mult | Divide 
type compoperator = Lt | Leq | Gt | Geq | Eq | Neq

(* type study = learn | forget *)

type param = 
    Lit of operator * int
  | Sym of  string
  | Var of string
  | TVar of int
  | Str of string 
  | Arr of params

and params = 
   Params of param list
   | Array of param list

	

type expr =
    Binop of expr*operator*expr   (* 0>$X>=5  $X==$Y 5!=4*)
  | ELit of operator*int
  | EVar of string
  | RVar of int
  | EStr of string

type stmt =
    Block of string*stmts                	(* {.....} *)
  | Comp of expr*compoperator*expr		(* $5+5<$4  $a=5,$b=6; *)
  | Eval of string*params		   	(*wall(4,5) *)
  | DirectiveStudy of string*stmts		(*@learn(wall(4,5);)*)
  | Directive of string*params       		
  | Dot1 of string*string*stmts              	(*$agent.@learn(wall(4,5);) *)
  | Dot2 of string*string*params          	(* env.view($X,$Y,$Obj)*)

and stmts=Stmts of stmt list  (* statment1;statment2;statement3; *) 


type ruleFact = 
    Rule of string * params * stmt
  | Fact of string * params
  | GlobalDirective of string*params (*@attach("dfsfsa")*) (*@print("ddafafa")*)


type program = Program of ruleFact list
	
