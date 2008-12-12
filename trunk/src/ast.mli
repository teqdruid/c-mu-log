type operator = Plus | Minus | Mult | Divide 
type compoperator = Lt | Leq | Gt | Geq | Eq | Neq

(* type study = learn | forget *)

type param = 
    Lit  of int
  | Sym  of string
  | Var  of string
  | Str  of string 
  | Arr  of params
  | Ques

and params = 
    Params of param list
  | Array of param list

type expr =
    Binop of expr*operator*expr   (* 0>$X>=5  $X==$Y 5!=4*)
  | ELit  of int
  | EVar  of string
  | EStr  of string
  | EId   of string

type eval = string*params

type stmt =
    Block of string*stmts                	(* {.....} *)
  | Comp of expr*compoperator*expr		(* $5+5<$4  $a=5,$b=6; *)
  | NEval of string*params		   	(*!wall(4,5) *)
  | Eval of eval			   	(*wall(4,5) *)
  | DirectiveStudy of string*(eval list)	(*@learn(wall(4,5);)*)
  | Directive of string*params       		(*@print("dfdsf");*)
  | Dot1 of string*string*(eval list)          	(*$agent.@learn(wall(4,5);) *)
  | Dot2 of string*string*params          	(* env.view($X,$Y,$Obj)*)

and stmts=Stmts of stmt list  (* statment1;statment2;statement3; *)


type ruleFact = 
    Rule of string * params * stmt
  | Fact of string * params
  | GlobalDirective of string*params (*@attach("dfsfsa")*) (*@print("ddafafa")*)


type program = Program of ruleFact list
	
