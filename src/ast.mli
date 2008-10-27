type operator = Plus | Minus | Mult | Divide 
type compoperator = Lt | Leq | Gt | Geq | Eq | Neq

(* type study = learn | forget *)

type param = 
    Lit of int
  | Var of string
  | Str of string 
  | Arr of array

and params = Params of param list	
and array = Array of param list

type expr =
    Binop of expr*operator*expr   (* 0>$X>=5  $X==$Y 5!=4*)
  | ELit of int
  | EVar of string
  | EStr of string

type stmt =
    Block of string*stmts                (* {.....} *)
  | Comp of expr*compoperator*expr		(* $5+5<$4  $a=5,$b=6; *)
  | Eval of string*params					     (* wall(4,5) *)
  | Direc of string*params       (*@attach("dfsfsa")*) (*@print("ddafafa")*)
(*  | Modif of study*stmts *)         (*@learn(wall(4,5);wall(5,6))*)
  | Dot1 of string*string*stmt              (*$agent.@learn() *)
  | Dot2 of string*stmt                   (* env.view()*)

and stmts=Stmts of stmt list  (* statment1;statment2;statement3; *) 


type ruleFact = 
    Rule of string * params * stmt
  | Fact of string * params

type program = Program of ruleFact list
	


	 
(*digits, variable and string (symbol) are defined in grammar written by Devesh*)
