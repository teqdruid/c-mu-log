(*
  This is a simpler, much more restrictive version of the AST.
  It is much easier for the interpreter to deal with, and is relatively
  easy to obtain given an AST.  The trans.ml module translates from the AST
  to this TST.
*)
type param = 
    Lit  of int
  | Sym  of string
  | Var  of int
  | Anon
  | Str  of string 
  | Arr  of param list

and params = param list

type expr =
  | ELit  of int

type eval = string*params
type var  = int

type stmt =
    Block of string*stmts                	(* {.....} *)
  | Comp of var*Ast.compoperator*expr		(* $5+5<$4  $a=5,$b=6; *)
  | StrComp of var*string
  | SymComp of var*string
  | NEval of eval			   	(*!wall(4,5) *)
  | Eval of eval			   	(*wall(4,5) *)
  | DirectiveStudy of string*(eval list)	(*@learn(wall(4,5);)*)
  | Directive of string*params       		(*@print("dfdsf");*)
  | Dot1 of int*string*stmts              	(*$agent.@learn(wall(4,5);) *)
  | Dot2 of int*string*params	          	(* env.view($X,$Y,$Obj)*)

and stmts=stmt list  (* statment1;statment2;statement3; *) 

type ruleFact = 
  | Rule of string * params * int * stmt * stmt list
  | Fact of string * params


type program = ruleFact list
	
