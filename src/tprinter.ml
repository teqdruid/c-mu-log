(*
*  tprinter.ml
*
*
*  Started on  Wed Nov  5 15:18:34 2008 John Demme
*  Last update Wed Nov  5 16:05:31 2008 John Demme
*)

open Tst

let string_of_compoperator = function
    Ast.Lt  -> "<"
  | Ast.Leq -> "<="
  | Ast.Gt  -> ">" 
  | Ast.Geq -> ">="
  | Ast.Eq  -> "="
  | Ast.Neq -> "!="

let string_of_operator = function
    Ast.Plus   -> "+"
  | Ast.Minus  -> "-"
  | Ast.Mult   -> "*"
  | Ast.Divide -> "/"


let rec string_of_expr = function 
  | ELit(i) -> string_of_int i

let rec string_of_param = function
    Lit(i) -> string_of_int i
  | Sym(s) -> s
  | Var(i) -> "$" ^ (string_of_int i)
  | Str(s) -> "\""^s^"\""
  | Arr(a) -> "["^string_of_params a^"]"
  | Anon   -> "?"	 	


and string_of_params pList = 
   String.concat "," (List.map string_of_param pList)
     
let rec string_of_stmts sList = 
    String.concat "\n" (List.map string_of_stmt sList)
and string_of_stmt = function
    Block(red, stmts) -> "{" ^ red ^ ":\n" ^ (string_of_stmts stmts) ^ "\n}"
  | Comp(v, c, e2) -> ("$" ^ (string_of_int v)) ^ (string_of_compoperator c) 
      ^ (string_of_expr e2) ^ ";"
  | Eval(name, ps) -> name ^ "(" ^ (string_of_params ps) ^ ");"
  | NEval(name1, ps1) -> "!" ^ name1 ^ "(" ^ (string_of_params ps1) ^ ");"	
  | DirectiveStudy(name,stmts) -> "@"^name ^ "(" ^ 
      (string_of_stmts (List.map (fun a -> Eval(a)) stmts)) ^ ")"
  | Directive(name,params) -> "@"^name^"("^(string_of_params params)^")"
  | Dot1(v,str2,stmts) -> "$"^(string_of_int v)^"."^"@"^str2^"("^(string_of_stmts stmts)^");" 
  | Dot2(v,str2,ps) -> "$"^(string_of_int v)^"."^str2^"("^(string_of_params ps)^");"

let string_of_ruleFact = function
  | Rule(name, params, slots, stmt, nseStmts) ->
      name ^ "(" ^ (string_of_params params) ^ ") " 
      ^ (if (List.length nseStmts) != 0
	 then
	   ( "{Run Per Solution: \n" ^
	       (String.concat ";\n" (List.map string_of_stmt nseStmts)) ^ ";\n}\n")
	 else "")
      ^ (string_of_stmt stmt)
  | Fact(name, params)       -> name ^ "(" ^ (string_of_params params) ^ ");"

let string_of_program ruleList =
  String.concat "\n" (List.map string_of_ruleFact ruleList) ^ "\n"
