(*
*  printer.ml
*
*
*  Started on  Wed Nov  5 15:18:34 2008 John Demme
*  Last update Wed Nov  5 16:05:31 2008 John Demme
*)

open Ast

let string_of_compoperator = function
    Lt  -> "<"
  | Leq -> "<="
  | Gt  -> ">" 
  | Geq -> ">="
  | Eq  -> "="
  | Neq -> "!="

let string_of_operator = function
    Plus   -> "+"
  | Minus  -> "-"
  | Mult   -> "*"
  | Divide -> "/"


let rec string_of_expr = function 
    Binop(e1, o, e2) -> (string_of_expr e1) ^ (string_of_operator o) ^ (string_of_expr e2)
  | ELit(i) -> string_of_int i
  | EVar(s) -> s
  | RVar(i) -> "$" ^ (string_of_int i)
  | EStr(s) -> s
  | EId(s)  -> s

let rec string_of_param = function
    Lit(i) -> string_of_int i
  | Sym(s) -> s
  | Var(s) -> s
  | TVar(i) -> "$" ^ (string_of_int i)
  | Str(s) -> "\""^s^"\""
  | Arr(a) -> "["^string_of_params a^"]"
  | Ques   -> "?"	 	


and string_of_params = function
    Params(pList) -> String.concat "," (List.map string_of_param pList)
  | Array(pList) -> String.concat "," (List.map string_of_param pList)

let rec string_of_stmts = function
    Stmts(sList) -> String.concat "\n" (List.map string_of_stmt sList)
and string_of_stmt = function
    Block(red, stmts) -> "{" ^ red ^ ":\n" ^ (string_of_stmts stmts) ^ "\n}"
  | Comp(e1, c, e2) -> (string_of_expr e1) ^ (string_of_compoperator c) 
      ^ (string_of_expr e2) ^ ";"
  | Eval(name, ps) -> name ^ "(" ^ (string_of_params ps) ^ ");"
  | NEval(name1, ps1) -> "!" ^ name1 ^ "(" ^ (string_of_params ps1) ^ ");"	
  | DirectiveStudy(name,stmts) -> "@"^name ^ "(" ^ 
      (string_of_stmts (Stmts (List.map (fun a -> Eval(a)) stmts))) ^ ")"
  | Directive(name,params) -> "@"^name^"("^(string_of_params params)^")"
  | Dot1(str1,str2,stmts) -> str1^"."^"@"^str2^"("^(string_of_stmts stmts)^");" 
  | Dot2(str1,str2,ps) -> str1^"."^str2^"("^(string_of_params ps)^");"

let string_of_ruleFact = function
    Rule(name, params, stmt) -> name ^ "(" ^ (string_of_params params) ^ ") " 
      ^ (string_of_stmt stmt)
  | TRule(name, params, slots, stmt, nseStmts) ->
      name ^ "(" ^ (string_of_params params) ^ ") " 
      ^ (if (List.length nseStmts) != 0
	 then
	   ( "{Run Per Solution: \n" ^
	       (String.concat ";\n" (List.map string_of_stmt nseStmts)) ^ ";\n}\n")
	 else "")
      ^ (string_of_stmt stmt)
  | Fact(name, params)       -> name ^ "(" ^ (string_of_params params) ^ ");"
  | GlobalDirective(name,ps) ->"@"^name ^ "(" ^ (string_of_params ps) ^ ");"

let string_of_program = function
    Program(ruleList) -> String.concat "\n" (List.map string_of_ruleFact ruleList) ^ "\n"
