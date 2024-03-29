(*
*  printer.ml
*
*  Originally written in group, typed by John Demme
*  Updated as necessary by everyone
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
  | EStr(s) -> s
  | EId(s)  -> s

let rec string_of_param = function
    Lit(i) -> string_of_int i
  | Sym(s) -> s
  | Var(s) -> s
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
  | Dot1(str1,str2,stmts) -> str1^"."^"@"^str2^"("^
      (string_of_stmts (Stmts (List.map (fun a -> Eval(a)) stmts))) ^ ")"
  | Dot2(str1,str2,ps) -> str1^"."^str2^"("^(string_of_params ps)^");"
  | NDot2(str1,str2,ps) -> "!"^str1^"."^str2^"("^(string_of_params ps)^");"
  

let string_of_ruleFact = function
    Rule(name, params, stmt) -> name ^ "(" ^ (string_of_params params) ^ ") " 
      ^ (string_of_stmt stmt)
  | Fact(name, params)       -> name ^ "(" ^ (string_of_params params) ^ ");"
  | GlobalDirective(name,ps) ->"@"^name ^ "(" ^ (string_of_params ps) ^ ");"

let string_of_program = function
    Program(ruleList) -> String.concat "\n" (List.map string_of_ruleFact ruleList) ^ "\n"
