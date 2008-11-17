(*
*  print.ml
*
*  Started on  Wed Nov  5 15:13:52 2008 John Demme
*  Last update Mon Nov 10 16:02:08 2008 John Demme
*)

(* module StrMap = Map.Make(String);; *)

type var_cnst = 
    Any
  | FalseSol
  | CEqlSymbol of string
  | CEqlInt    of int
  | CEqlStr    of string
;;

type cnst = var_cnst list;;

type signature = {
  name   : string;
  params : Ast.param list
};;

type next = 
    NoSolution
  | Solution of cnst * (unit -> next);;

type rule_fact = 
    Fact of signature
  | Rule of signature * (database -> cnst -> next)
and database = rule_fact list;;

let cAnd a b =
  match (a, b) with
      (Any, _) -> b
    | (_, Any) -> a
    | (CEqlSymbol(s1), CEqlSymbol(s2)) when (0 == String.compare s1 s2 ) -> a
    | (CEqlStr(s1),    CEqlStr(s2))    when (0 == String.compare s1 s2 ) -> a
    | (CEqlInt(i1),    CEqlInt(i2))    when (i2 == i2 ) -> a
    | (_, _) -> FalseSol
;;


let match_signature signature name vars =
  let match_param param var = match (param, var) with
      (Ast.Lit(a), CEqlInt(b))     -> a == b
    | (Ast.Sym(a), CEqlSymbol (b)) -> (String.compare a b) == 0
    | (Ast.Str(a), CEqlStr    (b)) -> (String.compare a b) == 0
	(* TODO: Array matching *)
    | (_         , Any)            -> true
    | (_         , _  )            -> false
  in
    (String.compare signature.name name == 0) &&
      ((List.length signature.params) == (List.length vars)) &&
      (List.for_all (fun a -> a) (List.map2 match_param signature.params vars))
;;


(* TODO: Need constraints list mapping *)
let rec run_eval db name vars =
  let rec run_gen nextGen =
    let sols = (nextGen ()) in
      match sols with
	  NoSolution -> run_eval (List.tl db) name vars
	| Solution (cnst, gen) -> Solution(cnst, (fun unit -> run_gen gen))
  in
  let rec eval_loop e = 
    match e with
	[] -> NoSolution
      | Fact (signature) :: tail
	  when match_signature signature name vars ->
	  Solution (vars, (fun unit -> eval_loop tail))
      | Rule (signature, exec) :: tail
	  when match_signature signature name vars ->
	  run_gen (fun unit -> exec db vars)
      | head :: tail -> eval_loop tail
  in
    eval_loop db
;;


let sig_to_cnst signature =
  let param_to_cnst = function
      Ast.Lit(i) -> CEqlInt    (i)
    | Ast.Sym(s) -> CEqlSymbol (s)
    | Ast.Var(v) -> Any
    | Ast.Str(s) -> CEqlStr    (s) 
    | Ast.Arr(a) -> Any (* TODO: Array Matching *)
  in
    List.map param_to_cnst signature
;;

let rec list_fill item number = 
  if number == 0
  then []
  else item :: (list_fill item number);;

let parseDB (prog) = 
  let parseCompilerDirective name params =
    fun db cnst ->
      (print_string "Compiler directive\n";
       NoSolution) (* TODO: This will cause AND blocks to fail.  Any will 
		      cause OR blocks to always succeed. Hack a fix*)
  in
  let rec parseAndBlock stmts = 
    match stmts with
	[] -> (fun db cnst -> Solution (list_fill Any (List.length cnst), fun unit -> NoSolution))
      | stmt :: tail ->
	  let s = (parseStatement stmt) in
	  let n = (parseAndBlock  tail) in
	    fun db cnst ->
	      let rec run_merge aNext bNext =
		match (aNext, bNext) with
		    (Solution(aC, aN), Solution(bC, bN)) ->
		      (let result = List.map2 cAnd aC bC in
			 if List.for_all (fun a -> a != FalseSol) result
			 then Solution(result, 
				       (fun unit -> run_merge aNext (bN ())))
			 else run_merge aNext (bN ()))
		  | (Solution(cnst, aN), NoSolution) -> run_merge (aN ()) (n db cnst)
		  | (NoSolution, _) -> NoSolution
	      in
		run_merge (s db cnst) (n db cnst)
  and parseOrBlock stmts = 
    match stmts with
	[] -> (fun db cnst -> NoSolution)
      | stmt :: tail ->
	  let nextStmt = (parseOrBlock tail) in
	  let currStmt = (parseStatement stmt) in
	    fun db cnst ->	    
	      let rec runOr nxt = 
		match nxt with
		    NoSolution -> nextStmt db cnst
		  | Solution(vars, nxt) -> Solution(vars, 
						    (fun unit -> runOr (nxt ())))
	      in
		runOr (currStmt db cnst)
  and parseStatement statement = 
    match statement with 
	Ast.Block (redOp, Ast.Stmts(statements))
	  when 0 == (String.compare redOp "AND") ->
	    parseAndBlock statements
      | Ast.Block (redOp, Ast.Stmts(statements))
	  when 0 == (String.compare redOp "OR") ->
	    parseOrBlock statements
      |	Ast.Block (redOp, Ast.Stmts(statements)) ->
	  (Printf.printf "Invalid reduction operator %s\n" redOp;
	   (fun db cnst -> NoSolution))
      | Ast.Eval (name, Ast.Params(params)) -> 
	  (fun db cnst -> run_eval db name (sig_to_cnst params))
      | Ast.Directive (name, Ast.Params(params)) ->
	  parseCompilerDirective name params
      | _ -> (Printf.printf "Unsupported operation\n";
	      (fun db cnst -> NoSolution))
  in
  let parseRF = function
      Ast.Rule (name, Ast.Params(parms), statement) -> 
	Rule ({ name = name; params = parms}, (parseStatement statement))
    | Ast.Fact (name, Ast.Params(parms))            -> 
	Fact ({ name = name; params = parms}) 
  in
    match prog with
	Ast.Program (ruleFacts) -> List.map parseRF ruleFacts
;;

let query db s =
  run_eval db s.name (sig_to_cnst s.params)
;;

let rec dump_db db = 
  let print_sig s =
    print_string s.name
  in
  match db with
      [] -> ()
    | Fact(s) :: tail -> print_sig s; print_string ";\n"; dump_db tail
    | Rule(s, f) :: tail -> print_sig s; print_string " {}\n"; dump_db tail
;;
    
let _ = 
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let pDB = parseDB(program) in
  let sGen = query pDB {name = "main"; params = []} in
    print_string "Database dump:\n";
    dump_db pDB;
    print_string "\n";
    match sGen with
	NoSolution -> print_string "main evaluates false\n"
      | Solution (c, n) -> print_string "main evaluates true\n"
;;
