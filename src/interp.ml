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
  | CLT        of int
  | CGT        of int
  | CRange     of int*int
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
    | (Ast.TVar(i), _)             -> true
	(* TODO: Array matching *)
    | (_         , Any)            -> true
    | (_         , _  )            -> false
  in
    (String.compare signature.name name == 0) &&
      ((List.length signature.params) == (List.length vars)) &&
      (List.for_all (fun a -> a) (List.map2 match_param signature.params vars))
;;

let string_of_cnst = function
    Any -> "Any"
  | FalseSol -> "False"
  | CEqlSymbol(s) -> s
  | CEqlStr(s) -> "'" ^ s ^ "'"
  | CEqlInt(i) -> string_of_int i
(*  | _ -> "!!"*)
;;

let string_of_eval name vars =
  name ^ "(" ^ String.concat "," (List.map string_of_cnst vars) ^")\n"
;;

(* TODO: Need constraints list mapping *)
let rec run_eval db name vars =
  let rec run_gen tail nextGen =
    let sols = (nextGen ()) in
      match sols with
	  NoSolution -> run_eval tail name vars
	| Solution (cnst, gen) -> Solution(cnst, (fun unit -> run_gen tail gen))
  in
  let rec eval_loop e = 
    match e with
	[] -> NoSolution
      | Fact (signature) :: tail
	  when match_signature signature name vars ->
	  Solution (vars, (fun unit -> eval_loop tail))
      | Rule (signature, exec) :: tail
	  when match_signature signature name vars ->
	  run_gen tail (fun unit -> exec db vars)
      | head :: tail -> eval_loop tail
  in
    (*print_string ("In: " ^ (string_of_eval name vars));*)
    eval_loop db
;;

let cnst_of_params params env =
  let param_to_cnst = function
      Ast.Lit(i) -> CEqlInt    (i)
    | Ast.Sym(s) -> CEqlSymbol (s)
    | Ast.Var(v) -> Any
    | Ast.TVar(i) -> List.nth env i
    | Ast.Str(s) -> CEqlStr    (s) 
    | Ast.Arr(a) -> Any (* TODO: Array Matching *)
  in
    (*print_string (string_of_eval "cop_env" env);
    print_string ("cop: " ^ (Printer.string_of_params (Ast.Params(params))) ^ "\n");*)
    List.map param_to_cnst params
;;

let sig_to_cnst signature =
  let param_to_cnst = function
      Ast.Lit(i) -> CEqlInt    (i)
    | Ast.Sym(s) -> CEqlSymbol (s)
    | Ast.Var(v) -> Any
    | Ast.TVar(i) -> Any
    | Ast.Str(s) -> CEqlStr    (s) 
    | Ast.Arr(a) -> Any (* TODO: Array Matching *)
  in
    List.map param_to_cnst signature
;;

let rec list_fill item number = 
  if number == 0
  then []
  else item :: (list_fill item (number - 1));;

let cnst_extend a b = 
  let delta = (List.length a) - (List.length b) in
    if delta > 0
    then (a, List.append b (list_fill Any delta))
    else if delta < 0
    then (List.append a (list_fill Any (delta * -1)), b)
    else (a,b)
;;

let rec list_replace i e list =
  match list with
      [] -> []
    | hd :: tl ->
	if i == 0
	then e :: tl
	else hd :: (list_replace (i - 1) e tl)
;;

let parseDB (prog) = 
  let parseCompilerDirective name params =
    let nc = String.compare name in
      if (nc "print") == 0
      then
	fun db cnst ->
	  let print_param param =
	    match param with
		Ast.Lit(i) -> print_int i
	      | Ast.Str(s) -> print_string s
	      | Ast.Sym(s) -> print_string s
	      | Ast.TVar(i) -> print_int i (*print_string (string_of_cnst (List.nth cnst i))*)
	      | _ -> ()
	  in
	    (ignore (List.map print_param params);
	     print_string "\n";
	     Solution([Any], fun () -> NoSolution))
	       (* TODO: This will cause AND blocks to fail.  Any will 
		  cause OR blocks to always succeed. Hack a fix*)
      else
	(print_string "Unknown compiler directive";
	 fun db cnst ->
	   NoSolution)
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
		      ( let (aC, bC) = cnst_extend aC bC in
			let result = List.map2 cAnd aC bC in
			 if List.for_all (fun a -> a != FalseSol) result
			 then Solution(result, 
				       (fun unit -> run_merge aNext (bN ())))
			 else run_merge aNext (bN ()))
		  | (Solution(sCnst, aN), NoSolution) -> run_merge (aN ()) (n db cnst)
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
  and parseEval name params =
    fun db cnst -> 
       let cnsts = cnst_of_params params cnst in
       let revMap rCnsts = 	 
	 let revMapIndv cnsts param cnst = 
	   match param with 
	       Ast.TVar(i) -> 
		 let delta = i - (List.length cnsts) in
		   if delta == 0
		   then List.append cnsts [cnst]
		   else if delta > 0
		   then List.append cnsts (List.append (list_fill Any i) [cnst])
		   else list_replace i cnst cnsts
	     | _       -> cnsts
	 in
	   List.fold_left2 revMapIndv [] params rCnsts in
       let nxt = run_eval db name cnsts in
       let rec doNxt nxt =
	 match nxt with 
	     NoSolution -> NoSolution
	   | Solution(rCnsts, nxt) ->
	       let rCnsts = revMap rCnsts in
		 Solution(rCnsts, (fun unit -> doNxt (nxt ())))
       in
	 doNxt nxt
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
	  parseEval name params
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
  let tProg = Trans.translate(prog) in
    match tProg with
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

let rec iter_sols nxt =
  match nxt with
      NoSolution -> print_string "No more solutions\n"
    | Solution(c,n) -> 
	(print_string "Solution\n");
	iter_sols (n ())
;;

let _ = 
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
  let program = Parser.program Scanner.token lexbuf in
  let pDB = parseDB(program) in
    (*print_string "Database dump:\n";
    dump_db pDB;
    print_string "\n";*)
    let sGen = query pDB {name = "main"; params = []} in
      iter_sols sGen
;;
