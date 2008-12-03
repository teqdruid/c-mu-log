(*
*  print.ml
*
*  Started on  Wed Nov  5 15:13:52 2008 John Demme
*  Last update Mon Nov 24 17:07:48 2008 John Demme
*)

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
    | (CEqlInt(i1),    CGT(i2))        when (i1 > i2) -> a
    | (CGT(i1),        CEqlInt(i2))    when (i2 > i1) -> a
    | (CEqlInt(i1),    CLT(i2))        when (i1 < i2) -> a
    | (CLT(i1),        CEqlInt(i2))    when (i2 < i1) -> a
    | (CLT(i1),        CLT(i2))        -> CLT(min i1 i2)
    | (CGT(i1),        CGT(i2))        -> CLT(max i1 i2)
    | (CGT(i1),        CLT(i2))        when (i1 < i2) -> CRange(i1, i2)
    | (CLT(i2),        CGT(i1))        when (i1 < i2) -> CRange(i1, i2)
    | (_, _) -> FalseSol
;;

let rec list_fill item number = 
  if number <= 0
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

let cnst_extend_to a l =
  let delta = l - (List.length a) in
    if delta > 0
    then List.append a (list_fill Any delta)
    else a
;;

let cnstAndAll aC bC =
  let (aC, bC) = cnst_extend aC bC in
    List.map2 cAnd aC bC
;;

let match_signature signature name vars =
  let match_param param var = match (param, var) with
      (Ast.Lit(a), CEqlInt(b))     -> a == b
    | (Ast.Sym(a), CEqlSymbol (b)) -> (String.compare a b) == 0
    | (Ast.Str(a), CEqlStr    (b)) -> (String.compare a b) == 0
    | (Ast.Lit(a), CLT(b))         -> a < b
    | (Ast.Lit(a), CGT(b))         -> a > b
    | (Ast.Lit(a), CRange(b,c))    -> b < a && a < c
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
  | CLT(i)     -> "<" ^ (string_of_int i)
  | CGT(i)     -> ">" ^ (string_of_int i)
  | CRange(a,b)-> (string_of_int a) ^ ".." ^ (string_of_int b)
;;

let string_of_eval name vars =
  name ^ "(" ^ String.concat "," (List.map string_of_cnst vars) ^")\n"
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
	  Solution (cnstAndAll vars (sig_to_cnst signature.params),
		    (fun unit -> eval_loop tail))
      | Rule (signature, exec) :: tail
	  when match_signature signature name vars ->
	  run_gen tail (fun unit -> exec db vars)
      | head :: tail -> eval_loop tail
  in
    (*print_string ("In: " ^ (string_of_eval name vars));*)
    eval_loop db
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
		      ( let result = cnstAndAll aC bC in
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
  and parseCompOp op e1 e2 = 
    let compOp i flip =
      if flip then
	match op with
	    Ast.Lt -> CGT(i)
	  | Ast.Gt -> CLT(i)
	  | Ast.Leq -> CGT(i + 1)
	  | Ast.Geq -> CLT(i - 1)
	  | Ast.Eq  -> CEqlInt(i)
      else
	match op with
	    Ast.Lt -> CLT(i)
	  | Ast.Gt -> CGT(i)
	  | Ast.Leq -> CLT(i - 1)
	  | Ast.Geq -> CGT(i + 1)
	  | Ast.Eq  -> CEqlInt(i)
    in
    let doAnd myCnsts db cnst =
      let sol = cnstAndAll myCnsts cnst in
	(*(print_string (string_of_eval "" myCnsts));
	(print_string (string_of_eval "" cnst));*)
	if List.for_all (fun a -> a != FalseSol) sol
	then Solution(sol, fun () -> NoSolution)
	else NoSolution
    in
    match (e1, e2) with
	(Ast.ELit(i), Ast.RVar(v)) -> 
	  doAnd ((list_fill Any (v - 1)) @ [(compOp i true)])
      |	(Ast.RVar(v), Ast.ELit(i)) -> 
	  doAnd ((list_fill Any (v - 1)) @ [(compOp i false)])
      | _ -> (print_string "Unsupported comparison\n";
	      fun db cnst -> NoSolution)
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
      | Ast.Comp(e1, compOp, e2) ->
	  parseCompOp compOp e1 e2
      | _ -> (Printf.printf "Unsupported operation\n";
	      (fun db cnst -> NoSolution))
  in
  let parseRule stmt slots actions = 
    fun db cnst ->
      let rec runPer nxt = 
	match nxt with
	    NoSolution -> NoSolution
	  | Solution(cnsts, nxt) -> 
	      List.iter (fun action ->
			   (ignore (action db cnsts))) actions;
	      Solution(cnsts, fun () -> runPer (nxt()))
      in
	runPer (stmt db (cnst_extend_to cnst slots))
  in
  let parseRF = function
      Ast.Rule (name, Ast.Params(parms), statement) -> 
	Printf.printf "Internal error"; exit 1
    | Ast.TRule (name, Ast.Params(parms), numVars, statement, nseStmt) -> 
	Rule ({ name = name; params = parms}, 
	      (parseRule (parseStatement statement) numVars 
		 (List.map parseStatement nseStmt)))
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
