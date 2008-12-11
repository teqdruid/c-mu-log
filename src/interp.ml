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
  params : cnst
};;

type next = 
    NoSolution
  | Solution of cnst * (unit -> next);;

type rule_fact = 
    Fact of signature
  | Rule of signature * (database -> cnst -> next)
and database = rule_fact list ref;;

let cAnd a b =
  match (a, b) with
      (Any, _) -> b
    | (_, Any) -> a
    | (CEqlSymbol(s1), CEqlSymbol(s2)) when (0 == String.compare s1 s2 ) -> a
    | (CEqlStr(s1),    CEqlStr(s2))    when (0 == String.compare s1 s2 ) -> a
    | (CEqlInt(i1),    CEqlInt(i2))    when (i2 == i2 ) -> a
    | (CEqlInt(i1),    CGT(i2))        when (i1 > i2) -> a
    | (CGT(i1),        CEqlInt(i2))    when (i2 > i1) -> b
    | (CEqlInt(i1),    CLT(i2))        when (i1 < i2) -> a
    | (CLT(i1),        CEqlInt(i2))    when (i2 < i1) -> b
    | (CLT(i1),        CLT(i2))        -> CLT(min i1 i2)
    | (CGT(i1),        CGT(i2))        -> CLT(max i1 i2)
    | (CGT(i1),        CLT(i2))        when (i1 < i2) -> CRange(i1, i2)
    | (CLT(i2),        CGT(i1))        when (i1 < i2) -> CRange(i1, i2)
    | (_, _) -> FalseSol
;;

(* return the first n elements of list *)
let rec list_first n list = 
  match list with
      [] -> []
    | hd :: tl when n > 0 -> hd :: list_first (n - 1) tl
    | _ -> []
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
  let match_params param vars =
    let anded = cnstAndAll param vars in
      List.for_all (fun a -> a != FalseSol) anded
  in
    (String.compare signature.name name == 0) &&
      ((List.length signature.params) == (List.length vars)) &&
      (match_params signature.params vars)
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

let remove_fact_all db pred cnsts = 
  (* print_string ("Removing: "^ (string_of_eval pred cnsts) ^ "\n"); *)
  List.filter
    (fun curr ->
       match curr with
	   Fact(signature) when 
	     match_signature signature pred cnsts -> false
	 | _ -> true)
    db
;;

let cnst_of_params params env =
  let param_to_cnst = function
      Tst.Lit(i) -> CEqlInt    (i)
    | Tst.Sym(s) -> CEqlSymbol (s)
    | Tst.Var(i) -> List.nth env i
    | Tst.Str(s) -> CEqlStr    (s) 
    | Tst.Anon   -> Any
    | Tst.Arr(a) -> failwith "Arrays are not support yet"
  in
    (*print_string (string_of_eval "cop_env" env);
    print_string ("cop: " ^ (Printer.string_of_params (Tst.Params(params))) ^ "\n");*)
    List.map param_to_cnst params
;;

let sig_to_cnst signature =
  let param_to_cnst = function
      Tst.Lit(i) -> CEqlInt    (i)
    | Tst.Sym(s) -> CEqlSymbol (s)
    | Tst.Var(i) -> Any
    | Tst.Anon   -> Any
    | Tst.Str(s) -> CEqlStr    (s) 
    | Tst.Arr(a) -> failwith "Arrays are not supported yet"
  in
    List.map param_to_cnst signature
;;

(* TODO: Need constraints list mapping *)
let rec run_eval db name vars =
  let rec run_gen tail nextGen =
    let sols = (nextGen ()) in
      match sols with
	  NoSolution -> eval_loop tail
	| Solution (cnst, gen) -> Solution(cnst, (fun unit -> run_gen tail gen))
   and eval_loop e = 
    match e with
	[] -> NoSolution
      | Fact (signature) :: tail
	  when match_signature signature name vars ->
	  Solution (cnstAndAll vars signature.params,
		    (fun unit -> eval_loop tail))
      | Rule (signature, exec) :: tail
	  when match_signature signature name vars ->
	  let matchedVars = cnstAndAll vars signature.params in
	    run_gen tail (fun unit -> exec db matchedVars)
      | head :: tail -> eval_loop tail
  in
    (*print_string ("In: " ^ (string_of_eval name vars));*)
    eval_loop !db
;;

let rec list_replace i e list =
  match list with
      [] -> []
    | hd :: tl ->
	if i == 0
	then e :: tl
	else hd :: (list_replace (i - 1) e tl)
;;

let rec range i j = if i >= j then [] else i :: (range (i+1) j)

let parseDB (prog) = 
  let parseCompilerDirective name params =
    let nc = String.compare name in
      if (nc "print") == 0
      then
	fun db cnst ->
	  let print_param param =
	    match param with
		Tst.Lit(i) -> print_int i
	      | Tst.Str(s) -> print_string s
	      | Tst.Sym(s) -> print_string s
	      | Tst.Var(i) -> print_string (string_of_cnst (List.nth cnst i))
	      | _ -> ()
	  in
	    (List.iter print_param params;
	     print_string "\n";
	     NoSolution)
      else
	(print_string "Unknown compiler directive";
	 fun db cnst ->
	   NoSolution)
  in
  let rec parseAndBlock stmts = 
    match stmts with
	[] -> (fun db cnst -> Solution (cnst, fun unit -> NoSolution))
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
	      let sN = (s db cnst) in
	      let nN = (n db cnst) in
		run_merge sN nN
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
       (*let revMap rCnsts = 	 
	 let revMapIndv cnsts param cnst = 
	   match param with 
	       Tst.Var(i) -> 
		 let delta = i - (List.length cnsts) in
		   if delta == 0
		   then List.append cnsts [cnst]
		   else if delta > 0
		   then List.append cnsts (List.append (list_fill Any i) [cnst])
		   else list_replace i cnst cnsts
	     | _       -> cnsts
	 in
	   List.fold_left2 revMapIndv [] params rCnsts in*)
       let revMap rCnsts = 
	 List.map2
	   (fun c idx ->
	      let intoMe = List.fold_left2
		(fun a prm pIdx -> 
		   match prm with
		       Tst.Var(i) when i == idx -> (List.nth rCnsts pIdx) :: a
		     | _ -> a)
		[]
		params
		(range 0 (List.length params))
	      in
		List.fold_left cAnd c intoMe)
	   cnst
	   (range 0 (List.length cnst))
       in
       let nxt = run_eval db name cnsts in
       let rec doNxt nxt =
	 match nxt with 
	     NoSolution -> NoSolution
	   | Solution(rCnsts, nxt) ->
	       (* print_string (string_of_eval name rCnsts); *)
	       let rCnsts = revMap (list_first (List.length params) rCnsts) in
		 (* print_string (string_of_eval name rCnsts); *)
		 Solution(rCnsts, (fun unit -> doNxt (nxt ())))
       in
	 doNxt nxt
  and doAnd myCnsts db cnst =
    let sol = cnstAndAll myCnsts cnst in
      (*(print_string (string_of_eval "" myCnsts));
	(print_string (string_of_eval "" cnst));*)
      if List.for_all (fun a -> a != FalseSol) sol
      then Solution(sol, fun () -> NoSolution)
      else NoSolution
  and parseCompOp op v e2 = 
    let compOp i  =
      match op with
	  Ast.Lt -> CLT(i)
	| Ast.Gt -> CGT(i)
	| Ast.Leq -> CLT(i + 1)
	| Ast.Geq -> CGT(i - 1)
	| Ast.Eq  -> CEqlInt(i)
	| _ -> failwith "Unsupported comparison operator"
    in
      match e2 with
	  Tst.ELit(i) -> 
	    doAnd ((list_fill Any v) @ [(compOp i)])
  and parseStrComp v s = 
    doAnd ((list_fill Any v) @ [CEqlStr(s)])    
  and parseSymComp v s = 
    doAnd ((list_fill Any v) @ [CEqlSymbol(s)])
  and parseLearnForget name statements =
    let remove_facts db cnsts =
      let remove_fact (name,params) =
	db := remove_fact_all !db name (cnst_of_params params cnsts)
      in
	List.iter remove_fact statements
    in
    let add_facts db cnsts =
      let add_fact (name,params) =
	db := Fact({name = name; params = (cnst_of_params params cnsts)}) :: !db
      in
	List.iter add_fact statements
    in
    let nm = String.compare name in
      if (nm "learn") == 0
      then (fun db cnsts -> add_facts db cnsts; NoSolution)
      else if (nm "forget") == 0 
      then (fun db cnsts -> remove_facts db cnsts; NoSolution)
      else failwith ("Invalid directive: " ^ name)
  and parseStatement statement = 
    match statement with 
	Tst.Block (redOp, statements)
	  when 0 == (String.compare redOp "AND") ->
	    parseAndBlock statements
      | Tst.Block (redOp, statements)
	  when 0 == (String.compare redOp "OR") ->
	    parseOrBlock statements
      |	Tst.Block (redOp, statements) ->
	  (Printf.printf "Invalid reduction operator %s\n" redOp;
	   (fun db cnst -> NoSolution))
      | Tst.Eval (name,   params) -> 
	  parseEval name params
      | Tst.Directive (name, params) ->
	  parseCompilerDirective name params
      | Tst.Comp(e1, compOp, e2) ->
	  parseCompOp compOp e1 e2
      | Tst.DirectiveStudy(name, statements) ->
	  parseLearnForget name statements
      | Tst.StrComp(v, s) ->
	  parseStrComp v s
      | Tst.SymComp(v, s) ->
	  parseSymComp v s
  in
  let parseRule stmt slots actions = 
    fun db inCnsts ->      
      let rec runPer sols nxt = 
	match nxt with
	    NoSolution -> NoSolution
	  | Solution(outCnsts, nxt) -> 
	      (* Have we already given this solution? *)
	      if (List.mem outCnsts sols)
	      then runPer sols (nxt())
	      else
		(List.iter 
		   (fun action ->
		      (ignore (action db outCnsts))) 
		   actions;
		 Solution(outCnsts, fun () -> runPer (outCnsts :: sols)(nxt())))
      in
	(* print_string ("Num slots: " ^ (string_of_int slots) ^ "\n"); *)
	runPer [] (stmt db (cnst_extend_to inCnsts slots))
  in
  let parseRF = function
      Tst.Rule (name, parms, numVars, statement, nseStmt) -> 
	Rule ({ name = name; params = (sig_to_cnst parms)}, 
	      (parseRule (parseStatement statement) numVars 
		 (List.map parseStatement nseStmt)))
    | Tst.Fact (name, parms)            -> 
	Fact ({ name = name; params = (sig_to_cnst parms)}) 
  in
  let tProg = Trans.translate(prog) in
    ref (List.map parseRF tProg)
;;

let query db pred numVars =
  run_eval db pred (list_fill Any numVars)
;;

let rec dump_db db = 
  let print_sig s =
    Printf.printf "%s(%s)" 
      s.name 
      (String.concat "," (List.map string_of_cnst s.params))
  in
  let dump_rf rf = 
    match rf with
	Fact(s) ->
	  print_sig s;
	  print_string ";\n"
      | Rule(s, f) ->
	  print_sig s;
	  print_string " {}\n"
  in
    List.iter dump_rf db
;;
