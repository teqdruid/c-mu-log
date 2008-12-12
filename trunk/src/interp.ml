(*
 *  interp.ml
 *
 *  This guy is the interpreter... It "compiles" the TST to a bunch of OCaml
 *     functions to be run during a query.
 *
 *  You'll quickly be able to tell that this whole method is _begging_ for co-routines.
 *    Lazy evaluation could be beneficial here as well.
 *
 * 
 *  John Demme
 * 
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
  | CEqlAgent  of database

and cnst = var_cnst list

and signature = {
  name   : string;
  params : cnst
}

and next = 
    NoSolution
  | Solution of cnst * (unit -> next)

and rule_fact = 
    Fact of signature
  | Rule of signature * (database -> database -> cnst -> next)

and database = rule_fact list ref
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
  | CEqlAgent(a) -> "Agent"
;;

let string_of_eval name vars =
  name ^ "(" ^ String.concat "," (List.map string_of_cnst vars) ^")\n"
;;

let cAnd a b =
  let rec and_int a b t = 
    match (a, b) with
	(Any, _) -> b
      | (_, Any) -> a
      | (CEqlAgent(a1),  CEqlAgent(a2))  when (a1 == a2) -> a
      | (CEqlSymbol(s1), CEqlSymbol(s2)) when (0 == String.compare s1 s2) -> a
      | (CEqlStr(s1),    CEqlStr(s2))    when (0 == String.compare s1 s2) -> a
      | (CEqlInt(i1),    CEqlInt(i2))    when (i2 == i2) -> a
      | (CEqlInt(i1),    CGT(i2))        when (i1 > i2) -> a
      | (CEqlInt(i1),    CLT(i2))        when (i1 < i2) -> a
      | (CLT(i1),        CLT(i2))        -> CLT(min i1 i2)
      | (CGT(i1),        CGT(i2))        -> CLT(max i1 i2)
      | (CGT(i1),        CLT(i2))        when (i1 < i2) -> CRange(i1, i2)
      | (CRange(l, u),   CEqlInt(i))     when (i > l && i < u) -> b
      | (CRange(l, u),   CGT(i))         when (i < u - 1) -> CRange((max l i), u)
      | (CRange(l, u),   CLT(i))         when (i > l + 1) -> CRange(l, (min u i))
      | (CRange(l1,u1),  CRange(l2,u2))  when (u1 > l2 && u2 > l1) -> CRange((max l1 l2), (min u1 u2))
      | (_, _) when t -> and_int b a false
      | (_, _) -> FalseSol
  in
    and_int a b true
;;

let range_to_int c =
  match c with
      CRange(l, u) when (l + 2 == u) -> CEqlInt(l+1)
    | _ -> c
;;

let int_to_range c =
  match c with
      CEqlInt(i) -> CRange(i-1, i+1)
    | _ -> c
;;

(* For each constraint, subtract the second from the first *)
(* TODO:  There are off-by-one errors in here... Fix when you have a clearer head *)
let cMinus b s = 
  let cmi b s = 
    (* Too many combinations and no play makes Johnny go something something *)
    match (b, s) with
	(_, Any) ->
	  []
      | (Any, _) ->
	  failwith "Unsupported subtraction- need != constraint"
      | (CEqlSymbol(s1), CEqlSymbol(s2)) when (0 != String.compare s1 s2) ->
	  [b; s]
      | (CEqlStr(s1), CEqlStr(s2)) when (0 != String.compare s1 s2) ->
	  [b; s]
      | (CEqlInt(i1), CEqlInt(i2)) when (i1 != i2) ->
	  [b; s]
      | (CEqlAgent(a1), CEqlAgent(a2)) when (a1 != a2) ->
	  [b; s]
      | (CRange(bl, bu), CRange(sl, su)) when (bl < sl && bu > su) -> 
	  [CRange(bl,sl); CRange(su, bu)]
      | (CRange(bl, bu), CRange(sl, su)) when (bl < sl && bu < su) -> 
	  [CRange(bl, min sl bu)]
      | (CRange(bl, bu), CRange(sl, su)) when (bl < sl && bu > su) -> 
	  [CRange(max bl su, bu)]
      | (CRange(bl, bu), CRange(sl, su)) when (bu > sl || bl > su) -> 
	  [b]
      | (CGT(bi), CLT(si)) -> [CGT(max bi si)]
      | (CLT(bi), CGT(si)) -> [CLT(min bi si)]
      | (CGT(bi), CGT(si)) when (bi < si) -> [CRange(bi, si)]
      | (CLT(bi), CLT(si)) when (bi < si) -> [CRange(si, bi)]
      | _ -> []
  in
    List.map range_to_int (cmi (int_to_range b) (int_to_range s))
;;

let list_acc mapper list = 
  let rec acc list ret = 
    match list with
	[] -> ret
      | hd :: tl -> acc tl ((mapper hd) @ ret)
  in
    acc list []
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

let rec remove_fact1 db pred cnsts = 
  (* print_string ("Removing: "^ (string_of_eval pred cnsts) ^ "\n"); *)
  match db with 
      [] -> []
    | Fact(sign) :: tl when match_signature sign pred cnsts -> tl
    | hd :: tl -> hd :: (remove_fact1 tl pred cnsts)
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
let rec run_eval db addDB name vars =
  let rec run_gen tail nextGen =
    let sols = (nextGen ()) in
      match sols with
	  NoSolution -> eval_loop tail
	| Solution (cnst, gen) -> 
	    Solution(
	      (list_first (List.length vars) cnst), 
	      (fun unit -> run_gen tail gen))
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
	    run_gen tail (fun unit -> exec db addDB matchedVars)
      | head :: tail -> eval_loop tail
  in
    (*print_string ("In: " ^ (string_of_eval name vars));*)
    eval_loop (!addDB @ !db)
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

  (* Our only compiler directive is print, for now.
     learn/forget have a special syntax *)
  let parseCompilerDirective name params =
    let nc = String.compare name in
      (* Print something... probably just "Hello World" *)
      if (nc "print") == 0
      then
	fun db addDB cnst ->
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
	 fun db addDB cnst ->
	   NoSolution)
  in


 (* Compute AND blocks by cANDing all the solutions in each row
  *  of the cross product of all the possible solutions 
  *)
  let rec parseAndBlock stmts = 
    match stmts with
	[] -> (fun db addDB cnst -> Solution (cnst, fun unit -> NoSolution))
      | stmt :: tail ->
	  let nextStatement = (parseAndBlock tail) in
	  let thisStatement = (parseStatement stmt) in
	    fun db addDB cnst ->
	      let nextGenMain = (nextStatement db addDB) in
	      let rec runThisGens thisGen =
		match (thisGen ()) with
		    NoSolution -> NoSolution
		  | Solution (thisCnsts, thisGenNxt) ->
		      let rec runNextGens nextGen =
			match (nextGen ()) with
			    NoSolution ->
			      runThisGens thisGenNxt
			  | Solution(nextCnsts, nextGenNxt) ->
			      Solution(nextCnsts, fun unit -> runNextGens nextGenNxt)
		      in
			runNextGens (fun unit -> nextGenMain thisCnsts)
	      in
		runThisGens (fun unit -> thisStatement db addDB cnst)


  (* Return all the solutions from one, then go to the next *)
  and parseOrBlock stmts = 
    match stmts with
	[] -> (fun db addDB cnst -> NoSolution)
      | stmt :: tail ->
	  let nextStmt = (parseOrBlock tail) in
	  let currStmt = (parseStatement stmt) in
	    fun db addDB cnst ->	    
	      let rec runOr nxt = 
		match nxt with
		    NoSolution -> nextStmt db addDB cnst
		  | Solution(vars, nxt) -> Solution(vars, 
						    (fun unit -> runOr (nxt ())))
	      in
		runOr (currStmt db addDB cnst)



  (* Return the results from a query *)
  and parseEval name params =
    fun db addDB cnst -> 
       let cnsts = cnst_of_params params cnst in
	 (* Map the slots returned from the eval into our slot-space *)
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
	 (* Run the eval, then send back the results, reverse mapping the slots as we go *)
       let nxt = run_eval db addDB name cnsts in
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


 (* ********   BEHOLD ---- The bane of my existence!!!!!!    *)

 (* A dumber man could not have written this function...
  *   ... A smarter man would have known not to.
  *)
  and parseNotEval name params =
    let eval = parseEval name params in
      fun db addDB cnsts ->
	(* It probably will help to think of this function as a binary blob... 
	 *    I blacked out while I was writing it, but I remember it having
	 *    something to do with lazily-generated cross products. ~John
	 *)
	let rec iter_outs bigList = 
	  (* Printf.printf "%s\n" (string_of_eval "Level:" (List.hd bigList)); *)
	  match bigList with
	      [] -> failwith "Internal error 23"
	    | myRow :: [] -> 
		let rec linearGen myList =
		  match myList with
		      [] -> NoSolution
		    | hd :: tl -> Solution([hd], fun unit -> linearGen tl)
		in
		  linearGen myRow
	    | myRow :: tl ->
		let tlGenMain = iter_outs tl in
		let rec twoGen myList nxtGen = 
		  match myList with 
		      [] -> NoSolution
		    | myHd :: myTl ->
			match nxtGen with
			    NoSolution ->
			      twoGen myTl tlGenMain
			  | Solution(sol, nxtGen) ->
			      Solution(myHd :: sol, fun unit -> twoGen myList (nxtGen()))
		in
		  twoGen myRow tlGenMain
	in
	let rec minus nxt outs =
	  match nxt with
	      NoSolution ->
		(* Printf.printf "outs len: %d\n" (List.length outs); *)
		(* Printf.printf "%s\n" (string_of_eval "step outs:" (List.hd outs)); *)
		iter_outs outs
	    | Solution(evCnsts, nxt) ->
		(* Printf.printf "%s\n" (string_of_eval "step outs:" (List.hd outs)); *)
		(* Printf.printf "%s\n" (string_of_eval "step evcn:" evCnsts); *)
		minus
		  (nxt())
		  (List.map2
		     (fun out evCnst ->
			list_acc (fun o -> cMinus o evCnst) out)
		     outs
		     evCnsts)
	in
	  minus (eval db addDB cnsts) [cnsts]


  (* Run an eval in somebody else's database *)
  and parseDot2 v pred params = 
    let eval = parseEval pred params in
      (fun db addDB cnst -> 
	 match (List.nth cnst v) with
	     CEqlAgent(adb) -> 
	       eval adb (ref []) cnst
	   | a -> (Printf.printf 
		     "Warning: attempted dot ('.') on a non-agent: %s\n"
		     (string_of_cnst a);
		   NoSolution))


  and doAnd myCnsts db addDB cnst =
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
    let remove_facts db addDB cnsts =
      let remove_fact (name,params) =
	db := remove_fact_all !db name (cnst_of_params params cnsts)
      in
	List.iter remove_fact statements
    in
    let remove_fact1 db addDB cnsts =
      let remove_fact (name,params) =
	db := remove_fact1 !db name (cnst_of_params params cnsts)
      in
	List.iter remove_fact statements
    in
    let add_facts db addDB cnsts =
      let add_fact (name,params) =
	db := Fact({name = name; params = (cnst_of_params params cnsts)}) :: !db
      in
	List.iter add_fact statements
    in
    let nm = String.compare name in
      if (nm "learn") == 0
      then (fun db addDB cnsts -> add_facts db addDB cnsts; NoSolution)
      else if (nm "forget") == 0 
      then (fun db addDB cnsts -> remove_facts db addDB cnsts; NoSolution)
      else if (nm "forget1") == 0 
      then (fun db addDB cnsts -> remove_fact1 db addDB cnsts; NoSolution)
      else failwith ("Invalid directive: " ^ name)
  and parseDot1 v dname statements = 
    let study = parseLearnForget dname statements in
      (fun db addDB cnst -> 
	 match (List.nth cnst v) with
	     CEqlAgent(adb) -> 
	       study adb (ref []) cnst
	   | a -> (Printf.printf 
		     "Warning: attempted @ dot ('.') on a non-agent: %s\n"
		     (string_of_cnst a);
		   NoSolution))
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
	   (fun db addDB cnst -> NoSolution))
      | Tst.Eval (name,   params) -> 
	  parseEval name params
      | Tst.NEval (name,   params) -> 
	  parseNotEval name params
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
      | Tst.Dot1(v, dname, statements) ->
	  parseDot1 v dname statements
      | Tst.Dot2(v, pred, params) ->
	  parseDot2 v pred params
  in
  let parseRule stmt slots actions = 
    fun db addDB inCnsts ->      
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
		      (ignore (action db addDB outCnsts))) 
		   actions;
		 Solution(outCnsts, fun () -> runPer (outCnsts :: sols)(nxt())))
      in
	(* print_string ("Num slots: " ^ (string_of_int slots) ^ "\n"); *)
	runPer [] (stmt db addDB (cnst_extend_to inCnsts slots))
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

let query db addDB pred numVars =
  run_eval db addDB pred (list_fill Any numVars)
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
