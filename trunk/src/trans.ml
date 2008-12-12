(* Functions to modify the AST slightly 
   to make parsing it easier for the interpreter.
   
   Static checking could happen here as well. *)

open Ast

module StringMap = Map.Make(String);;

(* Give me the number of items in a StringMap *)
let map_length sMap = 
  let fLength k a b =
    b + 1
  in
    StringMap.fold fLength sMap 0
;;

(* Use me with List.fold to get a maximum index *)
let max_index s i l =
  if i > l
  then i
  else l
;;

(* Print all items in a StringMap *)
let smPrint key a =
  Printf.printf "%s: %d\n" key a; ()
;;

(* Get a variable name to variable number binding from a rule *)
let getBindings mRule = 
  (* TODO: Many of these functions could be made nicer using stuff like List.fold *)
  let add_binding var bindings =
    if (StringMap.mem var bindings) then
      bindings
    else
      (StringMap.add var (map_length bindings) bindings)
  in
  let rec get_params_var_mapping params bindings = 
    let len = map_length bindings in
      match params with
	  [] -> bindings
	| Var(name) :: tail ->
	    get_params_var_mapping tail (StringMap.add name len bindings)
	| i :: tail -> 
	    get_params_var_mapping tail (StringMap.add (string_of_int len) len bindings)
  in
  let rec get_eval_var_mapping params bindings = 
    match params with
	[] -> bindings
      | Var(name) :: tail ->
	  get_eval_var_mapping tail
	    (add_binding name bindings)
      | _ :: tail -> get_eval_var_mapping tail bindings
  in
  let rec get_expr_var_mapping e bindings =
    match e with
	EVar(name) -> add_binding name bindings
      | Binop(a, op, b) -> get_expr_var_mapping a (get_expr_var_mapping b bindings)
      | _ -> bindings
  in
  let rec get_stmts_var_mapping stmts bindings = 
    match stmts with
	[] -> bindings
      | Block(redOp, Stmts(stmts)) :: tail -> 
	  get_stmts_var_mapping tail (get_stmts_var_mapping stmts bindings)
      | Comp(expr1, compOp, expr2) :: tail ->
	  get_stmts_var_mapping tail
	    (get_expr_var_mapping expr1 (get_expr_var_mapping expr2 bindings))
      | Eval(name, Params(params)) :: tail ->
	  get_stmts_var_mapping tail (get_eval_var_mapping params bindings)
      | Directive(name, Params(params)) :: tail ->
	  get_stmts_var_mapping tail (get_eval_var_mapping params bindings)
      | _ :: tail ->
	  get_stmts_var_mapping tail bindings
  in
    match mRule with
	Rule(name, Params(params), stmt) ->
	  get_stmts_var_mapping [stmt] (get_params_var_mapping params StringMap.empty)
      | Fact(name, Params(params)) ->
	  (get_params_var_mapping params StringMap.empty)
      | _ -> StringMap.empty
;;

(* Translate a rule or fact from AST to TST *)
let translate_rule mRule = 
  let bindings = getBindings mRule in
  let bget name =
    StringMap.find name bindings
  in
    (* Translate paramaters using these bindings *)
  let translate_params params = 
    let translate_param param = 
      match param with
	  Var(name) -> Tst.Var(bget name)
	| Lit(i)    -> Tst.Lit(i)
	| Sym(s)    -> Tst.Sym(s)
	| Str(s)    -> Tst.Str(s)
	| Arr(prms) -> failwith "Sorry, arrays are unsupported"
	| Ques      -> Tst.Anon
    in
      List.map translate_param params
  in
  let rec translate_stmts stmts = 
    (* Move the variable to one side, and simplyify to a constant on the other *)
    let translate_comp expr1 op expr2 =
      (* Can this expression be numerically reduced? *)
      let rec can_reduce expr =
	match expr with
	    ELit(i) -> true
	  | Binop(e1, op, e2) -> (can_reduce e1) && (can_reduce e2)
	  | _ -> false
      in
      (* Give me the reverse of an operator *)
      let rev_op op =
	match op with
	    Lt -> Gt
	  | Gt -> Lt
	  | Eq -> Eq
	  | Neq -> Neq
	  | Geq -> Leq
	  | Leq -> Geq
      in
	(* Translate a comparison where the variable is on the LHS *)
      let translate_comp_sv var_expr op expr =
	(* Reduce a constant expression to a literal *)
	let reduce expr =
	    let rec num_reduce expr = 
	      match expr with
		  ELit(i) -> i
		| Binop(e1, op, e2) ->
		    (let re1 = num_reduce e1 in
		     let re2 = num_reduce e2 in
		       match op with
			   Plus -> re1 + re2
			 | Minus -> re1 - re2
			 | Mult -> re1 * re2
			 | Divide -> re1 / re2)
		| _ -> failwith "Internal error 8"
	    in
	      Tst.ELit(num_reduce expr)
	in
	  match var_expr with
	      EVar(name) ->
		(* Can we numerically reduce the RHS? *)
		if not (can_reduce expr)
		then 
		  (* If not, if better be a simple string of symbol comparison *)
		  match (op, expr) with
		      Eq, EStr(s) -> Tst.StrComp(bget name,s)
		    | Eq, EId (s) -> Tst.SymComp(bget name,s)
		    | _ -> failwith "Unsupported comparison"
		else
		  Tst.Comp(bget name, op, reduce expr)
	    | _ -> failwith "Comparison unsupported"
      in
      	(* Does this expression have a variable *)
      let rec has_var expr =
	match expr with
	    EVar(i) -> true
	  | Binop(e1, op, e2) -> (has_var e1) || (has_var e2)
	  | _ -> false
      in
	(* Check each expression for variables *)
      let ev1 = has_var expr1 in
      let ev2 = has_var expr2 in
	if ev1 && ev2
	then failwith "Comparisons with multiple variables are unsupported."
	else if (not ev1) && (not ev2)
	then failwith "Error: Comparison is constant"
	else if ev1
	then translate_comp_sv expr1 op expr2
	else translate_comp_sv expr2 (rev_op op) expr1
    in
    let mapEvList evList = 
      List.map
	(fun ev -> 
	   match ev with
	       (name, Params(plist)) -> 
		 (name,
		  translate_params plist)
	     | (name, Array(alist)) -> 
		 failwith "Syntax error, arrays not permitted as params")
	evList
    in
      (* Translate a single statement *)
    let rec replace_stmt stmt = 
      match stmt with
	  Block(redOp, Stmts(stmts))-> 
	    Tst.Block(redOp, translate_stmts stmts)
	| Comp(expr1, compOp, expr2) ->
	    translate_comp expr1 compOp expr2
	| Eval(name, Params(params)) ->
	    Tst.Eval(name, translate_params params)
	| Dot2(vname, pred, Params(params)) ->
	    Tst.Dot2(bget vname, pred, translate_params params)
	| Directive(n, Params(params)) ->
	    Tst.Directive(n, translate_params params)
	| DirectiveStudy(n, evList) ->
	    Tst.DirectiveStudy(n, mapEvList evList)
	| Dot1(vname, n, evList) ->
	    Tst.Dot1(bget vname, n, mapEvList evList)
	| _ -> failwith "Unsupported statement"
    in
      List.map replace_stmt stmts
  in
  let rec filterSE stmts =
    match stmts with
	[] -> []
      | Tst.Block(redOp, stmts) :: tail ->
	  Tst.Block(redOp, filterSE stmts) :: filterSE tail
      | Tst.Directive(_, _) :: tail ->
	  filterSE tail
      | Tst.DirectiveStudy(_, _) :: tail ->
	  filterSE tail
      | Tst.Dot1(_, _, _) :: tail ->
	  filterSE tail
      | head :: tail  ->
	  head :: filterSE tail
  in
  let rec filterNSE stmts = 
    match stmts with
	[] -> []
      | Tst.Block(redOp, stmts) :: tail ->
	  List.append (filterNSE stmts) (filterNSE tail)
      | Tst.Directive(n, p) :: tail ->
	  Tst.Directive(n, p) :: filterNSE tail
      | Tst.DirectiveStudy(n, p) :: tail ->
	  Tst.DirectiveStudy(n, p) :: filterNSE tail
      | Tst.Dot1(v, n, p) :: tail ->
	  Tst.Dot1(v, n, p) :: filterNSE tail
      | head :: tail  ->
	  filterNSE tail
  in
    match mRule with
	Rule(name, Params(params), stmt) -> 
	  let replacedStmts = translate_stmts [stmt] in
	    Tst.Rule(name, 
		     (translate_params params),
		     1 + (StringMap.fold max_index bindings (-1)),
		     List.hd (filterSE replacedStmts),
		     filterNSE replacedStmts)
      | Fact(name, Params(params)) ->
	  Tst.Fact(name, (translate_params params))
      | _ -> failwith "Unsupported global directive"
;;


let translate prog =
  match prog with
      Program (rfList) -> 
	let newProgram = List.map translate_rule rfList in
	  (*print_string (Printer.string_of_program newProgram);*)
	  newProgram
;;
