(* Functions to modify the AST slightly 
   to make parsing it easier for the interpreter.
   
   Static checking could happen here as well. *)

open Ast

module StringMap = Map.Make(String);;

let map_length sMap = 
  let fLength k a b =
    b + 1
  in
  StringMap.fold fLength sMap 0
;;

let smPrint key a =
  Printf.printf "%s: %d\n" key a; ()
;;

let translate prog = 
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
  let bget name bindings =
    StringMap.find name bindings
  in
  let rec replace_params_var_mapping params bindings = 
    match params with
	[] -> []
      | Var(name) :: tail ->
	  TVar(bget name bindings) :: (replace_params_var_mapping tail bindings)
      | i :: tail -> 
	  i :: (replace_params_var_mapping tail bindings)
  in
  let rec replace_expr_var_mapping e bindings =
    match e with
	EVar(name) -> RVar(bget name bindings)
      | Binop(a, op, b) -> Binop(replace_expr_var_mapping a bindings, op,
				 replace_expr_var_mapping b bindings)
      | i -> i
  in
  let rec replace_stmts_var_mapping stmts bindings = 
    let rec replaceIndv stmt = 
      match stmt with
	  Block(redOp, Stmts(stmts))-> 
	    Block(redOp, Stmts(replace_stmts_var_mapping stmts bindings))
	| Comp(expr1, compOp, expr2) ->
	    Comp(replace_expr_var_mapping expr1 bindings, compOp,
		 replace_expr_var_mapping expr2 bindings)
	| Eval(name, Params(params)) ->
	    Eval(name, Params(replace_params_var_mapping params bindings))
	| Directive(n, Params(params)) ->
	    Directive(n, Params(replace_params_var_mapping params bindings))
	| DirectiveStudy(n, evList) ->
	    let evListMapped = 
	      List.map
		(fun ev -> match ev with
		     (name, Params(plist)) -> 
		       (name,
			Params(replace_params_var_mapping plist bindings))
		   | _ -> ev)
		evList
	    in
	      DirectiveStudy(n, evListMapped)
	| _ -> stmt
    in
      List.map replaceIndv stmts
  in
  let max_index s i l =
    if i > l
    then i
    else l
  in
  let rec filterSE stmts =
    match stmts with
	[] -> []
      | Block(redOp, Stmts(stmts)) :: tail ->
	  Block(redOp, Stmts(filterSE stmts)) :: filterSE tail
      | Directive(n, p) :: tail ->
	  filterSE tail
      | DirectiveStudy(n, p) :: tail ->
	  filterSE tail
      | head :: tail  ->
	  head :: filterSE tail
  in
  let rec filterNSE stmts = 
    match stmts with
	[] -> []
      | Block(redOp, Stmts(stmts)) :: tail ->
	  List.append (filterNSE stmts) (filterNSE tail)
      | Directive(n, p) :: tail ->
	  Directive(n,p) :: filterNSE tail
      | DirectiveStudy(n, p) :: tail ->
	  DirectiveStudy(n,p) :: filterNSE tail
      | head :: tail  ->
	  filterNSE tail
  in
  let rule_translate i = match i with
      Rule(name, Params(params), stmt) -> 
	let bindings = get_stmts_var_mapping [stmt]
	  (get_params_var_mapping params StringMap.empty)
	in
	let replacedStmts = 
	  replace_stmts_var_mapping [stmt] bindings
	in
	  TRule(name,
		Params(replace_params_var_mapping params bindings),
		1 + (StringMap.fold max_index bindings (-1)),
		List.hd (filterSE replacedStmts),
		filterNSE replacedStmts)
    | _ -> i
  in
    match prog with
	Program (rfList) -> 
	  let newProgram = Program (List.map rule_translate rfList) in
	    (*print_string (Printer.string_of_program newProgram);*)
	    newProgram
;;
