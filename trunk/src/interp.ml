(*
*  print.ml
*
*  Started on  Wed Nov  5 15:13:52 2008 John Demme
*  Last update Mon Nov 10 16:02:08 2008 John Demme
*)

(* module StrMap = Map.Make(String);; *)

type var_cnst = 
    Any
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
  | Rule of signature * (cnst -> next);;


let parseDB (prog) = 
  let parseRF = function
      Ast.Rule (name, Ast.Params(parms), statement) -> Fact { name = name; params = parms}
    | Ast.Fact (name, Ast.Params(parms))            -> Fact { name = name; params = parms} 
  in
    match prog with
	Ast.Program (ruleFacts) -> List.map parseRF ruleFacts
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

let rec run_eval db name vars =
  match db with
      [] -> NoSolution
    | Fact (signature) :: tail
	when match_signature signature name vars ->
	Solution (vars, (fun _ -> run_eval tail name vars))
    | head :: tail -> run_eval tail name vars
;;

let rec sig_to_cnst signature =
  let param_to_cnst = function
      Ast.Lit(i) -> CEqlInt    (i)
    | Ast.Sym(s) -> CEqlSymbol (s)
    | Ast.Var(v) -> Any
    | Ast.Str(s) -> CEqlStr    (s) 
    | Ast.Arr(a) -> Any (* TODO: Array Matching *)
  in
    List.map param_to_cnst signature
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
