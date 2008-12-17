(* simulator.ml
 * This is a simulator for entities interaction. 
 * Specifically, simulator comprises of two parts.
 * Part 1 : Simulator obtains information about agents and environment(grids) and simulate their behaviors
 * Part 2 : Simulator displays and outputs results to external files.
displays how agents move under the environment  
 *
 * Original author: Cheng Cheng for part1
 *                  Nishant Shah for part2
 * Support for loading multi-agents is added by John Demme
 *)

open Interp
open Ast

(* define global references to the parameters of environment*)
let grid_size_ref=ref 1;;
let grid_x_size_ref=ref 1;;
let grid_y_size_ref=ref 1;;
let goal_x_ref=ref 1;;
let goal_y_ref=ref 1;; 

(* define data structure of agent*)
type sim_agent = {
  x   : int;
  y   : int;
  sym : char;
  db  : database
}

(*define a global array to restore information of wall and positions of agents *)
(* maximum environment size is 100*100 *)
(* '.'represents empty grid,'|' represents wall*)
let record=
  let f index='.' in
    Array.init 10000 f ;;

let clear_array a=
  for index=0 to (Array.length a)-1 do
    if index= (!grid_y_size_ref- !goal_y_ref)* !grid_x_size_ref+ !goal_x_ref-1 then
      begin
	a.(index)<-'#'
      end
    else a.(index)<-'.'
  done
;;

let sim_exit s = 
  Printf.printf "\nSimulation over: %s\n\n" s;
  exit(1)
;;

(*set the size of environment *)
let rec set_size nxt=
  match nxt with
      NoSolution-> ()
    | Solution(c,n)->
        (match c with
             [CEqlInt(x);CEqlInt(y)]-> if x<1||x>100 then failwith "the length of grid is illegal!!!"
             else if y<1||y>100 then failwith "the width of grid is not illegal!!! "
             else
               begin
                 grid_x_size_ref:=x;
                 grid_y_size_ref:=y;
                 grid_size_ref:=x*y
               end
           |_-> ())
;;

(* set the goal agents try to reach*)
let rec set_goal nxt=
  match nxt with
      NoSolution-> ()
    | Solution(c,n)->
        (match c with
             [CEqlInt(x);CEqlInt(y)]-> if x<1||x> !grid_x_size_ref then failwith "illegal goal x position"
             else if y<1||y> !grid_x_size_ref then failwith "illegal goal y position"
             else
               begin
                 goal_x_ref:=x;
                 goal_y_ref:=y;
               end
           |_->())
;;

(* display and output the results after interactions*)
let print_grid oc arr = 
  for a=0 to !grid_y_size_ref-1
  do
    for j= !grid_x_size_ref*(a) to !grid_x_size_ref*(a+1)-1
    do
      Printf.fprintf oc "%c " arr.(j)
    done;
    Printf.fprintf oc "\n"
  done
;;

let print_file j arr =
  let file = "Agent"^string_of_int(j)^".dat" in  (* Write message to file *)
  let oc = open_out file in    (* create or truncate file, return channel *)
    (print_grid oc arr;
     close_out oc)
;;
      
let print_stdout j arr = 
  Printf.printf "\n==== Turn %d ====\n" j;
  print_grid stdout arr;
  print_string "\n"
;;

(* create wall in environment*)
let create_wall x_start x_end y_start y_end =
  if x_start<1 || x_end> !grid_x_size_ref then    
    failwith "Creating Wall : x position of wall exceeds the grids"      
  else if y_start<1 || y_end> !grid_y_size_ref then 
    failwith "Creating Wall : y position of wall exceeds the grids"
  else if x_start>x_end || y_start>y_end then failwith "Creating Wall:wrong range!!!"
  else for i=x_start to x_end do
    for j=y_start to y_end do
      record.((!grid_y_size_ref-j)* !grid_x_size_ref+i-1)<-'|'
    done
  done
;;


(* obtain wall information from interpretor and create walls*)
let rec iter_wall nxt=
  match nxt with
      NoSolution -> ()
    | Solution(c,n) -> 
	(match c with
	     [Any; Any]-> create_wall 1 !grid_x_size_ref 1 !grid_y_size_ref
           |[CEqlInt(x);Any]-> create_wall x x 1 !grid_y_size_ref
           |[CLT(x);Any]-> create_wall 1 (x-1) 1 !grid_y_size_ref
           |[CGT(x);Any]-> create_wall (x+1) !grid_x_size_ref 1 !grid_y_size_ref
           |[CRange(x1,x2);Any]->create_wall (x1+1) (x2-1) 1 !grid_y_size_ref
           |[Any;CEqlInt(y)]-> create_wall 1 !grid_x_size_ref y y
           |[Any;CLT(y)]-> create_wall 1 !grid_y_size_ref 1 (y-1)
           |[Any;CGT(y)]-> create_wall 1 !grid_x_size_ref (y+1) !grid_y_size_ref
           |[Any;CRange(y1,y2)]-> create_wall 1 !grid_x_size_ref (y1+1) (y2-1)
	   |[CEqlInt(x);CEqlInt(y)]-> create_wall x x y y
	   |[CEqlInt(x);CLT(y)]-> create_wall x x 1 (y-1)
	   |[CEqlInt(x);CGT(y)]-> create_wall x x (y+1) !grid_y_size_ref
	   |[CEqlInt(x);CRange(y1,y2)]-> create_wall x x (y1+1) (y2-1)
	   |[CLT(x);CEqlInt(y)]-> create_wall 1 (x-1) y y 
	   |[CLT(x);CLT(y)]-> create_wall 1 (x-1) 1 (y-1)
	   |[CLT(x);CGT(y)]-> create_wall 1 (x-1) (y+1) !grid_y_size_ref
	   |[CLT(x);CRange(y1,y2)]-> create_wall 1 (x-1) (y1+1) (y2-1) 
	   |[CGT(x);CEqlInt(y)]-> create_wall (x+1) !grid_x_size_ref y y
	   |[CGT(x);CLT(y)]-> create_wall (x+1) !grid_x_size_ref 1 (y-1)
	   |[CGT(x);CGT(y)]-> create_wall (x+1) !grid_x_size_ref (y+1) !grid_y_size_ref
	   |[CGT(x);CRange(y1,y2)]-> create_wall (x+1) !grid_x_size_ref (y1+1) (y2-1)
	   |[CRange(x1,x2);CEqlInt(y)]-> create_wall (x1+1) (x2-1) y y
	   |[CRange(x1,x2);CLT(y)]-> create_wall (x1+1) (x2-1) 1 (y-1)
	   |[CRange(x1,x2);CGT(y)]-> create_wall (x1+1) (x2-1) (y+1) !grid_y_size_ref
	   |[CRange(x1,x2);CRange(y1,y2)]->create_wall (x1+1) (x2-1) (y1+1) (y2-1)
	   | _ -> ());
	iter_wall (n ())
;;

(* agent moves towards to a direction*)
let agent_move a direction =
  Printf.printf "%c: Moving %s\n" a.sym direction;
  match direction with 
      "UP"   -> {x = a.x; y = a.y + 1; db = a.db; sym = a.sym}
    | "DOWN" -> {x = a.x; y = a.y - 1; db = a.db; sym = a.sym}
    | "LEFT" -> {x = a.x - 1; y = a.y; db = a.db; sym = a.sym}
    | "RIGHT"-> {x = a.x + 1; y = a.y; db = a.db; sym = a.sym}
    | _ ->failwith "No such a direction!"
;;

(*simulator stores the information of agent's move  *)
(*if agent reaches the goal or hits wall, simulator terminates *)
let do_agent_move a = 
  let array_index=(!grid_y_size_ref - a.y)* !grid_x_size_ref + a.x-1 in
    if a.x < 1|| a.x > !grid_x_size_ref then (*x position is beyond range *)
      begin 
        let str=(Char.escaped a.sym)^" hits the y margin and Game over!!! " in                         
	sim_exit str
      end
    else if a.y < 1|| a.y > !grid_y_size_ref then (*y position is beyond range *)
      begin
        let str=(Char.escaped a.sym)^" hits the x margin and Game over!!! " in
	sim_exit str	
      end	    
    else if Array.get record array_index = '|' then
      begin
        let str=(Char.escaped a.sym)^" hits the wall and Game over!!!" in
	sim_exit str
      end
    else if Array.get record array_index = '#' then
      begin
        let str=(Char.escaped a.sym)^" wins!!!Successfully reach the goal at position ("^string_of_int(!goal_x_ref)     ^","^string_of_int(!goal_y_ref)^")" 
        in
        sim_exit str 
      end
    else if (Array.get record array_index) != '.' then
      begin
        let str="Game over!!!Agents crash!!! at position (" ^ string_of_int(a.x)^","^ string_of_int(a.y)^")" in
        sim_exit str
      end	
    else record.(array_index)<- a.sym
;;

(* obtain current position of agent from interpretor and make it move*)
let iter_move agent nxt =
  match nxt with
      NoSolution -> failwith "No Solution"
    | Solution([CEqlStr(dir)], _ ) ->
	let new_agent = agent_move agent dir in
	  ignore (do_agent_move new_agent);
	  new_agent
    | _ -> failwith "Invalid (or no) move"
;;

(* load the databases of all agents in environment*)
let my_loc_db agent all env = 
  ref ([Interp.Fact({name = "loc"; params = [CEqlInt(agent.x);CEqlInt(agent.y)]});
	Interp.Fact({name = "env"; params = [CEqlAgent(env)]})]
       @
	(List.map 
	   (fun other -> 
	      Interp.Fact({name = "agent"; params = [CEqlAgent(other.db)]}))
	      (List.filter (fun a -> a != agent) all)))
;;
(* simulation function*)
let simulation envDB agents =
  let rec loop i agents =
    let sGen_size=query envDB (ref []) "size" 2 in
      set_size sGen_size;
      let sGen_goal=query envDB (ref []) "goal" 2 in
	set_goal sGen_goal;
	clear_array record;
	let sGen_wall=query envDB (ref []) "wall" 2 in
	  iter_wall sGen_wall;
	  let new_agents =
	    List.map
	      (fun agent ->
		 let sGen_move = query agent.db (my_loc_db agent agents envDB) "move" 1 in
		   iter_move agent sGen_move)
	      agents
	  in
	    print_stdout i record;
	    if i>100 then sim_exit "You lose! Can not reach the goal with in 100 steps" 	
	    else loop (i+1) new_agents
  in loop 1 agents
;;


let load_agent db_loc =
  match db_loc with
      (c, s) ->
	let lexbuf1 = Lexing.from_channel (open_in s) in
	let program = Parser.program Scanner.token lexbuf1 in
	  {x=1; y=1; sym = (String.get c 0); db = Interp.parseDB(program)}
;;

(*load database of rules and facts for a single agent*)
let load_db db_loc =
  let lexbuf1 = Lexing.from_channel (open_in db_loc) in
  let program = Parser.program Scanner.token lexbuf1 in
    {x=1; y=1; sym = 'x'; db = Interp.parseDB(program)}
;;


let get_agent_locs db = 
  let rec gal_int res = 
    match res with 
	NoSolution -> []
      | Solution([CEqlStr(c); CEqlStr(s)], nxt) -> (c,s) :: (gal_int (nxt()))
      | _ -> failwith "Failed to load agent"
  in
    gal_int (query db (ref []) "agent" 2)
;;
	  

let _ = 
  let envDB = load_db Sys.argv.(1) in
  let agent_locs = get_agent_locs envDB.db in
    if 0 == (List.length agent_locs)
    then simulation envDB.db [envDB]
    else
      let agentDBs = List.map load_agent agent_locs in
	simulation envDB.db agentDBs
;;

