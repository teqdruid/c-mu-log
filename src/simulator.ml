open Interp
open Ast


let grid_size_ref=ref 1;;
let grid_x_size_ref=ref 1;;
let grid_y_size_ref=ref 1;;
let goal_x_ref=ref 1;;
let goal_y_ref=ref 1;; (* define a goal*)
let x_position=1;;
let y_position=1;;
let x_ref=ref x_position;;
let y_ref=ref y_position;;
open Printf;;
(*a global array to restore information of wall and agent *)
(* 'o'represent empty grid,'|' represents wall and 'x'represents agent *)
let record=
  let f index='o' in
    Array.init 10000 f ;;
(* maximum grid size is 100*100 *)
let clear_array a=
  for index=0 to (Array.length a)-1 do
    if index= (!grid_y_size_ref- !goal_y_ref)* !grid_x_size_ref+ !goal_x_ref-1 then
      begin
	a.(index)<-'#'
      end
    else a.(index)<-'o'
  done
;;

let sim_exit s = 
  Printf.printf "\nSimulation over: %s\n\n" s;
  exit(1)
;;

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
                 (* print_int x;print_char '|';print_int y*)
           |_-> ())
;;

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
  let file = "Agent"^string_of_int(j)^".dat" in
    (* Write message to file *)

  let oc = open_out file in    (* create or truncate file, return channel *)
    (print_grid oc arr;
     close_out oc)
;;             (* flush and close the channel *)

let print_stdout j arr = 
  Printf.printf "\n==== Turn %d ====\n" j;
  print_grid stdout arr;
  print_string "\n"
;;

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

let agent_move direction =
  Printf.printf "Moving: %s\n" direction;
  match direction with 
      "UP"-> y_ref:=!y_ref + 1
    | "DOWN"-> y_ref:=!y_ref - 1
    | "LEFT"-> x_ref:=!x_ref -1
    | "RIGHT"->x_ref:= !x_ref + 1
    | _ ->failwith "No such a direction!"
;;

let rec iter_move nxt =
  (match nxt with
       NoSolution-> ()
     | Solution([c],n)->
	 (match c with 
  	      CEqlStr(dir)->(agent_move dir;					
			     let array_index=(!grid_y_size_ref - !y_ref)* !grid_x_size_ref + !x_ref-1 in
			       if !x_ref < 1|| !x_ref > !grid_x_size_ref then (*x position is beyond range *)
                                 begin                           
				   sim_exit "Hit the y margin and Game over!!! "	
                                 end
                               else if !y_ref < 1|| !y_ref > !grid_y_size_ref then (*y position is beyond range *)
                                 begin
				   sim_exit "Hit the x margin and Game over!!! "	
                                 end	    
			       else if Array.get record array_index = '|' then
                                 begin
				   sim_exit "Hit the wall and Game over!!!"
                                 end
                               else if Array.get record array_index = '#' then
                                 begin
                                   sim_exit "Win!!!Successfully reach the goal!"
                                 end	
			       else record.(array_index)<-'x')
	    | _ -> ())

     | _ -> failwith "Internal error number 9")
;;


let simulation db =
  let rec loop i database=
    let sGen_size=query database "size" 2 in
      set_size sGen_size;
      let sGen_goal=query database "goal" 2 in
	set_goal sGen_goal;
	clear_array record;
	let sGen_wall=query database "wall" 2 in
	  iter_wall sGen_wall;
	  let sGen_move=query database "move" 1 in
	    iter_move sGen_move;
	    (* print_file i record; *)
	    print_stdout i record;
	    if i>100 then sim_exit "You lose!Can not reach the goal with in 100 steps" 	
	    else loop (i+1) database
  in loop 1 db 
;;

let _ = 
  let lexbuf1 = Lexing.from_channel (open_in Sys.argv.(1)) in
  let program = Parser.program Scanner.token lexbuf1 in
  let pDB = Interp.parseDB(program) in
    simulation pDB
;;

(*query grid size; different part of the prgram for environment and agent*)
