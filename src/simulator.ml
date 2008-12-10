open Interp
open Ast

let x_position=1;;
let y_position=1;;
let x_ref=ref x_position;;
let y_ref=ref y_position;;
let grid_size_ref= ref 1;;
let goal_x_ref=ref 3;;
let goal_y_ref=ref 3;; (* define a goal*)
open Printf;;
(*a global array to restore information of wall and agent *)
(* 'o'represent empty grid,'|' represents wall and 'x'represents agent *)
let record=
  let f index='o' in
    Array.init 10000 f ;;
(* maximum grid size is 100*100 *)
let clear_array a=
  for x=0 to (Array.length a)-1 do
    if x=(!grid_size_ref- !goal_y_ref)* !grid_size_ref+ !goal_x_ref-1 then
    a.(x)<-'#'
    else a.(x)<-'o'
  done
;;

let print_array a= 
  for x=0 to (Array.length a)-1 do
    print_char a.(x)
  done
;;

let print_file j arr =
  let file = "Agent"^string_of_int(j)^".dat" in
    (* Write message to file *)

  let oc = open_out file in    (* create or truncate file, return channel *)

    for a=0 to !grid_size_ref-1
    do
      for j= !grid_size_ref*(a) to !grid_size_ref*(a+1)-1
      do
	Printf.fprintf oc "%c " arr.(j)
      done;
      Printf.fprintf oc "\n"

    done;
    close_out oc;;             (* flush and close the channel *)

let print_result name arr =
  let file = name^".dat" in
    (* Write message to file *)

  let oc = open_out file in    (* create or truncate file, return channel *)

    for a=0 to !grid_size_ref-1
    do
      for j= !grid_size_ref*(a) to !grid_size_ref*(a+1)-1
      do
	Printf.fprintf oc "%c " arr.(j)
      done;
      Printf.fprintf oc "\n"

    done;
    close_out oc;;             (* flush and close the channel *)





let create_wall x_start x_end y_start y_end =
  if x_start<1 || x_end> !grid_size_ref then failwith "Creating Wall : x position of wall exceeds the grids"
  else if y_start<1 || y_end> !grid_size_ref then failwith "Creating Wall : y position of wall exceeds the grids"
  else if x_start>x_end || y_start>y_end then failwith "Creating Wall:wrong range!!!"
  else for i=x_start to x_end do
    for j=y_start to y_end do
      record.((!grid_size_ref-j)* !grid_size_ref+i-1)<-'|'
    done
  done
;;

let rec iter_wall nxt=
  match nxt with
      NoSolution -> ()
    | Solution(c,n) -> 
	(match c with
	     [Any; Any] -> ()
	   |[CEqlInt(x);CEqlInt(y)]-> create_wall x x y y
	   |[CEqlInt(x);CLT(y)]-> create_wall x x 1 (y-1)
	   |[CEqlInt(x);CGT(y)]-> create_wall x x (y+1) !grid_size_ref
	   |[CEqlInt(x);CRange(y1,y2)]-> create_wall x x (y1+1) (y2-1)
	   |[CLT(x);CEqlInt(y)]-> create_wall 1 (x-1) y y 
	   |[CLT(x);CLT(y)]-> create_wall 1 (x-1) 1 (y-1)
	   |[CLT(x);CGT(y)]-> create_wall 1 (x-1) (y+1) !grid_size_ref
	   |[CLT(x);CRange(y1,y2)]-> create_wall 1 (x-1) (y1+1) (y2-1) 
	   |[CGT(x);CEqlInt(y)]-> create_wall (x+1) !grid_size_ref y y
	   |[CGT(x);CLT(y)]-> create_wall (x+1) !grid_size_ref 1 (y-1)
	   |[CGT(x);CGT(y)]-> create_wall (x+1) !grid_size_ref (y+1) !grid_size_ref
	   |[CGT(x);CRange(y1,y2)]-> create_wall (x+1) !grid_size_ref (y1+1) (y2-1)
	   |[CRange(x1,x2);CEqlInt(y)]-> create_wall (x1+1) (x2-1) y y
	   |[CRange(x1,x2);CLT(y)]-> create_wall (x1+1) (x2-1) 1 (y-1)
	   |[CRange(x1,x2);CGT(y)]-> create_wall (x1+1) (x2-1) (y+1) !grid_size_ref
	   |[CRange(x1,x2);CRange(y1,y2)]->create_wall (x1+1) (x2-1) (y1+1) (y2-1)
	   | _ -> ());
	iter_wall (n ())
;;

let agent_move direction =
  match direction with 
      "UP"-> y_ref:=!y_ref + 1
    | "DOWN"-> y_ref:=!y_ref - 1
    | "LEFT"-> x_ref:=!x_ref -1
    | "RIGHT"->x_ref:= !x_ref + 1
    | _ ->failwith "No such a direction!"
;;

let rec iter_move nxt =
  (match nxt with
       NoSolution -> ()
     | Solution([c],n)->
	 (match c with 
  	      CEqlStr(dir)->(agent_move dir;					
			     let array_index=(!grid_size_ref - !y_ref)* !grid_size_ref + !x_ref-1 in
			       if !x_ref < 1|| !x_ref > !grid_size_ref then
                                 begin
                                   print_result "agent trace" record;
				   failwith "Hit the margin and Game over!!! "	
                                 end	    
			       else if Array.get record array_index = '|' then
                                 begin
                                   print_result "agent trace" record;
				   failwith "Hit the wall and Game over!!!"
                                 end
                               else if Array.get record array_index = '#' then
                                 begin
                                   print_result "agent trace" record;
                                   failwith "Win!!!Successfully reach the goal!"
                                 end	
			       else record.(array_index)<-'x')
	    | _ -> ());
	 iter_move (n ()) 
     | _ -> failwith "Internal error number 9")
;;


let simulation db=
  let rec loop i database=
    let sGen1=query database "wall" 2 in
      iter_wall sGen1;
      let sGen2 = query database "move" 1 in
	iter_move sGen2;
	print_file i record;
 	(* output record array to file*)

 	(* clear the values in the array*)
	clear_array record;
	loop (i+1) database
  in loop 1 db 
;;




let _ = 
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
  let program = Parser.program Scanner.token lexbuf in
  let pDB = Interp.parseDB(program) in
    (*  simulation pDB*)
  let size=50 in (* get grid size here*)
    grid_size_ref:=size;
    clear_array record;
  let sGen1=query pDB "wall" 2 in
    iter_wall sGen1;
    let sGen2=query pDB "move" 1 in
      iter_move sGen2;
(*      print_array record; *)
      print_file 1 record;
      clear_array record;   
      
;;

(*query grid size; different part of the prgram for environment and agent*)
