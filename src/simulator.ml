open Interp
open Ast

let x_position=1;;
let y_position=1;;
let x_ref=ref x_position;;
let y_ref=ref y_position;;
(*a global array to restore information of wall and agent *)
(* 'o'represent empty grid,'|' represents wall and 'x'represents agent *)
let record=
   let f index='o' in
   Array.init 225 f ;;

let clear_array a=
	for x=0 to (Array.length a)-1 do
		a.(x)<-'o'
	done
;;

let create_wall x_start x_end y_start y_end=
	if x_start<1 || x_end>15 then failwith "Creating Wall : x position of wall exceeds the grids"
	else if y_start<1 || y_end>15 then failwith "Creating Wall : y position of wall exceeds the grids"
	else if x_start>x_end || y_start>y_end then failwith "Creating Wall:wrong range!!!"
	else for i=x_start to x_end do
		for j=y_start to y_end do
			record.((15-i)*15+j-1)<-'|'
		done
	     done
;;

let rec iter_wall nxt =
  match nxt with
      NoSolution -> ()
    | Solution(c,n) -> 
		(match c with
	    	 	[Any; Any] -> ()
			|[CEqlInt(x);CEqlInt(y)]-> create_wall x x y y
			|[CEqlInt(x);CLT(y)]-> create_wall x x 1 y
			|[CEqlInt(x);CGT(y)]-> create_wall x x y 15
			|[CEqlInt(x);CRange(y1,y2)]-> create_wall x x y1 y2
			|[CLT(x);CEqlInt(y)]-> create_wall 1 x y y 
			|[CLT(x);CLT(y)]-> create_wall 1 x 1 y
			|[CLT(x);CGT(y)]-> create_wall 1 x y 15
			|[CLT(x);CRange(y1,y2)]-> create_wall 1 x y1 y2 
			|[CGT(x);CEqlInt(y)]-> create_wall x 15 y y
			|[CGT(x);CLT(y)]-> create_wall x 15 1 y
			|[CGT(x);CGT(y)]-> create_wall x 15 y 15
			|[CGT(x);CRange(y1,y2)]-> create_wall x 15 y1 y2
			|[CRange(x1,x2);CEqlInt(y)]-> create_wall x1 x2 y y
			|[CRange(x1,x2);CLT(y)]-> create_wall x1 x2 1 y
			|[CRange(x1,x2);CGT(y)]-> create_wall x1 x2 y 15
			|[CRange(x1,x2);CRange(y1,y2)]-> create_wall x1 x2 y1 y2
	   		| _ -> ());
		iter_wall (n ())
;;

let agent_move direction =
	match direction with 
		"UP"-> y_ref:=!y_ref + 1
	       | "DOWN"-> y_ref:=!y_ref-1
	       | "LEFT"-> x_ref:=!x_ref -1
               | "RIGHT"->x_ref:= !x_ref + 1
	       | _ ->failwith "No such a direction!"
;;

let rec iter_move nxt =
  match nxt with
      NoSolution -> ()
    | Solution([c],n)->(match c with 
  	CEqlSymbol(dir)->(agent_move dir;					
		 let array_index=(15 - !x_ref)*15 + !y_ref-1 in
		    if Array.get record array_index = '|' then
			failwith "Hit the wall and Game over!!!"
		    else if !x_ref < 1|| !x_ref > 15|| !y_ref < 1|| !y_ref >15 then
			failwith "Hit the margin and Game over!!! "
		    else record.(array_index)<-'x')
	| _ -> ())
;;

let simulation db=
let rec loop i database=
    let sGen1=query database{name = "wall"; params = [TVar(0); TVar(1)]} in
    iter_wall sGen1;
    let sGen2 = query database{name = "move"; params=[TVar(0)]} in
    iter_move sGen2;
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
  simulation pDB
 
;;
