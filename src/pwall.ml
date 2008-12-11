(*
*  pwall.ml
*
*  Made by (John Demme)
*  Login   <teqdruid@teqBook>
*
*  Started on  Mon Nov 24 15:57:56 2008 John Demme
*  Last update Mon Nov 24 16:06:53 2008 John Demme
*)

open Interp
open Ast

let print_coord a b =
  print_string "("; print_string (string_of_cnst a); print_string ",";
  print_string (string_of_cnst b); print_string ")\n"
;;

let rec iter_sols nxt =
  match nxt with
      NoSolution -> ()
    | Solution(c,n) -> 
	(match c with
	     [a; b] -> print_coord a b
	   | _ -> ());
	iter_sols (n ())
;;

let _ = 
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
  let program = Parser.program Scanner.token lexbuf in
  let pDB = Interp.parseDB(program) in
    (*print_string "Database dump:\n";
    dump_db pDB;
    print_string "\n";*)
    let sGen = query pDB (ref []) "wall" 2 in
      iter_sols sGen
;;
