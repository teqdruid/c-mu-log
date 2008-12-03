(*
*  culog.ml
*
*  Made by (John Demme)
*  Login   <teqdruid@teqBook>
*
*  Started on  Mon Nov 24 16:03:20 2008 John Demme
*  Last update Mon Nov 24 16:03:27 2008 John Demme
*)

open Interp

let rec iter_sols nxt =
  match nxt with
      NoSolution -> print_string "No more solutions\n"
    | Solution(c,n) -> 
	(print_string "Solution\n");
	iter_sols (n ())
;;

let _ = 
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
  let program = Parser.program Scanner.token lexbuf in
  let pDB = parseDB(program) in
    (*print_string "Database dump:\n";
    dump_db pDB;
    print_string "\n";*)
    let sGen = query pDB {name = "main"; params = []} in
      iter_sols sGen
;;
