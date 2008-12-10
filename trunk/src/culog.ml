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
	(print_string "\n ^^^  Solution  ^^^ \n\n");
	iter_sols (n ())
;;

let myDBD db = 
  print_string "Database dump:\n";
  dump_db !db;
  print_string "\n";;

let _ = 
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
  let program = Parser.program Scanner.token lexbuf in
  let pDB = parseDB(program) in
    (* myDBD pDB; *)
    (let sGen = query pDB "main" 0 in
       iter_sols sGen);
    (* myDBD pDB; *)
;;
