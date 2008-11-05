(*
*  print.ml
*
*  Started on  Wed Nov  5 15:13:52 2008 John Demme
*  Last update Wed Nov  5 15:25:50 2008 John Demme
*)


let lexbuf = Lexing.from_channel stdin in
let program = Parser.program Scanner.token lexbuf in
let listing = Printer.string_of_program program in  
  print_string listing;;
