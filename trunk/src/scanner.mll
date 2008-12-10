{ open Parser }
rule token = parse
    [' ' '\t' '\r' '\n'] { token lexbuf }
  | "/*"     { comment lexbuf }
  | "//"     { linecomment lexbuf }
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | ';'      { SEMICOLON }
  | ','      { COMMA }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { TIMES }
  | '/'      { DIVIDE }
  | "==" | "="  { EQ }
  | "!="     { NEQ }
  | '<'      { LT }
  | "<="     { LEQ }
  | ">"      { GT }
  | ">="     { GEQ }
  | '@'      { AT } 
  | '.'	     { DOT }
  | ':'      { COLON }
  | '['      { ARROPEN }
  | ']'      { ARRCLOSE }
  | '"'	     { QUOTE} 
  | '?'	     { QUESTION }
  | '!'	     { NOT}		
  | '$'['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as var { VARIABLE(var) }
  | ['0'-'9']+ as lxm { DIGIT(int_of_string lxm) }
  | [ 'a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' ]* as lxm { ID(lxm) }
  | '"'([^ '"''\t' '\r' '\n']+ as lxm) '"'{STRING1(lxm)}
  | eof 	   { EOF }

and  comment = parse
    "*/" { token lexbuf }
  | _  { comment lexbuf }
      
and linecomment = parse
    ['\r' '\n'] {token lexbuf}
  | _ {linecomment lexbuf}
      



