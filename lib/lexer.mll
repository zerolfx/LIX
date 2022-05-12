{ open Parser }

rule line = parse
| ([^'\n']* '\n') as line
    { Some line }
| eof
    { None }
| ([^'\n']+ as line) eof
    { Some (line ^ "\n") }


and token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LCURLY }
| '}' { RCURLY }
| '-'? ['0'-'9']+ as lxm { INT_LIT(int_of_string lxm) }
| "true" { BOOL_LIT(true) }
| "false" { BOOL_LIT(false) }
| [^ '(' ')' '[' ']' '{' '}' '"' '0'-'9' ' ' '\n' '\r' '\t'][^ '(' ')' '[' ']' '{' '}' '"' ' ' '\n' '\r' '\t']* as lxm { Symbol(lxm) }
| eof { EOF }
