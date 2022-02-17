%{
    open Syntax
%}

%token LPAREN RPAREN
%token <int> INT_LIT
%token <string> Symbol
%token EOF


%start program code_eof
%type <code list> program
%type <code> code code_eof

%%


let program :=
  ~ = list(code); EOF; <>

let code :=
| LPAREN; code_list = list(code); RPAREN; { Form code_list }
| x = INT_LIT; { IntLit x }
| symbol = Symbol; { Symbol symbol }

let code_eof := ~ = code; EOF; <>