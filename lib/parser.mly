%{
    open Syntax
%}

%token LPAREN RPAREN LCURLY RCURLY
%token <int> INT_LIT
%token <bool> BOOL_LIT
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
| LCURLY; name = Symbol; code_list = list(code); RCURLY; { CForm (name, code_list) }
| x = INT_LIT; { IntLit x }
| x = BOOL_LIT; { BoolLit x }
| symbol = Symbol; { Symbol symbol }

let code_eof := ~ = code; EOF; <>
