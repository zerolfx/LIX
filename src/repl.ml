let process (line: string) =
  let linebuf = Lexing.from_string line in
  try
    let code = Parser.code_eof Lexer.token linebuf in
    Syntax.print_code code;
    let ast = Ast.code_to_ast code in
    Ast.show_ast ast |> print_endline;
    let hast = Hast.ast_to_hast ast in
    Hast.show_hast hast  |> print_endline;
    let last = Last.hast_to_last hast in
    Last.show_last last  |> print_endline;
    Compiler.codegen_repl last
  with
  | Parser.Error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf)
  | e -> Printexc.print_backtrace stdout; flush stdout; Printexc.to_string e |> print_endline


let rec repl () =
  print_string "> "; flush stdout;
  let option_line = Lexer.line (Lexing.from_channel stdin) in
  match option_line with
  | None -> 
    print_string "Bye.\n"
  | Some line ->
    process line;
    repl ()

let () = 
  Llvm.enable_pretty_stacktrace ();
  repl ()