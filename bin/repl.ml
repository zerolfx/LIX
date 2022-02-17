open Lix

let process (line: string) =
  let linebuf = Lexing.from_string line in
  try
    let code = Parser.code_eof Lexer.token linebuf in
    Syntax.show_code code |> print_endline;
    let ast = Ast.code_to_ast code in
    Ast.show_ast ast |> print_endline;
    let hast = Hast.ast_to_hast ast in
    Hast.show_hast hast  |> print_endline;
    let last = Last.hast_to_last hast in
    Last.show_last last  |> print_endline;
    let (result_type, result) = Jit.codegen_repl last in
    Printf.printf "Result type: %s\n" (Type.show result_type);
    match result with
    | Some v -> Printf.printf "Eval to: %s\n" (Ast.show_primitive v)
    | _ -> ()
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
  Jit.init_jit ();
  repl ()