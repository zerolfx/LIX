open Lix

let process (line: string) =
  let linebuf = Lexing.from_string line in
  try
    let code = Parser.code_eof Lexer.token linebuf in
    Syntax.show_code code |> print_endline;
    let ast = Ast.code_to_ast code in
    Ast.show_ast ast |> print_endline;
    let dast = ast |> Dast.ast_to_dast in
    Dast.show_dast dast |> print_endline;
    let tast, sc = dast |> Tast.dast_to_tast in
    Tast.show_tast (Type.pp_scheme) tast |> print_endline;
    let last = tast |> Last.tast_to_last in
    Last.show_last last  |> print_endline;
    let (result_type, result) = Jit.codegen_repl (last, sc) in
    Printf.printf "Result type: %s\n" (Type.show_scheme result_type);
    match result with
    | Some v -> Printf.printf "Eval to: %s\n" (Type.show_primitive v)
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
  | Some "dump\n" -> 
    Llvm.dump_module Compiler_common.the_module;
    repl ()
  | Some line ->
    process line;
    repl ()

let () = 
  Llvm.enable_pretty_stacktrace ();
  Jit.init_jit ();
  repl ()