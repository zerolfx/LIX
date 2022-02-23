let code_to_last c = 
  let tast, sc = c
  |> Ast.code_to_ast
  |> Dast.ast_to_dast
  |> Tast.dast_to_tast in
  (Last.tast_to_last tast, sc)