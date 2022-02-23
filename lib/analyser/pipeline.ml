let code_to_last c = c 
  |> Ast.code_to_ast
  |> Dast.ast_to_dast
  |> Tast.dast_to_tast
  |> Last.tast_to_last 