let code_to_last c = c 
  |> Ast.code_to_ast
  |> Dast.ast_to_dast
  |> Hast.dast_to_hast
  |> Last.hast_to_last