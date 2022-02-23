open OUnit2
let rec list_last_element = function
| [] -> raise (Failure "list_last_element: empty list")
| [x] -> x
| _ :: xs -> list_last_element xs


let eval s = 
  let code_list = Lix.Parser.program Lix.Lexer.token (Lexing.from_string s) in
  let ast_list = List.map Lix.Pipeline.code_to_last code_list in
  List.map Lix.Jit.codegen_repl ast_list |> List.map snd

let assert_eval_int x s = fun _ -> match eval s |> list_last_element with
| Some (Lix.Type.Int y) -> assert_equal x y
| _ -> assert false


let assert_eval_bool x s = fun _ -> match eval s |> list_last_element with
| Some (Lix.Type.Bool y) -> assert_equal x y
| _ -> assert false


let assert_eval_all xs s = fun _ ->
  let ys = eval s in
  let rec check (xs : Lix.Type.primitive list) (ys : Lix.Type.primitive option list) : unit = match xs, ys with
  | _, None::yy -> check xs yy
  | [], [] -> ()
  | _, [] -> ()
  | [], _ -> ()
  | x::xx, Some y::yy -> assert_equal ~cmp:Lix.Type.equal_primitive x y; check xx yy in
  check xs ys
