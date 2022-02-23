type type_constraint =
| SameType of Type.t * Type.t
[@@deriving show]

type type_assumption = string * Type.t
[@@deriving show]
let remove_assumption (name : string) (l : type_assumption list) : type_assumption list =
  l |> List.filter (fun (n, _) -> n <> name)

let find_assumptions (name : string) (l : type_assumption list) : Type.t list =
  l |> List.filter (fun (n, _) -> n = name) |> List.map snd


module M = Map.Make(String)
module S = Set.Make(String) [@@deriving show]

type subst = Type.t M.t
let rec sub_type (s: subst) (t : Type.t) : Type.t = match t with
| Type.VarT v -> (match M.find_opt v s with
  | Some t' -> sub_type s t'
  | None -> t)
| Type.FunT (t1, t2) -> Type.FunT (sub_type s t1, sub_type s t2)
| _ -> t

let sub_constraint (s: subst) c = match c with
| SameType (t1, t2) -> SameType (sub_type s t1, sub_type s t2)


let sub_union (s1 : subst) (s2 : subst) : subst =
  M.map (sub_type s1) s2 |> M.union (fun _ v1 v2 -> assert (v1 = v2); Some v1) s1
  


exception TypeMismatch of Type.t * Type.t


let rec unify (t1 : Type.t) (t2 : Type.t) : subst = 
  let bind n t =
    if S.mem n (Type.free_type_vars t) then raise (Failure "Infinite type")
    else M.singleton n t in
  match (t1, t2) with
| _ when t1 = t2 -> M.empty
| (VarT v, _) -> bind v t2
| (_, VarT v) -> bind v t1

| (FunT (t11, t12), FunT (t21, t22)) ->
  let s1 = unify t11 t21 in
  let s2 = unify (sub_type s1 t12) (sub_type s1 t22) in
  sub_union s1 s2
| _ -> raise @@ TypeMismatch (t1, t2)


type 't tast =
| Primitive of Type.primitive
| Variable of Type.var
| Lambda of 't * Type.var * 't tast
| Application of 't tast * 't tast
| If of 't tast * 't tast * 't tast
[@@deriving show]


type env = S.t

let globals = List.map 
  (fun Builtin_constants.{ internal_name; ty; _ } -> (internal_name, ty) ) 
  Builtin_constants.builtins  |> List.to_seq |> M.of_seq |> ref

let rec infer (env : env) (expr : Dast.dast) : 
  (type_assumption list * type_constraint list * Type.t * Type.t tast) = match expr with
| Dast.Primitive ((Type.Int _) as p) -> ([], [], Type.IntT, Primitive p)
| Dast.Primitive ((Type.Bool _) as p) -> ([], [], Type.BoolT, Primitive p)
| Dast.TypeAnnotation (t, e) ->
  let (sl, cl, t', e') = infer env e in
  (sl, SameType (t, t') :: cl, t', e')
| Dast.Variable name ->
  if String.starts_with ~prefix:"__builtin" name 
  then ([], [], M.find name !globals, Variable name)
  else
    let v = Common.gen_name name in
    let tv = Type.VarT v in
    ([(name, tv)], [], tv, Variable name)
| Dast.Application (e1, e2) ->
  let (sl1, cl1, t1, e1') = infer env e1 in
  let (sl2, cl2, t2, e2') = infer env e2 in
  let tv = Type.VarT (Common.gen_name "ret") in
  (
    sl1 @ sl2,
    SameType (t1, FunT (t2, tv)) :: cl1 @ cl2,
    tv,
    Application (e1', e2')
  )
| Dast.Lambda (name, e) ->
  let a = Common.gen_name (name ^ "_arg") in
  let ta = Type.VarT a in
  let (sl, cl, te, e') = infer env e in
  (
    remove_assumption name sl, 
    cl @ List.map (fun t -> SameType (ta, t)) (find_assumptions name sl),
    FunT (ta, te),
    Lambda (FunT (ta, te), name, e')
  )
| Dast.If (e1, e2, e3) ->
  let (sl1, cl1, t1, e1') = infer env e1 in
  let (sl2, cl2, t2, e2') = infer env e2 in
  let (sl3, cl3, t3, e3') = infer env e3 in
  (
    sl1 @ sl2 @ sl3,
    SameType (t2, t3) :: SameType (t1, BoolT) :: cl1 @ cl2 @ cl3,
    t2,
    If (e1', e2', e3')
  )
| Dast.Define _ -> raise @@ Failure "Not implemented"




let rec solve (cl : type_constraint list) : subst = match cl with
| [] -> M.empty
| (SameType (t1, t2) :: cl) ->
  let su1 = unify t1 t2 in
  let su2 = solve (List.map (sub_constraint su1) cl) in
  sub_union su1 su2 



let generalize (bound_type_vars : S.t) (t : Type.t) : Type.scheme =
  Scheme (S.diff (Type.free_type_vars t) bound_type_vars |> S.to_seq |> List.of_seq, t)


let rec sub_expr (sub : subst) (bound_type_vars : S.t) (e : Type.t tast) : Type.scheme tast = 
  let sub_expr' = sub_expr sub bound_type_vars in
  match e with
| Primitive p -> Primitive p
| Variable name -> Variable name
| Application (e1, e2) -> Application (sub_expr' e1, sub_expr' e2)
| Lambda (t, name, e) -> 
  let Scheme (names, _) as sc = generalize (bound_type_vars) (sub_type sub t) in
  let e' = sub_expr sub (S.union bound_type_vars (S.of_list names)) e in
  Lambda (sc, name, e')
| If (e1, e2, e3) -> If (sub_expr' e1, sub_expr' e2, sub_expr' e3)


let dast_to_tast (d: Dast.dast) : Type.scheme tast =
  let (sl, cl, _, e) = infer S.empty d in
  assert (List.length sl = 0);

  (* cl |> List.map show_type_constraint |> String.concat "\n" |> print_endline; *)

  let sub = solve cl in


  (* sub 
    |> M.to_seq 
    |> List.of_seq 
    |> List.map (fun (k, v) -> k ^ " = " ^ Type.show v)
    |> String.concat "\n" |> print_endline; *)

  sub_expr sub S.empty e