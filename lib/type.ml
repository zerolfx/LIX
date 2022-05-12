module S = Set.Make (String)


type t =
| IntT
| BoolT
| FunT of t * t
| VarT of string
| CustomT of string * t list
[@@deriving show, eq, ord]

type scheme = Scheme of string list * t [@@deriving show]

type primitive =
| Int of int
| Bool of bool
[@@deriving show, eq]


type var = string [@@deriving show, eq, ord]
type constructor = string [@@deriving show, eq, ord]
type t_var = var * t [@@deriving show, eq, ord]

type constructor_definition =
| ConstructorDefinition of constructor * t list
[@@deriving show]

type pattern =
| WildcardPattern
| NamePattern of string
| ConstructorPattern of constructor * pattern list
| LiteralPattern of primitive
[@@deriving show]

let rec free_type_vars (t: t) = match t with
| IntT | BoolT -> S.empty
| FunT (t1, t2) -> S.union (free_type_vars t1) (free_type_vars t2)
| VarT v -> S.singleton v
| CustomT (_, ts) -> List.fold_left S.union S.empty (List.map free_type_vars ts)
