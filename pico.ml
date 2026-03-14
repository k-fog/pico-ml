type id = string
type binop = Add | Sub | Mul | Less

type expr =
  | Int of int
  | Bool of bool
  | Var of id
  | BinOp of binop * expr * expr
  | If of expr * expr * expr
  | Let of id * expr * expr
  | Fun of id * expr
  | App of expr * expr

type value = VInt of int | VBool of bool | VClosure of id * expr * env
and env = (id * value) list

let as_int = function VInt n -> n | _ -> failwith "expected int"
let as_bool = function VBool b -> b | _ -> failwith "expected bool"

let rec eval env expr =
  match expr with
  | Int n -> VInt n
  | Bool b -> VBool b
  | Var x -> List.assoc x env
  | BinOp (op, lhs, rhs) -> (
      let lhs = eval env lhs |> as_int in
      let rhs = eval env rhs |> as_int in
      match op with
      | Add -> VInt (lhs + rhs)
      | Sub -> VInt (lhs - rhs)
      | Mul -> VInt (lhs * rhs)
      | Less -> VBool (lhs < rhs))
  | If (c, t, f) ->
      let vc = eval env c in
      if as_bool vc then eval env t else eval env f
  | Let (x, e1, e2) ->
      let v = eval env e1 in
      eval ((x, v) :: env) e2
  | Fun (x, body) -> VClosure (x, body, env)
  | App (e1, e2) -> (
      let vf = eval env e1 in
      let va = eval env e2 in
      match vf with
      | VClosure (x, body, cenv) -> eval ((x, va) :: cenv) body
      | _ -> failwith "attempt to call a non-function")
