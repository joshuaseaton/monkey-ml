module Objmap = Map.Make (String)

module Environment = struct
  type t = Object.t Objmap.t

  let create : t = Objmap.empty

  let get_opt (env : t) (name : string) : Object.t option =
    Objmap.find_opt name env

  let set (env : t) (name : string) (obj : Object.t) : t =
    Objmap.add name obj env
end

type error = string

let ( let* ) res f = Result.bind res f

let object_type_string = function
  | Object.Null -> "NULL"
  | Object.Boolean _ -> "BOOLEAN"
  | Object.Integer _ -> "INTEGER"
  | Object.String _ -> "STRING"
  | Object.Function _ -> "FUNCTION"

let rec eval (node : Ast.node) (env : Environment.t) :
    (Object.t * Environment.t, error) result =
  match node with
  | Ast.Program stmts -> eval_statements stmts env
  | Ast.Statement stmt ->
      let* obj, _, env = eval_statement stmt env in
      Ok (obj, env)
  | Ast.Expression expr ->
      let* value = eval_expression expr env in
      Ok (value, env)

and eval_statements (stmts : Ast.statement list) (env : Environment.t) :
    (Object.t * Environment.t, error) result =
  match stmts with
  | [] -> Ok (Object.Null, env)
  | first :: rest ->
      let* obj, return, env = eval_statement first env in
      if return then
        Ok (obj, env)
      else if rest == [] then
        Ok (obj, env)
      else
        eval_statements rest env

(* In the success case, also returns whether the evaluation should be returned
   (i.e., whether it resulted from a return statement) *)
and eval_statement (stmt : Ast.statement) (env : Environment.t) :
    (Object.t * bool * Environment.t, error) result =
  match stmt with
  | Ast.Expression_statement expr ->
      let* value = eval_expression expr env in
      Ok (value, false, env)
  | Ast.Return expr ->
      let* value = eval_expression expr env in
      Ok (value, true, env)
  | Ast.Let (ident, expr) ->
      let* value = eval_expression expr env in
      Ok (Object.Null, false, Environment.set env ident value)

(* Environment is not returned, as the evaluation of an expression should not
   affect the environment in the containing scope. *)
and eval_expression (expr : Ast.expression) (env : Environment.t) :
    (Object.t, error) result =
  match expr with
  | Ast.Integer n -> Ok (Object.Integer n)
  | Ast.Boolean b -> Ok (Object.Boolean b)
  | Ast.String s -> Ok (Object.String s)
  | Ast.Prefix (op, right) -> eval_prefix_expression op right env
  | Ast.Infix (left, op, right) -> eval_infix_expression left op right env
  | Ast.If (cond, cons, alt) -> eval_if_expression cond cons alt env
  | Ast.Identifier name -> (
      match Environment.get_opt env name with
      | None -> Error (Printf.sprintf "unknown identifier: %s" name)
      | Some value -> Ok value)
  | Ast.Function (params, body) -> Ok (Object.Function (params, body))
  | Ast.Call (func, args) -> eval_call_expression func args env

and eval_prefix_expression (op : Token.t) (right : Ast.expression)
    (env : Environment.t) : (Object.t, error) result =
  let invalid_operation op right =
    Printf.sprintf "invalid expression: %s%s" (Token.to_string op)
      (object_type_string right)
  in
  let* right = eval_expression right env in
  match right with
  | Object.Boolean b -> (
      match op with
      | Token.Bang -> Ok (Object.Boolean (not b))
      | _ -> Error (invalid_operation op right))
  | Object.Integer n -> (
      match op with
      | Token.Bang -> Ok (Object.Boolean false)
      | Token.Minus -> Ok (Object.Integer (-n))
      | _ -> Error (invalid_operation op right))
  | _ -> Error (invalid_operation op right)

and eval_infix_expression (left : Ast.expression) (op : Token.t)
    (right : Ast.expression) (env : Environment.t) : (Object.t, error) result =
  let invalid_operation left op right =
    Printf.sprintf "invalid operation: %s %s %s" (object_type_string left)
      (Token.to_string op) (object_type_string right)
  in

  let* left = eval_expression left env in
  let* right = eval_expression right env in
  match (left, right) with
  | Object.Boolean a, Object.Boolean b -> (
      match op with
      | Token.Equal -> Ok (Object.Boolean (a == b))
      | Token.Not_equal -> Ok (Object.Boolean (a != b))
      | _ -> Error (invalid_operation left op right))
  | Object.Integer a, Object.Integer b -> (
      match op with
      | Token.Equal -> Ok (Object.Boolean (a == b))
      | Token.Not_equal -> Ok (Object.Boolean (a != b))
      | Token.Less_than -> Ok (Object.Boolean (a < b))
      | Token.Greater_than -> Ok (Object.Boolean (a > b))
      | Token.Plus -> Ok (Object.Integer (a + b))
      | Token.Minus -> Ok (Object.Integer (a - b))
      | Token.Asterisk -> Ok (Object.Integer (a * b))
      | Token.Slash -> Ok (Object.Integer (a / b))
      | _ -> Error (invalid_operation left op right))
  | Object.String a, Object.String b -> (
      match op with
      | Token.Plus -> Ok (Object.String (a ^ b))
      | Token.Equal -> Ok (Object.Boolean (a == b))
      | Token.Not_equal -> Ok (Object.Boolean (a != b))
      | _ -> Error (invalid_operation left op right))
  | _ -> Error (invalid_operation left op right)

and eval_if_expression (condition : Ast.expression)
    (consequence : Ast.statement list) (alternative : Ast.statement list option)
    (env : Environment.t) : (Object.t, error) result =
  let* truthy =
    let* value = eval_expression condition env in
    match value with
    | Object.Boolean b -> Ok b
    | Object.Null -> Ok false
    | _ -> Ok true
  in
  if truthy then
    let* obj, _ = eval_statements consequence env in
    Ok obj
  else
    match alternative with
    | None -> Ok Object.Null
    | Some expr ->
        let* obj, _ = eval_statements expr env in
        Ok obj

and eval_call_expression (func : Ast.expression) (args : Ast.expression list)
    (env : Environment.t) : (Object.t, error) result =
  (* This is an accumulator for a fold over the lists of parameters and
     substitutions. It substitutes and returns the updated environment until it
     reaches an evaluation error, at which point it just forwards that. *)
  let substitute_param param arg env_or_err =
    match env_or_err with
    | Error _ -> env_or_err
    | Ok env ->
        let* value = eval_expression arg env in
        Ok (Environment.set env param value)
  in

  let* value = eval_expression func env in
  match value with
  | Object.Function (params, body) -> (
      match List.fold_right2 substitute_param params args (Ok env) with
      | exception Stdlib.Invalid_argument _ ->
          let len = List.length params in
          Error
            (Printf.sprintf "%s takes %d argument%s; %d provided"
               (Ast.expression_to_string func)
               len
               (if len > 1 then
                  "s"
                else
                  "")
               (List.length args))
      | Error err -> Error err
      | Ok env ->
          let* obj, _ = eval_statements body env in
          Ok obj)
  | _ ->
      Error
        (Printf.sprintf "%s is not a function" (Ast.expression_to_string func))
