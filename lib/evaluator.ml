type error = string

module Environment = struct
  module Objmap = Map.Make (String)

  type t = Object.t Objmap.t

  type builtin = {
    name : string;
    value : Object.t;
  }

  let builtins =
    [
      { name = "len"; value = Object.Builtin Object.Len };
      { name = "first"; value = Object.Builtin Object.First };
      { name = "last"; value = Object.Builtin Object.Last };
      { name = "rest"; value = Object.Builtin Object.Rest };
      { name = "push"; value = Object.Builtin Object.Push };
      { name = "puts"; value = Object.Builtin Object.Puts };
    ]

  let get_opt (env : t) (name : string) : Object.t option =
    Objmap.find_opt name env

  let set (env : t) (name : string) (obj : Object.t) : (t, error) result =
    if List.mem name (List.map (fun builtin -> builtin.name) builtins) then
      Error
        (Printf.sprintf "`%s` is a built-in function and cannot be redefined"
           name)
    else
      Ok (Objmap.add name obj env)

  let create : t =
    let set_builtin builtin env = Objmap.add builtin.name builtin.value env in
    List.fold_right set_builtin builtins Objmap.empty
end

let ( let* ) res f = Result.bind res f

let object_type_string = function
  | Object.Null -> "NULL"
  | Object.Boolean _ -> "BOOLEAN"
  | Object.Integer _ -> "INTEGER"
  | Object.String _ -> "STRING"
  | Object.Array _ -> "ARRAY"
  | Object.Hash _ -> "HASH"
  | Object.Builtin _ -> "BUILTIN"
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
      let* env = Environment.set env ident value in
      Ok (Object.Null, false, env)

(* Environment is not returned, as the evaluation of an expression should not
   affect the environment in the containing scope. *)
and eval_expression (expr : Ast.expression) (env : Environment.t) :
    (Object.t, error) result =
  match expr with
  | Ast.Integer n -> Ok (Object.Integer n)
  | Ast.Boolean b -> Ok (Object.Boolean b)
  | Ast.String s -> Ok (Object.String s)
  | Ast.Array elems -> eval_array_expression elems env
  | Ast.Hash pairs -> eval_hash_expression pairs env
  | Ast.Prefix (op, right) -> eval_prefix_expression op right env
  | Ast.Infix (left, op, right) -> eval_infix_expression left op right env
  | Ast.Index (array, index) -> eval_index_expression array index env
  | Ast.If (cond, cons, alt) -> eval_if_expression cond cons alt env
  | Ast.Identifier name -> (
      match Environment.get_opt env name with
      | None -> Error (Printf.sprintf "unknown identifier: %s" name)
      | Some value -> Ok value)
  | Ast.Function (params, body) -> Ok (Object.Function (params, body))
  | Ast.Call (func, args) -> eval_call_expression func args env

and eval_array_expression (elems : Ast.expression list) (env : Environment.t) :
    (Object.t, error) result =
  let rec eval_elems elems env =
    match elems with
    | [] -> Ok []
    | first :: rest ->
        let* value = eval_expression first env in
        let* values = eval_elems rest env in
        Ok (value :: values)
  in
  let* values = eval_elems elems env in
  Ok (Object.Array values)

and eval_hash_expression (pairs : (Ast.expression * Ast.expression) list)
    (env : Environment.t) : (Object.t, error) result =
  let rec eval_pairs hash pairs env =
    match pairs with
    | [] -> Ok hash
    | pair :: rest -> (
        let key, data = pair in
        let* key = eval_expression key env in
        let* data = eval_expression data env in
        match key with
        | Object.Integer _ | Object.Boolean _ | Object.String _ ->
            let hash, ok = Object.Hash.add hash key data in
            if ok then
              eval_pairs hash rest env
            else
              Error
                (Printf.sprintf "duplicate hash key: %s" (Object.to_string key))
        | _ ->
            Error
              (Printf.sprintf "invalid hash key type: %s"
                 (object_type_string key)))
  in
  let* hash = eval_pairs (Object.Hash.create ()) pairs env in
  Ok (Object.Hash hash)

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

and eval_index_expression (array : Ast.expression) (index : Ast.expression)
    (env : Environment.t) : (Object.t, error) result =
  let* array = eval_expression array env in
  let* index = eval_expression index env in
  match (array, index) with
  | Object.Array elems, Object.Integer n ->
      if n < 0 || n >= List.length elems then
        Ok Object.Null
      else
        Ok (List.nth elems n)
  | Object.Hash hash, _ -> (
      match index with
      | Object.String _ | Object.Boolean _ | Object.Integer _ -> (
          match Object.Hash.find_opt hash index with
          | None -> Ok Object.Null
          | Some value -> Ok value)
      | _ ->
          Error
            (Printf.sprintf "invalid hash key type: %s"
               (object_type_string index)))
  | _ ->
      Error
        (Printf.sprintf "invalid index expression: %s[%s]"
           (object_type_string array) (object_type_string index))

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
        let* env = Environment.set env param value in
        Ok env
  in

  let wrong_num_args name expected actual =
    Error
      (Printf.sprintf "%s takes %d argument%s; %d provided" name expected
         (if expected == 1 then
            ""
          else
            "s")
         actual)
  in

  let* value = eval_expression func env in
  match value with
  | Object.Builtin builtin -> (
      let wrong_num_args expected =
        wrong_num_args (Object.to_string value) expected (List.length args)
      in
      match builtin with
      | Object.Len -> (
          match args with
          | [ arg ] -> eval_len_expression arg env
          | _ -> wrong_num_args 1)
      | Object.First -> (
          match args with
          | [ arr ] -> eval_first_array_expression arr env
          | _ -> wrong_num_args 1)
      | Object.Last -> (
          match args with
          | [ arr ] -> eval_last_array_expression arr env
          | _ -> wrong_num_args 1)
      | Object.Rest -> (
          match args with
          | [ arr ] -> eval_rest_array_expression arr env
          | _ -> wrong_num_args 1)
      | Object.Push -> (
          match args with
          | [ arr; value ] -> eval_push_array_expression arr value env
          | _ -> wrong_num_args 2)
      | Object.Puts -> (
          match args with
          | [ value ] -> eval_puts_expression value env
          | _ -> wrong_num_args 1))
  | Object.Function (params, body) -> (
      match List.fold_right2 substitute_param params args (Ok env) with
      | exception Stdlib.Invalid_argument _ ->
          wrong_num_args
            (Ast.expression_to_string func)
            (List.length params) (List.length args)
      | Error err -> Error err
      | Ok env ->
          let* obj, _ = eval_statements body env in
          Ok obj)
  | _ ->
      Error
        (Printf.sprintf "%s is not a function" (Ast.expression_to_string func))

and eval_len_expression (arg : Ast.expression) (env : Environment.t) :
    (Object.t, error) result =
  let* value = eval_expression arg env in
  match value with
  | Object.String s -> Ok (Object.Integer (String.length s))
  | Object.Array elems -> Ok (Object.Integer (List.length elems))
  | _ ->
      Error
        (Printf.sprintf "invalid expression: len(%s)" (object_type_string value))

and eval_first_array_expression (arr : Ast.expression) (env : Environment.t) :
    (Object.t, error) result =
  let* value = eval_expression arr env in
  match value with
  | Object.Array elems ->
      Ok
        (if elems == [] then
           Object.Null
         else
           List.hd elems)
  | _ ->
      Error
        (Printf.sprintf "invalid expression: first(%s)"
           (object_type_string value))

and eval_last_array_expression (arr : Ast.expression) (env : Environment.t) :
    (Object.t, error) result =
  let* value = eval_expression arr env in
  match value with
  | Object.Array elems ->
      Ok
        (if elems == [] then
           Object.Null
         else
           List.nth elems (List.length elems - 1))
  | _ ->
      Error
        (Printf.sprintf "invalid expression: last(%s)"
           (object_type_string value))

and eval_rest_array_expression (arr : Ast.expression) (env : Environment.t) :
    (Object.t, error) result =
  let* value = eval_expression arr env in
  match value with
  | Object.Array elems ->
      Ok
        (if elems == [] then
           Object.Null
         else
           Object.Array (List.tl elems))
  | _ ->
      Error
        (Printf.sprintf "invalid expression: rest(%s)"
           (object_type_string value))

and eval_push_array_expression (arr : Ast.expression) (elem : Ast.expression)
    (env : Environment.t) : (Object.t, error) result =
  let* arr = eval_expression arr env in
  let* elem = eval_expression elem env in
  match arr with
  | Object.Array elems -> Ok (Object.Array (elems @ [ elem ]))
  | _ ->
      Error
        (Printf.sprintf "invalid expression: push(%s, %s)"
           (object_type_string arr) (object_type_string elem))

and eval_puts_expression (value : Ast.expression) (env : Environment.t) :
    (Object.t, error) result =
  let* value = eval_expression value env in
  print_endline (Object.to_string value);
  Ok Object.Null
