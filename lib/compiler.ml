open Ppx_compare_lib.Builtin
open Sexplib.Std

let ( let* ) = Result.bind

type error =
  | Invalid_infix_operator of Token.t
  | Invalid_prefix_operator of Token.t
  | Unimplemented
[@@deriving compare, sexp_of]

let error_to_string = function
  | Invalid_infix_operator tok ->
      Printf.sprintf "Invalid infix operator: %s" (Token.to_string tok)
  | Invalid_prefix_operator tok ->
      Printf.sprintf "Invalid prefix operator: %s" (Token.to_string tok)
  | Unimplemented -> "Implement me!"

type bytecode = {
  instructions : Code.Instructions.t;
  constants : Object.t list;
}
[@@deriving compare, sexp_of]

let add_instruction (code : bytecode) (insn : Code.t) : bytecode =
  { code with instructions = Code.Instructions.append code.instructions insn }

let add_constant (code : bytecode) (const : Object.t) : bytecode * int =
  let pos = List.length code.constants in
  ({ code with constants = code.constants @ [ const ] }, pos)

let rec compile (node : Ast.node) : (bytecode, error) result =
  let code = { instructions = Bytes.empty; constants = [] } in
  match node with
  | Ast.Program stmts ->
      let rec compile_statements code stmts =
        match stmts with
        | [] -> Ok code
        | first :: rest ->
            let* code = compile_statement code first in
            compile_statements code rest
      in
      compile_statements code stmts
  | Ast.Expression expr -> compile_expression code expr
  | Ast.Statement stmt -> compile_statement code stmt

and compile_statement (code : bytecode) (stmt : Ast.statement) :
    (bytecode, error) result =
  match stmt with
  | Ast.Expression_statement expr ->
      let* code = compile_expression code expr in
      Ok (add_instruction code Code.Pop)
  | _ -> Error Unimplemented

and compile_expression (code : bytecode) (expr : Ast.expression) :
    (bytecode, error) result =
  match expr with
  | Ast.Integer n ->
      let code, pos = add_constant code (Object.Integer n) in
      Ok (add_instruction code (Code.Constant pos))
  | Ast.Boolean b ->
      Ok
        (add_instruction code
           (if b then
              Code.True
            else
              Code.False))
  | Ast.Infix (left, tok, right) ->
      let* code = compile_expression code left in
      let* code = compile_expression code right in
      let* op =
        match tok with
        | Token.Plus -> Ok Code.Add
        | Token.Minus -> Ok Code.Sub
        | Token.Asterisk -> Ok Code.Mul
        | Token.Slash -> Ok Code.Div
        | Token.Equal -> Ok Code.Equal
        | Token.Not_equal -> Ok Code.Not_equal
        | Token.Less_than -> Ok Code.Less_than
        | Token.Greater_than -> Ok Code.Greater_than
        | _ -> Error (Invalid_infix_operator tok)
      in
      Ok (add_instruction code op)
  | Ast.Prefix (tok, right) ->
      let* code = compile_expression code right in
      let* op =
        match tok with
        | Token.Minus -> Ok Code.Minus
        | Token.Bang -> Ok Code.Bang
        | _ -> Error (Invalid_prefix_operator tok)
      in
      Ok (add_instruction code op)
  | _ -> Error Unimplemented
