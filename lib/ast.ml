open Ppx_compare_lib.Builtin
open Sexplib.Std

type node =
  | Program of statement list
  | Expression of expression
  | Statement of statement
[@@deriving compare, sexp_of]

and expression =
  | Identifier of string
  | Boolean of bool
  | Integer of int
  | String of string
  | Array of expression list
  | Prefix of Token.t * expression
  | Infix of expression * Token.t * expression
  | Index of expression * expression
  | If of expression * statement list * statement list option
  | Function of string list * statement list
  | Call of expression * expression list
[@@deriving compare, sexp_of]

and statement =
  | Expression_statement of expression
  | Let of string * expression
  | Return of expression
[@@deriving compare, sexp_of]

let rec concat (l : 'a list) (to_string : 'a -> string) (delim : string) :
    string =
  match l with
  | [] -> ""
  | [ el ] -> to_string el
  | first :: rest -> to_string first ^ delim ^ concat rest to_string delim

let rec node_to_string = function
  | Program stmts -> concat stmts statement_to_string "\n"
  | Statement stmt -> statement_to_string stmt
  | Expression expr -> expression_to_string expr

and statement_to_string = function
  | Expression_statement expr -> expression_to_string expr
  | Let (ident, expr) ->
      Printf.sprintf "let %s = %s;" ident (expression_to_string expr)
  | Return expr -> Printf.sprintf "return %s;" (expression_to_string expr)

and expression_to_string = function
  | Identifier s -> s
  | Boolean b -> string_of_bool b
  | Integer n -> string_of_int n
  | String s -> Printf.sprintf "%S" s
  | Array arr -> "[" ^ concat arr expression_to_string ", " ^ "]"
  | Prefix (op, right) ->
      Printf.sprintf "(%s%s)" (Token.to_string op) (expression_to_string right)
  | Infix (left, op, right) ->
      Printf.sprintf "(%s %s %s)"
        (expression_to_string left)
        (Token.to_string op)
        (expression_to_string right)
  | Index (array, index) ->
      Printf.sprintf "%s[%s]"
        (expression_to_string array)
        (expression_to_string index)
  | If (cond, cons, alt) -> (
      match alt with
      | None ->
          Printf.sprintf "if (%s) { %s }"
            (expression_to_string cond)
            (block_to_string cons)
      | Some alt ->
          Printf.sprintf "if (%s) { %s } else { %s }"
            (expression_to_string cond)
            (block_to_string cons) (block_to_string alt))
  | Function (params, body) ->
      Printf.sprintf "fn(%s) { %s }"
        (concat params (fun x -> x) ", ")
        (block_to_string body)
  | Call (func, args) ->
      Printf.sprintf "%s(%s)"
        (expression_to_string func)
        (concat args expression_to_string ", ")

and block_to_string (block : statement list) : string =
  concat block statement_to_string " "
