open Sexplib.Std

(* An evaluated Monkey object. *)
type t =
  | Null
  | Integer of int
  | Boolean of bool
  | Function of (string list * Ast.statement list)
[@@deriving ord, sexp_of]

let to_string = function
  | Null -> "NULL"
  | Integer n -> string_of_int n
  | Boolean b -> string_of_bool b
  | Function (params, body) ->
      Ast.expression_to_string (Ast.Function (params, body))
