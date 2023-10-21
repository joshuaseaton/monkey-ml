open Sexplib.Std

(* A built-in Monkey function. *)
type builtin = Len [@@deriving ord, sexp_of]

(* An evaluated Monkey object. *)
type t =
  | Null
  | Integer of int
  | Boolean of bool
  | String of string
  | Builtin of builtin
  | Function of (string list * Ast.statement list)
[@@deriving ord, sexp_of]

let rec to_string = function
  | Null -> "NULL"
  | Integer n -> string_of_int n
  | Boolean b -> string_of_bool b
  | String s -> Printf.sprintf "%S" s
  | Builtin builtin -> builtin_to_string builtin ^ "()"
  | Function (params, body) ->
      Ast.expression_to_string (Ast.Function (params, body))

and builtin_to_string = function Len -> "len"
