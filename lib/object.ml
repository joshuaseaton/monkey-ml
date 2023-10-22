open Sexplib.Std

type builtin =
  | Len
  | First
  | Last
  | Rest
  | Push
[@@deriving ord, sexp_of]

type t =
  | Null
  | Integer of int
  | Boolean of bool
  | String of string
  | Array of t list
  | Builtin of builtin
  | Function of (string list * Ast.statement list)
[@@deriving ord, sexp_of]

let rec to_string = function
  | Null -> "NULL"
  | Integer n -> string_of_int n
  | Boolean b -> string_of_bool b
  | String s -> Printf.sprintf "%S" s
  | Array elems -> Printf.sprintf "[%s]" (concat elems ", ")
  | Builtin builtin -> (
      match builtin with
      | Len -> "len()"
      | First -> "first()"
      | Last -> "last()"
      | Rest -> "rest()"
      | Push -> "push()")
  | Function (params, body) ->
      Ast.expression_to_string (Ast.Function (params, body))

and concat (objs : t list) (sep : string) : string =
  match objs with
  | [] -> ""
  | [ obj ] -> to_string obj
  | first :: rest -> to_string first ^ sep ^ concat rest sep
