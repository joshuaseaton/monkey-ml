(* A built-in Monkey function. *)
type builtin =
  | Len
  | First
  | Last
  | Rest
  | Push
[@@deriving ord, sexp_of]

(* An evaluated Monkey object. *)
type t =
  | Null
  | Integer of int
  | Boolean of bool
  | String of string
  | Array of t list
  | Builtin of builtin
  | Function of (string list * Ast.statement list)
[@@deriving ord, sexp_of]

val to_string : t -> string
