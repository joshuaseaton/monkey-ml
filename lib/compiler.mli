type bytecode = {
  instructions : Code.Instructions.t;
  constants : Object.t list;
}
[@@deriving compare, sexp_of]
(** Monkey bytecode, along with related, compiled artifacts. *)

type error =
  | Invalid_infix_operator of Token.t
  | Invalid_prefix_operator of Token.t
  | Unimplemented  (** TODO: Remove me once done. *)
[@@deriving compare, sexp_of]

val error_to_string : error -> string

val compile : Ast.node -> (bytecode, error) result
(** Compiles the given AST node into bytecode. *)
