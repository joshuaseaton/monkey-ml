(* A parse-time error. *)
type error = string

(* Represents a parser of the Monkey language. *)
type t

(* Creates a parser given a lexer. *)
val create : Lexer.t -> t

(* Parses the associated Monkey source, returning an AST node representing the
   entire program or an error. *)
val parse : t -> (Ast.node, error) result
