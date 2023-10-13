(* Environment represents evaluation context, providing a means for AST
   evaluation to track Monkey objects by identifier. *)
module Environment : sig
  type t

  (* Creates an empty environment. *)
  val create : t
end

(* Represents an error that occurs during. *)
type error = string

(* Evaluates a Monkey AST node. An environment for the containing scope is
   provided, and an updated instance additionally capturing the identifiers
   defined within the node is returned alongside the evaluated object. *)
val eval : Ast.node -> Environment.t -> (Object.t * Environment.t, error) result
