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
  | Prefix of Token.t * expression
  | Infix of expression * Token.t * expression
  | If of expression * statement list * statement list option
  | Function of string list * statement list
  | Call of expression * expression list
[@@deriving compare, sexp_of]

and statement =
  | Expression_statement of expression
  | Let of string * expression
  | Return of expression
[@@deriving compare, sexp_of]

val node_to_string : node -> string
val statement_to_string : statement -> string
val expression_to_string : expression -> string
