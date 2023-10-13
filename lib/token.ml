open Ppx_compare_lib.Builtin
open Sexplib.Std

(* Represents the tokens that comprise Monkey source. *)
type t =
  | Illegal of char
  (* Identifiers + literals *)
  | Identifier of string
  | Integer of string
  | Boolean of bool
  (* Operators *)
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | Equal
  | Not_equal
  | Less_than
  | Greater_than
  (* Delimiters *)
  | Comma
  | Semicolon
  | Left_paren
  | Right_paren
  | Left_brace
  | Right_brace
  (* Keywords *)
  | Function
  | Let
  | If
  | Else
  | Return
[@@deriving compare, sexp_of]

let to_string (token : t) : string =
  match token with
  | Illegal ch -> String.make 1 ch
  | Identifier s -> s
  | Integer s -> s
  | Boolean b -> string_of_bool b
  | Assign -> "="
  | Plus -> "+"
  | Minus -> "-"
  | Bang -> "!"
  | Asterisk -> "*"
  | Slash -> "/"
  | Equal -> "=="
  | Not_equal -> "!="
  | Less_than -> "<"
  | Greater_than -> ">"
  | Comma -> ","
  | Semicolon -> ";"
  | Left_paren -> "("
  | Right_paren -> ")"
  | Left_brace -> "{"
  | Right_brace -> "}"
  | Function -> "fn"
  | Let -> "let"
  | If -> "if"
  | Else -> "else"
  | Return -> "return"
