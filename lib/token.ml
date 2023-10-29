open Ppx_compare_lib.Builtin
open Sexplib.Std

(* Represents the tokens that comprise Monkey source. *)
type t =
  | Illegal of char
  | Unterminated_string of string
  (* Identifiers + literals *)
  | Identifier of string
  | Integer of string
  | Boolean of bool
  | String of string
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
  | Colon
  | Left_paren
  | Right_paren
  | Left_brace
  | Right_brace
  | Left_bracket
  | Right_bracket
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
  | Unterminated_string s -> "\"" ^ s
  | Identifier s -> s
  | Integer s -> s
  | Boolean b -> string_of_bool b
  | String s -> "\"" ^ s ^ "\""
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
  | Colon -> ":"
  | Left_paren -> "("
  | Right_paren -> ")"
  | Left_brace -> "{"
  | Right_brace -> "}"
  | Left_bracket -> "["
  | Right_bracket -> "]"
  | Function -> "fn"
  | Let -> "let"
  | If -> "if"
  | Else -> "else"
  | Return -> "return"
