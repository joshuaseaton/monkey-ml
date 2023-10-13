(* Represents a Monkey lexer.

   An instance is immutable and represents the lexing at a given point during
   the process. *)
type t

(* Creates a lexer from the given source string. *)
val create : string -> t

(* Reads the next token if one remains, returning it alongside a new lexer
   instance that points to the position after that. None is returned when the
   lexer has reached the end of the input source, at which point further calls
   to this function will only continue to yield that. *)
val next_token : t -> t * Token.t option
